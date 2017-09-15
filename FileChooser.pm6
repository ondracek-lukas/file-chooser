#!/usr/bin/perl6
# Copyright (C) 2017  Lukáš Ondráček <ondracek.lukas@gmail.com>, to be used under GNU GPLv3

use v6;

sub USAGE is export { MAIN :h; exit 2 }
multi MAIN(Bool:D :$h) is export { $*ERR.print: Q:to/END-OF-USAGE/ }

	The application File Chooser provides AutoComplete functionality
	for file paths in terminal using asynchronous IO;
	it interacts via STDIN and STDERR and prints paths to STDOUT.
	
	You can spawn this script directly,
	but it is recommended to use 'cf' (choose file) script instead,
	not to recompile the whole code at each startup (both has same arguments):
	  cf -h
	  cf [-d] [-m] [-q] [-v] [INITIAL_PATH]
	
	Options:
	  -h  ...print this help and exit;
	  -d  ...restrict suggestions to directories;
	  -m  ...allow multiple files to be selected (use Enter twice to quit);
	  -q  ...quote output paths ( /bob's/file  -->  '/bob'\''s/file' );
	  -v  ...print debugging information to STDIN
	         (use `cf > fifo` not to break interactive interface).
	
	Examples of intended use:
	  [1] cd "`cf -d`"
	  [2] eval vim -p `cf -m -q`
	  [3] cf -m ~/myfiles/ | while read file; do process "$file"; done
	  [4] eval declare -a files=(`cf -m -q`); cp "${files[@]}" .
	  [5] files=`cf -m -q` && eval mv $files .
	
	The interface consists of an input line with a proposed completion
	and a ls-like list of suitable files (suggestions).
	The completion consists of an absolute path to the current directory before the input string,
	if it doesn't start with '/' or '~/',
	and the longest completion common to all suggested files after the input string.
	
	Key binding:
	  Left, Right           ...move cursor by one character;
	  C-a/Home, C-e/End     ...move cursor to begin/end of line;
	  Backspace, Delete     ...remove previous/next character;
	  C-w                   ...remove last path segment (uses '/' as delimiter, not spaces);
	  C-u¹                  ...remove all before cursor;
	  Tab                   ...accept proposed completion and continue editing
	                           (blocks iff suggestions are not created yet);
	  Esc/C-c               ...send SIGINT to itself causing shell to abort the whole command;
	  Enter                 ...accept path (print it to STDOUT) and possibly quit;
	  C-d, C-u¹             ...scroll suggestions by half a screen;
	  C-f/PgDown, C-b/PgUp  ...scroll suggestions by whole screens.
	¹C-u meaning depends on whether the last action was line-editing or suggestion-scrolling.
	
	You can start writing path even before full interpreter and application initialization.
	
	The application handles dot-dot directories logically (like cd does, unlike cp, mv, ...);
	not to behave confusingly, returned paths never contains /../ segments.
	
	
	Copyright (C) 2017  Lukáš Ondráček <ondracek.lukas@gmail.com>, to be used under GNU GPLv3.
	                    https://github.com/ondracek-lukas/file-chooser
	
	END-OF-USAGE


my Bool:D $verbose = False;
sub log(|c) {
	say(|c) if $verbose;
}


# --- INPUT LINE BUFFERS ---

class StrBuf {
	has Str $.Str = "";
	has Buf $!buf .= new;
	method !tryUpdate {
		try $!Str = $!buf.decode;
	}
	method push (*@_) {
		push $!buf, |@_;
		self!tryUpdate;
	}
	method unshift (*@_) {
		unshift $!buf, |@_;
		self!tryUpdate;
	}
	method popChar() {
		my $c = $!Str.chars;
		return () if $c == 0;
		my @ret;
		repeat {
			unshift @ret, pop $!buf;
			self!tryUpdate;
		} while $!Str.chars >= $c;
		return @ret;
	}
	method shiftChar() {
		my $c = $!Str.chars;
		return () if $c == 0;
		my @ret;
		repeat {
			push @ret, shift $!buf;
			self!tryUpdate;
		} while $!Str.chars >= $c;
		return @ret;
	}
	method shiftAll() {
		my @ret;
		push @ret, shift $!buf while $!buf.elems>0;
		$!Str="";
		return @ret;
	}
	method pushStr($_) {
		self.push: $_.encode;
		self!tryUpdate;
	}
	method unshiftStr($_) {
		self.unshift: $_.encode;
		self!tryUpdate;
	}
}


# --- BINARY SEARCH ---

sub binarySearch(&f, @arr, Int :$l is copy =0, Int :$r is copy =@arr-1) {
	my $m = ($l + $r) / 2;
	return $m if $r < $l;
	$m .= Int;
	given f(@arr[$m]) {
		when Less { $l = $m + 1 }
		when More { $r = $m - 1 }
		when Same { return $m }
	}
	binarySearch &f, @arr, :$l, :$r;
}

sub bsgrep(&f, @arr) {  # f should return Less/More if its argument is too low/high; Same if wanted
	@arr[binarySearch({f($_) <=> -0.5}, @arr).ceiling .. binarySearch({f($_) <=> 0.5}, @arr).floor]
}


# --- ESCAPE SEQUENCES ---

my Regex $escSeq = /\e\[.*?<[a..zA..Z~]>/;
my Regex $escNewSize = /^"\e[8;"(\d+)\;(\d+)t/;

my %escSeqs;
%escSeqs< getsize erasedown eraseright up down right left linebegin home end pageup pagedown delete color > =
        < 18t     J         K          A  B    C     D    G         H    F   5~     6~       3~     m     >;

sub esc(*@_) {
	my (@args, $seq);
	my $str = "";
	for @_ {
		if my $nextSeq = %escSeqs{$_} {
			$str ~= "\e[" ~ @args.join(";") ~ $seq if $seq;
			($seq, @args) = $nextSeq;
		} else {
			push @args, $_;
		}
	}
	$str ~= "\e[" ~ @args.join(";") ~ $seq if $seq;
	return $str;
}

sub escResetCursorPos($relRow=0, $absCol=0) {
	my $str = esc "linebegin";
	$str   ~= esc "up",   -$relRow if $relRow < 0;
	$str   ~= esc "down",  $relRow if $relRow > 0;
	$str   ~= esc "right", $absCol if $absCol > 0;
	return $str;
}

sub escColored($color, $str) {
	return esc(«color $color») ~ $str ~ esc(<color 0>) if $str;
	return "";
}

my $colorSugg = "33";


my $sttyOrig;
sub initInteraction {
	$sttyOrig = chomp qqx (stty -g);
	run <stty -icanon -echo -isig>;
	$*ERR.print: esc "getsize";
}
sub finInteraction {
	run "stty", $sttyOrig if $sttyOrig;
}


# --- PRINTING ---

my ($width, $height)    = 0,0;  # window size in characters
my ($curRow, $curCol)   = 0,0;  # cursor coordinates (relative to origin)
my StrBuf $inputL      .= new;  # input before cursor
my StrBuf $inputR      .= new;  # input after  cursor
my Str    $complBefore  = "";   # completion before input
my Str    $complAfter   = "";   # completion  after input
my        $sugg         = Nil;  # suggestions below input
my Int    $suggShown    = Nil;  # number of suggestions shown
my Int    $suggFirst    = Nil;  # first suggestion shown
my Int    $suggLast     = Nil;  # last  suggestion shown

sub calcStrSize($str) {  # --> rows to be appended (except current one), characters at last line
	$_ = $str;
	s:g/$escSeq//;
	my ($rows, $cols) = -1,0;
	for .split(/\n/) {
		$rows += my $curRows = (.chars + $width - 1) div $width || 1;
		$cols  = .chars - $width * ($curRows - 1);
	}
	return $rows, $cols;
}

sub printLine($str?) {
	# clear interactive output (incl. input line), print, assume input line under it
	$*ERR.print: escResetCursorPos(-$curRow)
	           ~ esc(<eraseright erasedown>)
	           ~ ($str ?? $str ~ "\n" !! "");
	($curRow, $curCol) = 0, 0;
}

sub updateScreen {  # reprint all interactive output (incl. input line)
	return unless $width and $height;

	my $str = escResetCursorPos(-$curRow)
	        ~ esc(<eraseright erasedown>)
	        ~ escColored($colorSugg, $complBefore)
	        ~ $inputL
	        ~ ($inputR ~ escColored($colorSugg, $complAfter) || " ");
	($curRow, $curCol) = calcStrSize($complBefore ~ $inputL ~ " "); $curCol--;

	my $maxRowsCnt = $height - calcStrSize($str)[0] - 5;  # keep at least 3 lines of context above input

	my Str $suggStr;
	if $maxRowsCnt >= 3 and $width > 10 {
		if $sugg ~~ List and $sugg.elems > 0 {
			my $succColumns;
			OUTER-LOOP: loop {  # repeat only if reversed strategy was chosen and it failed
				my $rowsCnt = $maxRowsCnt;
				my $reverse = False;
				without $suggFirst {
					with $suggLast {
						$reverse = True;  # fill the screen from the last suggestion, which should be shown
						($suggFirst, $suggLast) = $sugg.elems - 1 - $suggLast;
					} else {
						($suggFirst, $suggLast) = 0;
					}
				}

				my @suggTail = |$sugg;
				@suggTail .= reverse if $reverse;
				@suggTail.splice(0, $suggFirst);
				@suggTail.unshift: "..." if $suggFirst;
				$succColumns = Nil;

				INNER-LOOP: loop {  # fill the screen greedy, lower number of rows if needed
					my @remain = @suggTail;
					my @columns;
					my $curWidth = 0;
					my $curSuggShown = -?$suggFirst;

					while @remain {
						my @column = @remain.splice: 0, $rowsCnt;
						my $newWidth = $curWidth + 2*?@columns + [max] 3, |map *.chars, @column;
						if @columns and $newWidth > $width {
							@remain = True;  # undoing last splice, @remain is nonempty, no matter what is inside
							last;
						}
						push @columns, [@column];
						$curWidth = $newWidth;
						$curSuggShown += @column;
					}
					if $curWidth > $width {
						@columns = [map { .chars > $width ?? $_.substr(0, $width-1) ~ "…" !! $_ }, |@columns[0]],;
					}
					if @remain {
						without $succColumns {
							@columns[*-1][*-1] = "...";
							$curSuggShown--;
							$succColumns = [@columns];
							$suggShown = $curSuggShown;
						}
						last INNER-LOOP;
					} else {
						if $suggFirst or $reverse {
							# last/first element is shown but the other is not (or there may be a gap at the top of screen),
							# show as much as possible »trying to keep already shown« (XXX this doesn't work always)
							if $reverse {
								($suggFirst, $suggLast) = 0;
							} else {
								($suggLast, $suggFirst) = $sugg.elems - 1;
							}
							redo OUTER-LOOP;
						}
						$succColumns = [@columns];
						$suggShown = $curSuggShown;
						my $oldRowsCnt = $rowsCnt;
						my $columnsCnt = @columns.elems;
						while $rowsCnt > 1 and $rowsCnt >= $oldRowsCnt {
							$rowsCnt = ceiling($sugg.elems / ($columnsCnt++));
						}
						last INNER-LOOP if $rowsCnt == $oldRowsCnt;
					}
				}  # end of INNER-LOOP

				$suggLast = $suggFirst + $suggShown - 1;

				if $reverse {
					$succColumns = [map {[$_.reverse]}, $succColumns.reverse];
					($suggFirst, $suggLast) = $sugg.elems - 1 - $suggLast, $sugg.elems - 1 - $suggFirst;
				}
				last OUTER-LOOP;
			}  # end of OUTER-LOOP

			if $succColumns.elems > 1 {
				$succColumns[*-1][*-0..$maxRowsCnt-1]=""xx*;
				my $fmtStr = join "  ", map { "%-" ~ ([max] 3, |map *.chars, |$_) ~ "s" }, flat $succColumns;
				$suggStr = join "\n", map { sprintf $fmtStr, |$_ }, [Z] flat $succColumns;
			} else {
				$suggStr = join "\n", |$succColumns[0];
			}
		} else {
			try $suggStr = $sugg // "...";
		}
	}


	$str ~= "\n\n" ~ $suggStr with $suggStr;

	$str ~= escResetCursorPos($curRow - calcStrSize($str)[0], $curCol);

	$*ERR.print: $str;
}


# --- FILE SYSTEM ---

my Channel $fsChannel .= new;

class TryLaterException is Exception {};

sub compareFileNames(Str $a, Str $b) {
	# assert: names with the same prefix are consecutive
	# assert: hidden files are all at the beginning or all at the end
	# (binary search would not work otherwise)
	($b.starts-with(".") <=> $a.starts-with(".")) || (lc($a) cmp lc($b));
}
sub defaultBsFilter($_) {
	# filters file names if no prefix is given in current path segment
	# assert: acceptable names are consecutive in list ordered by previous function
	(~$_).starts-with('.') ?? Less !! Same;
}

class dirCache {
	has $!files = Nil;
	has $.path = "/";
	has $.name = "";
	has $.d = False;
	method new($dir='/'.IO, Bool:D :$d=False) {
		self.bless(path=>$dir.path, name=>$dir.basename, :$d);
	}
	method Str() {
		return $.name ~ "/";
	}
	method files(Bool:D :$sync=False) {
		without $!files {
			$!files = start {
				my %ret;
				dir($!path, test=>True)
					==> classify {$_.d ?? "dirs" !! "others"} \
					==> my %allFiles;
				|%allFiles<dirs> // ()
					==> map {$_.basename => dirCache.new($_, :$!d)} \
					==> hash()
					==> %ret<dirs>;
				|%allFiles<dirs> // ()
					==> map *.basename ~ "/"
					==> sort &compareFileNames
					==> %ret<namesDirs>;
				|(%allFiles<others> // () unless $!d)
					==> map *.basename
					==> sort &compareFileNames
					==> %ret<namesOthers>;
				%ret;
			}
			$!files.then: { $fsChannel.send(True) }
		}
		if $!files ~~ Promise {
			if $sync or $!files.status ~~ Kept {
				$!files = await $!files;
			} else {
				die TryLaterException.new;
			}
		}
		return $!files;
	}

	method getChild($name, Bool:D :$sync=False) {
		return $.files(:$sync)<dirs>{$name};
	}

	method getSuggestions($completion is rw, *@_, Bool:D :$sync=False) {
		my $pattern = shift @_;
		if @_ {
			with my $child = $.getChild($pattern, :$sync) {
				return $child.getSuggestions($completion, @_, :$sync);
			} else {
				return ();
			}
		} else {
			my $bsFilter = &defaultBsFilter;
			my $patLen = $pattern.chars;
			$bsFilter = {compareFileNames $_.substr(0, $patLen), $pattern} if $pattern;
			my @dirs   = |bsgrep($bsFilter, $.files(:$sync)<namesDirs>);
			my @others = |bsgrep($bsFilter, $.files(:$sync)<namesOthers>);

			for |(@dirs[0, *-1] if @dirs), |(@others[0, *-1] if @others) -> $name {
				with $completion {
					my $i=0;
					$i++ while try $_.substr($i,1) eq $name.substr($i+$patLen,1);
					$_ = $name.substr($patLen, $i);
				} else {
					$_ = $name.substr($patLen);
				}
			}
			return [|@dirs, |@others];
		}
	}
}

my dirCache $cache;
my Bool $suggUpdatedSincePathAccept = False;

sub getCurrentPath() {
	$_ = $complBefore ~ $inputL ~ $inputR;
	s{^"~/"} = %*ENV<HOME> ~ "/";
	while s{"//"}             = "/" {}
	while s{"/./"}            = "/" {}
	while s{"/"<-[/]>*"/../"} = "/" {}
	while s{^"/../"}          = "/" {}
	$_;
}

sub updateSuggestions(Bool:D :$sync=False) {
	$suggUpdatedSincePathAccept = True;
	($suggFirst, $suggLast, $suggShown) = ();
	$complBefore = "";
	$complBefore = %*ENV<PWD> ~ "/" unless $inputL ~ $inputR ~~ m{^\~?\/};
	$complAfter  = "";
	my @input = split("/", getCurrentPath)[1..*];

	my Str $completion;
	$sugg = $cache.getSuggestions: $completion, @input, :$sync;
	$complAfter = $_ with $completion;
	CATCH {
		when TryLaterException { $sugg = Nil; }
	}
}

sub acceptPath($path is copy, Bool:D :$q) {
	$suggUpdatedSincePathAccept = False;
	printLine $path;
	if $q {
		$path ~~ s:g/\'/'\\''/;
		$path = "'$path'";
	}
	say $path;
}


# --- USER COMMANDS ---

my $suggScrollMode = False;

sub cmdCursorLeft {
	$suggScrollMode = False;
	$inputR.unshift: $inputL.popChar;
	updateScreen;
}
sub cmdCursorRight {
	$suggScrollMode = False;
	$inputL.push: $inputR.shiftChar;
	updateScreen;
}
sub cmdCursorBegin {
	$suggScrollMode = False;
	$inputR.unshift: $inputL.shiftAll;
	updateScreen;
}
sub cmdCursorEnd {
	$suggScrollMode = False;
	$inputL.push: $inputR.shiftAll;
	updateScreen;
}
sub cmdRmLastChar {
	$suggScrollMode = False;
	$inputL.unshiftStr($complBefore) unless ~$inputL;
	$inputL.popChar;
	updateSuggestions;
	updateScreen;
}
sub cmdRmNextChar {
	$suggScrollMode = False;
	$inputR.shiftChar;
	updateSuggestions;
	updateScreen;
}
sub cmdRmLastPathSegment {
	$suggScrollMode = False;
	$inputL.unshiftStr($complBefore) unless ~$inputL;
	if ~$inputL {
		$inputL.popChar;
		$inputL.popChar while ~$inputL and (~$inputL).substr(*-1) ne "/";
		updateSuggestions;
		updateScreen;
	}
}
sub cmdRmAllBeforeCursor {
	$suggScrollMode = False;
	$inputL.shiftAll;
	updateSuggestions;
	updateScreen;
}
sub cmdComplete {
	$suggScrollMode = False;
	updateSuggestions :sync;
	$inputL.push: |$inputR.shiftAll;
	$inputL.unshiftStr($complBefore);
	$inputL.pushStr($complAfter);
	$complAfter="";
	updateSuggestions;
	updateScreen;
}
sub cmdAcceptQuit(:$q) {
	$suggScrollMode = False;
	acceptPath getCurrentPath, :$q;
	finInteraction;
	exit;
}
sub cmdAcceptContinue(:$q) {
	$suggScrollMode = False;
	acceptPath getCurrentPath, :$q;
	updateScreen;
}
sub cmdQuitCancel {
	$suggScrollMode = False;
	printLine;
	finInteraction;
	run «kill -SIGINT $*PID»;
}
sub cmdQuitSucc {
	$suggScrollMode = False;
	printLine;
	finInteraction;
	exit;
}

sub cmdSuggDownScreen {
	$suggScrollMode = True;
	if $sugg ~~ List and $suggFirst.defined and $suggLast.defined {
		($suggFirst, $suggLast) = min $suggLast + 1, $sugg.elems - 1;
		updateScreen;
	}
}
sub cmdSuggUpScreen {
	$suggScrollMode = True;
	if $sugg ~~ List and $suggFirst.defined and $suggLast.defined {
		($suggLast, $suggFirst)  = max $suggFirst - 1, 0;
		updateScreen;
	}
}
sub cmdSuggDownHalf {
	$suggScrollMode = True;
	if $sugg ~~ List and $suggFirst.defined and $suggLast.defined {
		($suggFirst, $suggLast) = min $suggFirst + ($suggShown div 2), $sugg.elems-1;
		updateScreen;
	}
	updateScreen;
}
sub cmdSuggUpHalf {
	$suggScrollMode = True;
	if $sugg ~~ List and $suggFirst.defined and $suggLast.defined {
		($suggLast, $suggFirst)  = max $suggLast - ($suggShown div 2), 0;
		updateScreen;
	}
	updateScreen;
}



# --- MAIN LOOP ---

multi MAIN(Str:D $initPath="", Bool:D :$d=False, Bool:D :$q=False, Bool:D :$m=False, Bool:D :$v=False) is export {

	$cache .= new :$d;
	$verbose = $v;
	$inputL.pushStr: $initPath;

	my Channel $inputChannel .= new;
	start {
		while $_=$*IN.read(1) {
			$inputChannel.send: $_[0];
		}
		$inputChannel.done;
	}

	initInteraction;

	updateSuggestions;
	react {
		whenever $inputChannel {
			state $escRead = "";
			if not $escRead {
				$inputL.push: $_;
				given try (~$inputL).substr(*-1) {
					when "\e" {
						$inputL.popChar;
						$escRead = $_;
					}
					when "\b" {
						$inputL.popChar;
						cmdRmLastChar;
					}
					when "\t" {
						$inputL.popChar;
						cmdComplete;
					}
					when "\n" {
						$inputL.popChar;
						if $m {
							if $suggUpdatedSincePathAccept {
								cmdAcceptContinue :$q;
							} else {
								cmdQuitSucc;
							}
						} else {
							cmdAcceptQuit :$q;
						}
					}
					when "" {
						$inputL.popChar;
						cmdCursorBegin;
					}
					when "" {
						$inputL.popChar;
						cmdCursorEnd;
					}
					when "" {
						$inputL.popChar;
						if $suggScrollMode {
							cmdSuggUpHalf;
						} else {
							cmdRmAllBeforeCursor;
						}
					}
					when "" {
						$inputL.popChar;
						cmdSuggDownScreen;
					}
					when "" {
						$inputL.popChar;
						cmdSuggUpScreen;
					}
					when "" {
						$inputL.popChar;
						cmdSuggDownHalf;
					}
					when "" {
						$inputL.popChar;
						cmdRmLastPathSegment;
					}
					when "" {
						$inputL.popChar;
						cmdQuitCancel;
					}
					when chr any 0..^32 {
						$inputL.popChar;
						log "Unknown control character ^" ~ chr 64 + ord $_;
					}
					default {
						updateSuggestions;
						updateScreen;
					}
				}
			} else {  # ?$escRead
				my $nextEscRead = "";
				if .chr eq "\e" {
					$nextEscRead = .chr;
				} else {
					$escRead ~= .chr;
				}
				if $nextEscRead or .chr ~~ /<[a..zA..Z~]>/ {
					given ($escRead) {
						when $escNewSize {
							unless $height == $0 and $width == $1 {
								($height, $width) = +$0, +$1;
								updateScreen;
							}
						}
						when esc "left" {
							cmdCursorLeft;
						}
						when esc "right" {
							cmdCursorRight;
						}
						when esc "home" {
							cmdCursorBegin;
						}
						when esc "end" {
							cmdCursorEnd;
						}
						when esc "pageup" {
							cmdSuggUpScreen;
						}
						when esc "pagedown" {
							cmdSuggDownScreen;
						}
						when esc "delete" {
							cmdRmNextChar;
						}
						when "\e" {
							cmdQuitCancel;
						}
						default {
							s:g/\e/\\e/;
							log "Unknown control sequence $_";
						}
					}
					$escRead = $nextEscRead;
				}
			}
		}
		whenever $fsChannel {
			without $sugg {
				updateSuggestions;
				updateScreen;
			}
		}
		whenever Supply.interval(0.2) {
			$*ERR.print: esc "getsize";
		}
	}

	finInteraction;
}
