[UNDEFINED] assert [IF]

MARKER rm_assert

\ Name space prefixes used:
\ ts_ tg_ tc_ tw_ tv_	test suite group case word value


VARIABLE tests_passed
VARIABLE tests_failed
VARIABLE tests_skipped

: ansi_normal S\" \e[0m" TYPE ;
: ansi_bold S\" \e[1m" TYPE ;
: ansi_dim S\" \e[2m" TYPE ;
: ansi_standout S\" \e[3m" TYPE ;
: ansi_underline S\" \e[4m" TYPE ;
: ansi_blink S\" \e[5m" TYPE ;
: ansi_reverse S\" \e[6m" TYPE ;
: ansi_hide S\" \e[7m" TYPE ;

: ansi_black S\" \e[30m" TYPE ;
: ansi_red S\" \e[31m" TYPE ;
: ansi_green S\" \e[32m" TYPE ;
: ansi_yellow S\" \e[33m" TYPE ;
: ansi_blue S\" \e[34m" TYPE ;
: ansi_magenta S\" \e[35m" TYPE ;
: ansi_cyan S\" \e[36m" TYPE ;
: ansi_white S\" \e[37m" TYPE ;

: ansi_bg_black S\" \e[40m" TYPE ;
: ansi_bg_red S\" \e[41m" TYPE ;
: ansi_bg_green S\" \e[42m" TYPE ;
: ansi_bg_yellow S\" \e[43m" TYPE ;
: ansi_bg_blue S\" \e[44m" TYPE ;
: ansi_bg_magenta S\" \e[45m" TYPE ;
: ansi_bg_cyan S\" \e[46m" TYPE ;
: ansi_bg_white S\" \e[47m" TYPE ;

: test_pass 1 tests_passed +! ansi_green ." ." ansi_normal ;
: test_fail 1 tests_failed +! ansi_red ." F" ansi_normal ;
: test_skip 1 tests_skipped +! ansi_magenta ." S" ansi_normal ;

DEFER test_skip_fail
' test_fail IS test_skip_fail

\ ( bool -- )
: assert IF test_pass ELSE test_skip_fail THEN ;

\ ( bool -- )
: assert_not 0= assert ;

\ ( bool -- )
: assert_skip IF test_pass ELSE test_skip THEN ;

\ ( bool -- )
: assert_not_skip 0= assert_skip ;

VARIABLE tc_stack_start
VARIABLE tc_stack_expect

: tc_stack_drop BEGIN DEPTH tc_stack_start @ > WHILE DROP REPEAT ;
: ts{ DEPTH tc_stack_start ! ['] test_skip IS test_skip_fail ;
: t{ DEPTH tc_stack_start ! ['] test_fail IS test_skip_fail ;
: -> DEPTH tc_stack_expect ! ;
: }t
	DEPTH tc_stack_expect @ - DUP >R
	tc_stack_expect @ tc_stack_start @ -
	<> IF R> tc_stack_drop test_skip_fail EXIT THEN
	R@ BEGIN
	  ?DUP
	WHILE
	  1-
	  R@ 1+ PICK ROT
	  <> IF R> tc_stack_drop test_skip_fail EXIT THEN
	REPEAT
	R> tc_stack_drop test_pass
;

: test_suite
	0 tests_passed ! 0 tests_failed ! 0 tests_skipped !
	DECIMAL
;

: test_suite_end
	." Total Pass " tests_passed @ ansi_green U. ansi_normal
	." Fail " tests_failed @ ansi_red U. ansi_normal
	." Skip " tests_skipped @ ansi_magenta U. ansi_normal CR
	tests_failed @ 0<> IF 1 bye-code THEN
;

: test_group ( -- )( R: -- depth)
	['] test_fail IS test_skip_fail	\ S: --		R: ip
	DEPTH R>			\ S: depth ip	R: --
	SWAP >R >R			\ S: --		R: depth ip
	CR
	\ Setup clean-up marker.
	S" MARKER rm_test_group" EVALUATE
;

: test_group_end ( -- )( R: depth -- )
	DEPTH 2R> >R <>
	CR ABORT" Test group stack depth incorrect."
	\ Clean-up test words and data.
	S" rm_test_group" EVALUATE
;

.( Test case stack check ) test_group
t{ -> }t
t{ 1 -> 1 }t
t{ 1 2 -> 1 2 }t
t{ 1 2 3 -> 1 2 3 }t
ts{ -> 377 }t
ts{ 377 -> }t
ts{ 377 -> 377 33 }t
ts{ 377 33 -> 33 }t
test_group_end

0 INVERT CONSTANT 1S

MAX-N 2/ CONSTANT HI-INT		\ 0011...1111
MIN-N 2/ CONSTANT LO-INT 		\ 1100...0001

MAX-U MAX-N  2CONSTANT MAX-2INT		\ 0111...1111
0     MIN-N~ 2CONSTANT MIN-2INT		\ 1000...0000
MAX-2INT  2/ 2CONSTANT HI-2INT		\ 0011...1111
MIN-2INT  2/ 2CONSTANT LO-2INT		\ 1100...0000

[THEN]
