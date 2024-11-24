[UNDEFINED] assert [IF]

MARKER rm_assert

\ Name space prefixes used:
\ ts_ tg_ tc_ tw_ tv_	test suite group case word value


VARIABLE tests_passed
VARIABLE tests_failed
VARIABLE tests_skipped

INCLUDE ../examples/ansiterm.p4

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

VARIABLE tc_ds_start
VARIABLE tc_ds_expect
VARIABLE tc_fs_start
VARIABLE tc_fs_expect

[UNDEFINED] _fs [IF]
\ No float support.
: FDEPTH 0 ;
: FDROP ;
: FPICK ;
: FSWAP ;
: F= FALSE ;
: fs>ds ;
[THEN]

: tc_fs_drop ( F: i*f -- ) BEGIN FDEPTH tc_fs_start @ > WHILE FDROP REPEAT ;
: tc_ds_drop ( i*x -- ) BEGIN DEPTH tc_ds_start @ > WHILE DROP REPEAT ;
: tc_drop_all ( F: i*f -- )( i*x -- ) tc_ds_drop tc_fs_drop ;

: xt{ ( xt -- )
	IS test_skip_fail
	DEPTH tc_ds_start !
	FDEPTH tc_fs_start !
;
: ts{ ( -- ) ['] test_skip xt{ ;
: t{ ( -- ) ['] test_fail xt{ ;

: -> ( -- )
	DEPTH tc_ds_expect !
	FDEPTH tc_fs_expect !
;

: }t_ds ( i*x j*x -- i*x j*x bool )
	DEPTH tc_ds_expect @ - DUP >R
	tc_ds_expect @ tc_ds_start @ -
	<> IF R> DROP TRUE EXIT THEN
	R@ BEGIN
		?DUP
	WHILE
		1-
		R@ 1+ PICK ROT
		<> IF R> 2DROP TRUE EXIT THEN
	REPEAT
	R> DROP FALSE
;
: }t_fs ( F: i*f j*f -- i*f j*f )( -- bool )
	FDEPTH tc_fs_expect @ - DUP 1- >R
	tc_fs_expect @ tc_fs_start @ -
	<> IF R> DROP TRUE EXIT THEN
	R@ 1+ BEGIN
		?DUP
	WHILE
		1-
		\ Float stack is typically small (6) size, so need
		\ to juggle and compare using the data stck.
		fs>ds R@ FPICK fs>ds
		<> IF R> 2DROP TRUE EXIT THEN
	REPEAT
	R> DROP FALSE
;
: }t ( F: i*f -- )( i*x -- )
	}t_ds }t_fs OR IF
		tc_drop_all test_skip_fail EXIT
	THEN
	tc_drop_all test_pass
;

: test_show_stack_sizes
	." Size Ds " stack-cells . ." Fs " floating-stack . ." Rs " return-stack-cells . CR
;

: test_suite ( -- )
	0 tests_passed ! 0 tests_failed ! 0 tests_skipped !
	tc_drop_all
	DECIMAL
;

: test_suite_end ( -- )
	test_show_stack_sizes
	." Total Pass " tests_passed @ ansi_green U. ansi_normal
	." Fail " tests_failed @ ansi_red U. ansi_normal
	." Skip " tests_skipped @ ansi_magenta U. ansi_normal CR
	tests_failed @ 0<> IF 1 bye-status THEN
;

: test_group ( -- )
	['] test_fail IS test_skip_fail
	S" MARKER rm_test_group" EVALUATE CR
;

: test_group_end ( -- )
	CR DEPTH ABORT" Test group stack depth incorrect."
	S" rm_test_group" EVALUATE
;

test_suite
.( Data stack checks. ) test_group
\ Must pass.
t{ -> }t
t{ 1 -> 1 }t
t{ 1 2 -> 1 2 }t
t{ 1 2 3 -> 1 2 3 }t

\ Must fail (skip)
ts{ -> 377 }t
ts{ 377 -> }t
ts{ 377 -> 377 33 }t
ts{ 377 33 -> 33 }t
test_group_end

[DEFINED] _fs [IF]
.( Float and data stack checks ) test_group
\ Just float stack.  Must pass.
t{ 1e0 -> 1e0 }t
t{ 1e0 2e0 -> 1e0 2e0 }t
t{ 1e0 2e0 3e0 -> 1e0 2e0 3e0 }t

\ Mix float and data stacks.  Must pass.
t{ 1 2e0 -> 1 2e0 }t
t{ 1 2e0 3 4.0 -> 1 2e0 3 4.0 }t
t{ 1 2e0 3e0 4 5.0 -> 1 2e0 3e0 4 5.0 }t

\ Mix float and data stacks.  Must fail (skip).
ts{ -> 123.45 }t
ts{ 123.45 -> }t
ts{ 377 -> 377 33.0 }t
ts{ 33.0 -> 377 33.0 }t
ts{ 377 33.0 -> 33.0 }t
test_group_end
[THEN]

test_suite_end

          0 INVERT CONSTANT 1S		\ 1111...1111
1S 1 RSHIFT INVERT CONSTANT MSB		\ 1000...0000

1S 1 RSHIFT CONSTANT MAX-INT		\ 0111...1111
MAX-INT INVERT CONSTANT MIN-INT		\ 1000...0000
MAX-INT 2/ CONSTANT HI-INT		\ 0011...1111
MIN-INT 2/ CONSTANT LO-INT 		\ 1100...0001

1S MAX-INT   2CONSTANT MAX-2INT		\ 0111...1111
0  MIN-INT   2CONSTANT MIN-2INT		\ 1000...0000
MAX-2INT  2/ 2CONSTANT HI-2INT		\ 0011...1111
MIN-2INT  2/ 2CONSTANT LO-2INT		\ 1100...0000

[THEN]
