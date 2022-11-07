[UNDEFINED] assert [IF]

MARKER rm_assert

VARIABLE tests_passed
VARIABLE tests_failed
VARIABLE group_passed
VARIABLE group_failed

: ansi_normal S\" \e[0m" TYPE ;
: ansi_red S\" \e[31m" TYPE ;
: ansi_green S\" \e[32m" TYPE ;

: pass 1 group_passed +! 1 tests_passed +! ;
: fail 1 group_failed +! 1 tests_failed +! ;

\ ( bool -- )
: assert IF ansi_green ." ." pass ELSE ansi_red ." F" fail THEN ansi_normal ;

\ ( bool -- )
: assert_not 0= assert ;

: test_suite
	0 tests_passed ! 0 tests_failed !
	DECIMAL
;

: test_suite_end
	." Total Pass " tests_passed @ ansi_green U. ansi_normal
	." Fail " tests_failed @ ansi_red U. ansi_normal CR
;

: test_group
	0 group_passed ! 0 group_failed !
	DEPTH
	CR
;

: test_group_end
\	." Pass " group_passed @ ansi_green U. ansi_normal
\	." Fail " group_failed @ ansi_red U. ansi_normal
	DEPTH 1 - <> CR ABORT" Test group stack depth incorrect."
;

[THEN]
