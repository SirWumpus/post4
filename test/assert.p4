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
: assert IF ansi_green ." ." ansi_normal pass ELSE ansi_red ." F" ansi_normal fail THEN ;

\ ( bool -- )
: assert_not 0= assert ;

: test_results
	." Total Pass " tests_passed @ ansi_green . ansi_normal
	." Fail " tests_failed @ ansi_red . ansi_normal CR
	0 tests_passed ! 0 tests_failed !
;

: test_group
	0 group_passed ! 0 group_failed !
	DEPTH
	CR
;

: test_group_end
\	CR
\	." Pass " group_passed @ ansi_green . ansi_normal
\	." Fail " group_failed @ ansi_red . ansi_normal CR
	DEPTH 1 - = assert
	CR
;

[THEN]
