INCLUDE ../test/assert.p4

.( Exceptions ) test_group
: tw_abort" $BEEF SWAP ABORT" Should not appear.  BEEF is stack marker." ;

\ ABORT and QUIT never return to caller, cannot be caught, see
\ https://github.com/ForthHub/discussion/discussions/116#discussioncomment-3518213
\ Though a word can throw -1, -2, or -56 directly.
: tw_catch_cold
	CATCH CASE
		 -1 OF 1001 ENDOF
		 -2 OF 1002 ENDOF
		-13 OF 1013 ENDOF
		-56 OF 1056 ENDOF
	ENDCASE
;

\ ABORT caught.
\ $DEAD ' ABORT tw_catch_cold 1001 = assert $DEAD = assert

\ No ABORT" error, pass through.
$DEAD 0 ' tw_abort" tw_catch_cold $BEEF = assert $DEAD = assert

\ ABORT" caught, no message.
$DEAD 1 ' tw_abort" tw_catch_cold 1002 = assert $BEEF = assert $DEAD = assert
test_group_end

.( CATCH basic ) test_group
: tw_catch_0 ['] TRUE ;
' TRUE CATCH 0= assert assert
' tw_catch_0 CATCH 0= assert assert
test_group_end

.( CATCH THROW ) test_group

: tw_throw_noop $CAFE 0 THROW ;
: tw_catch_noop $FEED ['] tw_throw_noop CATCH ;
tw_catch_noop 0= assert $CAFE = assert $FEED = assert

: tw_throw_depth 10 20 69 THROW ;
: tw_catch_depth 1 2 ['] tw_throw_depth CATCH ;
tw_catch_depth 69 = assert 2 = assert 1 = assert

: tw_throw_stack 2DROP 2DROP 6969 THROW ;
: tw_catch_stack 1 2 3 4 ['] tw_throw_stack CATCH DEPTH >R DROP 2DROP 2DROP R> ;
\ Test suite and/or group may have adminstration data on stack at.
tw_catch_stack 5 = assert

: tw_throw_unwind 1- DUP 0> IF RECURSE ELSE $999 THROW $222 THEN ;
: tw_catch_unwind $9876 10 ['] tw_throw_unwind CATCH $111 ;
tw_catch_unwind $111 = assert $999 = assert 0= assert $9876 = assert

test_group_end

.( -56 THROW ) test_group
\ https://github.com/ForthHub/discussion/discussions/116#discussioncomment-3541822

: tw_throw_neg56 123 [: 456 -56 THROW ;] CATCH ;
tw_throw_neg56 -56 = assert 123 = assert

\ While QUIT leaves the data stack untouched, it does resets the return
\ stack, disrupting the test suite.  Also isolate QUIT with EVALUATE,
\ which saves input source state.
: tw_quit_noreturn [: 123 QUIT ;] CATCH 456 THROW ;
_rsp@ S" tw_quit_noreturn" EVALUATE SWAP _rsp! 123 = assert

test_group_end
