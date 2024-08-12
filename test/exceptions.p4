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
t{ $DEAD 0 ' tw_abort" tw_catch_cold -> $DEAD $BEEF }t

\ ABORT" caught, no message.
t{ $DEAD 1 ' tw_abort" tw_catch_cold -> $DEAD $BEEF 1002 }t
test_group_end

.( CATCH basic ) test_group
: tw_catch_0 ['] TRUE ;
t{ ' TRUE CATCH -> TRUE 0 }t
t{ ' tw_catch_0 CATCH -> ' TRUE 0 }t
test_group_end

.( CATCH THROW ) test_group

: tw_throw_noop $CAFE 0 THROW ;
: tw_catch_noop $FEED ['] tw_throw_noop CATCH ;
t{ tw_catch_noop -> $FEED $CAFE 0 }t

: tw_throw_depth 10 20 69 THROW ;
: tw_catch_depth 1 2 ['] tw_throw_depth CATCH ;
t{ tw_catch_depth -> 1 2 69 }t

: tw_throw_stack 2DROP 2DROP 6969 THROW ;
: tw_catch_stack 1 2 3 4 ['] tw_throw_stack CATCH DEPTH >R DROP 2DROP 2DROP R> ;
\ Test suite and/or group may have adminstration data on stack at.
t{ tw_catch_stack -> 5 }t

: tw_throw_unwind 1- DUP 0> IF RECURSE ELSE $999 THROW $222 THEN ;
: tw_catch_unwind $9876 10 ['] tw_throw_unwind CATCH $111 ;
t{ tw_catch_unwind -> $9876 0 $999 $111 }t

\ GH-8
T{ 123 :NONAME 456 -1 THROW ; CATCH -> 123 -1 }T
T{ 123 :NONAME 456 ABORT ; CATCH -> 123 -1 }T

T{ 123 :NONAME 456 -2 THROW ; CATCH -> 123 -2 }T
T{ 123 :NONAME 456 ABORT" TEST ERROR" ; CATCH -> 123 -2 }T

T{ 123 :NONAME 456 1138 THROW ; CATCH -> 123 1138 }T
T{ 123 :NONAME 456 HERE THROW ; CATCH -> 123 HERE }T

test_group_end

.( -56 THROW ) test_group
\ https://github.com/ForthHub/discussion/discussions/116#discussioncomment-3541822
t{ :NONAME 123 [: 456 -56 THROW ;] CATCH ; EXECUTE -> 123 -56 }t

\ While QUIT leaves the data stack untouched, it does resets the return
\ stack, disrupting the test suite.  Also isolate QUIT with EVALUATE,
\ which saves input source state.
\ : tw_quit_noreturn [: 123 QUIT ;] CATCH 456 THROW ;
\ t{ _rsp@ S" tw_quit_noreturn" EVALUATE SWAP _rsp! -> 123 }t

test_group_end
