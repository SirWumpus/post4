INCLUDE ../test/assert.p4

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
