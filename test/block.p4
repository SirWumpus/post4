INCLUDE ../test/assert.p4

[UNDEFINED] BLOCK-OPEN [IF]

.( Block file support disabled. ) CR

[ELSE]

MARKER rm_block_tests

: tw_tmp_blk S" /tmp/tmp.blk" ;

.( BLOCK-OPEN BLOCK-CLOSE DELETE-FILE ) test_group
t{ _ctx ctx.block_fd @ -> 0 }t
t{ 666 tw_tmp_blk BLOCK-OPEN BLOCK-CLOSE 999 -> 666 0 999 }t
t{ tw_tmp_blk DELETE-FILE -> 0 }t
t{ tw_tmp_blk BLOCK-OPEN -> 0 }t
test_group_end

.( BUFFER UPDATE ) test_group
t{ 1 BUFFER _ctx ctx.block @ blk.buffer = -> TRUE }t
t{ _ctx ctx.block @ blk.number @ -> 1 }t
t{ _ctx ctx.block @ blk.state @ -> 1 }t

t{ 1 BUFFER DUP 1024 BLANK
    CHAR ^ PARSE S" SOURCE-ID BLK @ " EVALUATE ^ ROT SWAP CMOVE
-> }t
t{ _ctx ctx.block @ blk.number @ -> 1 }t
t{ UPDATE _ctx ctx.block @ blk.state @ -> 2 }t
test_group_end

.( BLOCKS SAVE-BUFFERS ) test_group
t{ BLOCKS -> 0 }t
t{ SAVE-BUFFERS -> }t
t{ BLOCKS -> 1 }t
test_group_end

.( LOAD ) test_group
t{ 1 LOAD -> -1 0 }t
test_group_end

.( LIST SCR ) test_group
t{ SCR @ -> 0 }t
t{ CR 1 LIST SCR @ -> 1 }t
test_group_end

.( THRU ) test_group
t{ 2 BUFFER DUP 1024 BLANK
    CHAR ^ PARSE 123 456 + \ Just the facts ma'am ^ ROT SWAP CMOVE
    UPDATE ->  }t
t{ CR 2 LIST SCR @ -> 2 }t
t{ 1 LOAD -> -1 0 }t
t{ 2 LOAD -> 579 }t
t{ 1 LOAD 2 LOAD -> -1 0 579 }t
t{ 1 2 THRU -> -1 0 579 }t
test_group_end

 .( BLOCK ) test_group
t{ CHAR ^ PARSE 123 456 + \ Just the facts ma'am ^ 2 BLOCK OVER COMPARE -> 0 }t
t{ CR SCR @ DUP LIST -> 2 }t
test_group_end

\ GH-16
.( Nested LOAD ) test_group

t{ 3 BUFFER DUP 1024 BLANK
    CHAR ^ PARSE 123 4 LOAD 456^ ROT SWAP CMOVE
    UPDATE
-> }t
t{ 4 BUFFER DUP 1024 BLANK
    CHAR ^ PARSE 666 2 LOAD 1 LOAD 999^ ROT SWAP CMOVE
    UPDATE
-> }t
t{ SAVE-BUFFERS 4 LOAD -> 666 579 -1 0 999 }t
t{ SAVE-BUFFERS 3 LOAD -> 123 666 579 -1 0 999 456 }t

\ Create blocks out of order.
t{ 6 BUFFER DUP 1024 BLANK
    CHAR ^ PARSE S" 666 2 LOAD 1 LOAD 999 " EVALUATE ^ ROT SWAP CMOVE
    UPDATE
-> }t
t{ 5 BUFFER DUP 1024 BLANK
    CHAR ^ PARSE S" 123 6 LOAD 456 " EVALUATE ^ ROT SWAP CMOVE
    UPDATE
-> }t
t{ 6 LOAD -> 666 579 -1 0 999 }t
t{ 5 LOAD -> 123 666 579 -1 0 999 456 }t
t{ BLOCKS -> 6 }t
test_group_end

.( BLOCK-CLOSE DELETE-FILE ) test_group
\ t{ BLOCK-CLOSE tw_tmp_blk DELETE-FILE -> 0 }t
test_group_end

rm_block_tests

[THEN]

