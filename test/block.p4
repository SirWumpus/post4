INCLUDE ../test/assert.p4

[UNDEFINED] BLOCK-OPEN [IF]

.( Block file support disabled. ) CR

[ELSE]

MARKER rm_block_tests

: tw_tmp_blk S" /tmp/tmp.blk" ;

.( BLOCK-OPEN ) test_group
t{ BLOCK-CLOSE tw_tmp_blk DELETE-FILE DROP -> }t
t{ tw_tmp_blk BLOCK-OPEN -> TRUE }t
test_group_end

.( BLK BUFFER UPDATE ) test_group
t{ 1 BUFFER DUP 1024 BLANK
    CHAR ^ PARSE S" SOURCE-ID BLK @ " EVALUATE ^ ROT SWAP CMOVE
    UPDATE
-> }t
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
    CHAR ^ PARSE 123 456 + \ Just the facts mam ^ ROT SWAP CMOVE
    UPDATE ->  }t
t{ CR 2 LIST SCR @ -> 2 }t
t{ 1 2 THRU -> -1 0 579 }t
test_group_end

 .( BLOCK ) test_group
t{ CHAR ^ PARSE S" SOURCE-ID BLK @ " EVALUATE ^ 1 BLOCK OVER COMPARE -> 0 }t
test_group_end

.( BLOCK-CLOSE ) test_group
t{ BLOCK-CLOSE tw_tmp_blk DELETE-FILE -> 0 }t
test_group_end

rm_block_tests

[THEN]

