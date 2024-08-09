INCLUDE ../test/assert.p4

[UNDEFINED] BLOCK-OPEN [IF]

.( Block file support disabled. ) CR

[ELSE]

MARKER rm_block_tests

: tw_tmp_blk S" /tmp/tmp.blk" ;

.( BLOCK-OPEN BLOCK-CLOSE ) test_group
t{ tw_tmp_blk BLOCK-OPEN -> TRUE }t
\ t{ :NONAME 1 LOAD ; CATCH -> -33 }t
t{ BLOCK-CLOSE -> }t
t{ tw_tmp_blk DELETE-FILE -> 0 }t
test_group_end

.( BLK BUFFER UPDATE SAVE-BUFFERS ) test_group
t{ tw_tmp_blk BLOCK-OPEN
    1 BUFFER DUP 1024 BLANK
    CHAR ^ PARSE S" SOURCE-ID BLK @ " EVALUATE ^ ROT SWAP CMOVE
    UPDATE SAVE-BUFFERS BLOCK-CLOSE
-> TRUE }t
test_group_end

.( LOAD ) test_group
t{ tw_tmp_blk BLOCK-OPEN 1 LOAD -> TRUE -1 0 }t
t{ BLOCK-CLOSE tw_tmp_blk DELETE-FILE -> 0 }t
test_group_end

rm_block_tests

[THEN]

