INCLUDE ../test/assert.p4

[UNDEFINED] CREATE-FILE [IF]

.( File access support disabled. ) CR

[ELSE]

.( CREATE-FILE CLOSE-FILE ) test_group
VARIABLE tv_fid1
: tw_fn1 S" /tmp/empty.txt" ;

T{ tw_fn1 R/W CREATE-FILE SWAP tv_fid1 ! -> 0 }T
T{ tv_fid1 @ CLOSE-FILE -> 0 }T
test_group_end

.( DELETE-FILE OPEN-FILE ) test_group
VARIABLE tv_fid1
: tw_fn1 S" /tmp/empty.txt" ;

T{ S" /tmp/bogus.txt" DELETE-FILE 0<> -> TRUE }T
T{ tw_fn1 DELETE-FILE -> 0 }T
T{ tw_fn1 R/W BIN OPEN-FILE SWAP tv_fid1 ! -> 0 }T
T{ tw_fn1 DELETE-FILE -> 0 }T
T{ tv_fid1 @ CLOSE-FILE -> 0 }T
test_group_end

.( SOURCE-ID ) test_group
T{ SOURCE-ID DUP -1 = SWAP 0= OR -> FALSE }T
test_group_end

[THEN]
