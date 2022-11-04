
INCLUDE ../test/assert.p4

MARKER rm_core_words

.( 0= ) test_group
0 0= assert
1 0= assert_not
-1 0= assert_not
test_group_end

.( 0< ) test_group
0 0< assert_not
1 0< assert_not
-1 0< assert
-2 0< assert
test_group_end

.( = ) test_group
0 0 = assert
12 12 = assert
12 34 = assert_not
-1 -1 = assert
-21 -21 = assert
-21 -43 = assert_not
test_group_end

.( + ) test_group
0 0 + 0 = assert
1 0 + 1 = assert
0 1 + 1 = assert
1 1 + 2 = assert
-1 1 + 0 = assert
1 -1 + 0 = assert
test_group_end

.( NEGATE ) test_group
0 NEGATE 0 = assert
1 NEGATE -1 = assert
-1 NEGATE 1 = assert
0x1234 NEGATE 0xffffffffffffedcc = assert
0xffffffffffffedcc NEGATE 0x1234 = assert
test_group_end

.( - ) test_group
0 0 - 0 = assert
1 0 - 1 = assert
2 1 - 1 = assert
0 1 - -1 = assert
1 -1 - 2 = assert
test_group_end

.( U< ) test_group
123 123 U< assert_not
122 123 U< assert
124 124 U< assert_not
test_group_end

.( < ) test_group
123 123 < assert_not
122 123 < assert
124 123 < assert_not
-123 123 < assert
123 -123 < assert_not
test_group_end

.( DROP ) test_group
12 34 DROP 12 = assert
test_group_end

.( DUP ) test_group
12 DUP = assert
test_group_end

.( OVER ) test_group
12 34 OVER 12 = assert
34 = assert
12 = assert
test_group_end

.( SWAP ) test_group
12 34 SWAP 12 = assert
34 = assert
test_group_end

.( ROT ) test_group
12 34 56 ROT 12 = assert
56 = assert
34 = assert
test_group_end

.( CONSTANT ) test_group
0xCAFE CONSTANT java java 0xCAFE = assert
test_group_end

.( VARIABLE ! @ ) test_group
VARIABLE woot woot @ 0= assert
0xCAFE woot ! woot @ 0xCAFE = assert
0xDEAD woot ! woot @ 0xCAFE = assert_not
woot @ 0xDEAD = assert
test_group_end

rm_core_words
