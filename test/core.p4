
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

.( FALSE ) test_group
FALSE 0= assert
test_group_end

.( TRUE ) test_group
TRUE -1 = assert
test_group_end

.( INVERT ) test_group
FALSE INVERT TRUE = assert
TRUE INVERT FALSE = assert
0 INVERT -1 = assert
test_group_end

.( AND ) test_group
0 0 AND 0= assert
0 1 AND 0= assert
1 0 AND 0= assert
1 1 AND 1 = assert
%01 %10 AND 0 = assert
%10 %10 AND 2 = assert
%11 %10 AND 2 = assert
0 INVERT 1 AND 1 = assert
1 INVERT 1 AND 0 = assert
test_group_end

.( OR ) test_group
0 0 OR 0 = assert
0 1 OR 1 = assert
1 0 OR 1 = assert
1 1 OR 1 = assert
%01 %10 OR 3 = assert
test_group_end

.( XOR ) test_group
0 0 XOR 0 = assert
0 1 XOR 1 = assert
1 0 XOR 1 = assert
1 1 XOR 0 = assert
%01 %10 XOR 3 = assert
%11 %01 XOR 2 = assert
test_group_end

.( Numeric notation ) test_group
#1289 1289 = assert
\ #12346789. 12346789. = assert
#-1289 -1289 = assert
\ #-12346789. -12346789. = assert
$12eF 4847 = assert
\ $12aBcDeF. 313249263. = assert
$-12eF -4847 = assert
\ $-12AbCdEf. -313249263. = assert
%10010110 150 = assert
\ %10010110. 150. = assert
%-10010110 -150 = assert
\ Post4 extensions.
0x12eF 4847 = assert
0x-12eF -4847 = assert
0377 255 = assert
0-377 -255 = assert
0177777 65535 = assert
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
 0 NEGATE  0 = assert
 1 NEGATE -1 = assert
-1 NEGATE  1 = assert
 2 NEGATE -2 = assert
-2 NEGATE  2 = assert
MAX-N NEGATE MIN-N = assert
MIN-N NEGATE MAX-N = assert
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

.( ABS ) test_group
 0 ABS 0 = assert
 1 ABS 1 = assert
-1 ABS 1 = assert
MIN-N ABS MAX-N = assert
test_group_end

rm_core_words
