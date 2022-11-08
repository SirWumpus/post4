
INCLUDE ../test/assert.p4

MARKER rm_d0equal_words

.( D0= ) test_group
0 0 D0= assert
2 1 D0= assert_not
-1 -1 D0= assert_not
-2 -1 D0= assert_not
test_group_end

.( D0< ) test_group
0 0 D0< assert_not
2 1 D0= assert_not
-1 -1 D0< assert
-2 -1 D0< assert
test_group_end

.( D= ) test_group
0 0 0 0 D= assert
2 1 2 1 D= assert
2 1 4 3 D= assert_not
-1 -1 -1 -1 D= assert
-2 -1 -2 -1 D= assert
-2 -1 2 1 D= assert_not
test_group_end

.( D+ ) test_group
0 0 1 0 D+ 1 0 D= assert
1 0 1 0 D+ 2 0 D= assert
-1 -1 1 0 D+ D0= assert
-1 0 1 0 D+ 0 1 D= assert
test_group_end

.( S>D ) test_group
 0 S>D  0  0 D= assert
 1 S>D  1  0 D= assert
 2 S>D  2  0 D= assert
-1 S>D -1 -1 D= assert
-2 S>D -2 -1 D= assert
MIN-N S>D MIN-N -1 D= assert
MAX-N S>D MAX-N  0 D= assert
test_group_end

.( D>S ) test_group
 1234  0 D>S  1234 = assert
-1234 -1 D>S -1234 = assert
MAX-N  0 D>S MAX-N = assert
MIN-N -1 D>S MIN-N = assert
test_group_end

.( DNEGATE ) test_group
0 0 DNEGATE 0 0 D= assert
MAX-UD DNEGATE 1 0 D= assert
1 0 DNEGATE MAX-UD D= assert
MAX-D DNEGATE MIN-D D= assert
MIN-D DNEGATE MAX-D D= assert
0x1234 0 DNEGATE 0xffffffffffffedcc MAX-U D= assert
0xffffffffffffedcc MAX-U DNEGATE 0x1234 0 D= assert
test_group_end

.( D- ) test_group
1 0 1 0 D- D0= assert
2 0 1 0 D- 1 0 D= assert
0 0 1 0 D- -1 -1 D= assert
0 1 1 0 D- -1 0 D= assert
test_group_end

MAX-N 2/ CONSTANT HALF-MAX-N
MIN-N 2/ CONSTANT HALF-MIN-N
MAX-D 2/ 2CONSTANT HALF-MAX-D
MIN-D 2/ 2CONSTANT HALF-MIN-D

.( DU< ) test_group
 1 S>D  1 S>D DU< assert_not
 1 S>D -1 S>D DU< assert
-1 S>D  1 S>D DU< assert_not
-1 S>D -2 S>D DU< assert_not
MAX-D HALF-MAX-D DU< assert_not
HALF-MAX-D MAX-D DU< assert
MAX-D MIN-D~ DU< assert
MIN-D~ MAX-D DU< assert_not
MIN-D~ HALF-MIN-D DU< assert
test_group_end

.( D< ) test_group
 0 S>D  1 S>D D< assert
 0 S>D  0 S>D D< assert_not
 1 S>D  0 S>D D< assert_not
-1 S>D  1 S>D D< assert
-1 S>D  0 S>D D< assert
-2 S>D -1 S>D D< assert
-1 S>D -2 S>D D< assert_not
-1 S>D MAX-D D< assert
MIN-D  MAX-D D< assert
MAX-D -1 S>D D< assert_not
MAX-D MIN-D  D< assert_not
MAX-D 2DUP -1 S>D D+ D< assert_not
MIN-D 2DUP  1 S>D D+ D< assert
test_group_end

.( 2DROP ) test_group
1 2 3 4 2DROP 1 2 D= assert
test_group_end

.( 2DUP ) test_group
1 2 2DUP D= assert
test_group_end

.( 2OVER ) test_group
100 99 1 2 3 4 2OVER 1 2 D= assert
3 4 D= assert
1 2 D= assert
100 99 D= assert
test_group_end

.( 2SWAP ) test_group
100 99 1 2 3 4 2SWAP 1 2 D= assert
3 4 D= assert
100 99 D= assert
test_group_end

.( 2ROT ) test_group
\ :NONAME 2 1 2ROT ; CATCH -4 = assert
100 99 2 1 4 3 6 5 2ROT 2 1 D= assert
6 5 D= assert
4 3 D= assert
100 99 D= assert
test_group_end

.( 2CONSTANT ) test_group
0xCAFE 0xBABE 2CONSTANT const_pair const_pair 0xCAFE 0xBABE D= assert
test_group_end

.( 2VARIABLE 2! 2@ ) test_group
2VARIABLE var_pair var_pair 2@ D0= assert
0xCAFE 0xBABE var_pair 2! var_pair 2@ 0xCAFE 0xBABE D= assert
0xDEAD 0xBEEF var_pair 2! var_pair 2@ 0xCAFE 0xBABE D= assert_not
var_pair 2@ 0xDEAD 0xBEEF D= assert
test_group_end

.( DABS ) test_group
 1 S>D DABS 1 0 D= assert
-1 S>D DABS 1 0 D= assert
MAX-D DABS MAX-D D= assert
MIN-D DABS MAX-D D= assert
MIN-D~ 1 0 D+ DABS MAX-D D= assert
test_group_end

.( DMAX ) test_group
 1 S>D  2 S>D DMAX  2 S>D D= assert
 1 S>D  0 S>D DMAX  1 S>D D= assert
 1 S>D -1 S>D DMAX  1 S>D D= assert
 1 S>D  1 S>D DMAX  1 S>D D= assert
 0 S>D  1 S>D DMAX  1 S>D D= assert
 0 S>D -1 S>D DMAX  0 S>D D= assert
-1 S>D  1 S>D DMAX  1 S>D D= assert
-1 S>D -2 S>D DMAX -1 S>D D= assert
MAX-D HALF-MAX-D DMAX MAX-D D= assert
MAX-D MIN-D~ DMAX MAX-D D= assert
MIN-D~ MAX-D DMAX MAX-D D= assert
MIN-D HALF-MIN-D DMAX HALF-MIN-D D= assert
MAX-D   1 S>D DMAX MAX-D D= assert
MAX-D  -1 S>D DMAX MAX-D D= assert
MIN-D~  1 S>D DMAX  1 S>D D= assert
MIN-D~ -1 S>D DMAX -1 S>D D= assert
test_group_end

.( DMIN ) test_group
 1 S>D  2 S>D DMIN  1 S>D D= assert
 1 S>D  0 S>D DMIN  0 S>D D= assert
 1 S>D -1 S>D DMIN -1 S>D D= assert
 1 S>D  1 S>D DMIN  1 S>D D= assert
 0 S>D  1 S>D DMIN  0 S>D D= assert
 0 S>D -1 S>D DMIN -1 S>D D= assert
-1 S>D  1 S>D DMIN -1 S>D D= assert
-1 S>D -2 S>D DMIN -2 S>D D= assert
MAX-D HALF-MAX-D DMIN HALF-MAX-D D= assert
MAX-D MIN-D~ DMIN MIN-D~ D= assert
MIN-D~ MAX-D DMIN MIN-D~ D= assert
MIN-D HALF-MIN-D DMIN MIN-D D= assert
MAX-D   1 S>D DMIN  1 S>D D= assert
MAX-D  -1 S>D DMIN -1 S>D D= assert
MIN-D~  1 S>D DMIN MIN-D~ D= assert
MIN-D~ -1 S>D DMIN MIN-D~ D= assert
test_group_end

rm_d0equal_words
