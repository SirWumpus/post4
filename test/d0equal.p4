
INCLUDE ../test/assert.p4

MARKER rm_d0equal_words

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

.( D0= ) test_group
T{    1 S>D D0= -> FALSE }T
T{  MIN-N 0 D0= -> FALSE }T
T{ MAX-2INT D0= -> FALSE }T
T{ -1 MAX-N D0= -> FALSE }T
T{    0 S>D D0= -> TRUE  }T
T{   -1 S>D D0= -> FALSE }T
T{  0 MIN-N D0= -> FALSE }T
test_group_end

.( D0< ) test_group
T{    0 S>D D0< -> FALSE }T
T{    1 S>D D0< -> FALSE }T
T{  MIN-N 0 D0< -> FALSE }T
T{  0 MAX-N D0< -> FALSE }T
T{ MAX-2INT D0< -> FALSE }T
T{   -1 S>D D0< -> TRUE  }T
T{ MIN-2INT D0< -> TRUE  }T
test_group_end

.( D= ) test_group
0 0 0 0 D= assert
2 1 2 1 D= assert
2 1 4 3 D= assert_not
-1 -1 -1 -1 D= assert
-2 -1 -2 -1 D= assert
-2 -1 2 1 D= assert_not

T{ -1 S>D -1 S>D D= -> TRUE  }T
T{ -1 S>D  0 S>D D= -> FALSE }T
T{ -1 S>D  1 S>D D= -> FALSE }T
T{  0 S>D -1 S>D D= -> FALSE }T
T{  0 S>D  0 S>D D= -> TRUE  }T
T{  0 S>D  1 S>D D= -> FALSE }T
T{  1 S>D -1 S>D D= -> FALSE }T
T{  1 S>D  0 S>D D= -> FALSE }T
T{  1 S>D  1 S>D D= -> TRUE  }T

T{ 0 -1 0 -1 D= -> TRUE  }T
T{ 0 -1 0  0 D= -> FALSE }T
T{ 0 -1 0  1 D= -> FALSE }T
T{ 0  0 0 -1 D= -> FALSE }T
T{ 0  0 0  0 D= -> TRUE  }T
T{ 0  0 0  1 D= -> FALSE }T
T{ 0  1 0 -1 D= -> FALSE }T
T{ 0  1 0  0 D= -> FALSE }T
T{ 0  1 0  1 D= -> TRUE  }T

T{ MAX-2INT MIN-2INT D= -> FALSE }T
T{ MAX-2INT    0 S>D D= -> FALSE }T
T{ MAX-2INT MAX-2INT D= -> TRUE  }T
T{ MAX-2INT  HI-2INT D= -> FALSE }T
T{ MAX-2INT MIN-2INT D= -> FALSE }T
T{ MIN-2INT MIN-2INT D= -> TRUE  }T
T{ MIN-2INT  LO-2INT D= -> FALSE }T
T{ MIN-2INT MAX-2INT D= -> FALSE }T
test_group_end

.( D+ ) test_group
T{  0  0 1 0 D+ -> 1 0 }T
T{  1  0 1 0 D+ -> 2 0 }T
T{ -1 -1 1 0 D+ -> 0 0 }T
T{ -1  0 1 0 D+ -> 0 1 }T

\ small integers
T{  0 S>D  5 S>D D+ ->  5 S>D }T
T{ -5 S>D  0 S>D D+ -> -5 S>D }T
T{  1 S>D  2 S>D D+ ->  3 S>D }T
T{  1 S>D -2 S>D D+ -> -1 S>D }T
T{ -1 S>D  2 S>D D+ ->  1 S>D }T
T{ -1 S>D -2 S>D D+ -> -3 S>D }T
T{ -1 S>D  1 S>D D+ ->  0 S>D }T

\ mid range integers
T{  0  0  0  5 D+ ->  0  5 }T
T{ -1  5  0  0 D+ -> -1  5 }T
T{  0  0  0 -5 D+ ->  0 -5 }T
T{  0 -5 -1  0 D+ -> -1 -5 }T
T{  0  1  0  2 D+ ->  0  3 }T
T{ -1  1  0 -2 D+ -> -1 -1 }T
T{  0 -1  0  2 D+ ->  0  1 }T
T{  0 -1 -1 -2 D+ -> -1 -3 }T
T{ -1 -1  0  1 D+ -> -1  0 }T

T{ MIN-N~ 0 2DUP D+ -> 0 1 }T
T{ MIN-N~ S>D MIN-N~ 0 D+ -> 0 0 }T

\ large double integers
T{ HI-2INT 1 S>D D+ -> 0 HI-INT 1+ }T
T{ HI-2INT 2DUP D+ -> 1S 1- MAX-N }T
T{ MAX-2INT MIN-2INT D+ -> -1 S>D }T
T{ MAX-2INT LO-2INT D+ -> HI-2INT }T
T{ LO-2INT 2DUP D+ -> MIN-2INT }T
T{ HI-2INT MIN-2INT D+ 1 S>D D+ -> LO-2INT }T
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
T{ 1 0 1 0 D- ->  0  0 }T
T{ 2 0 1 0 D- ->  1  0 }T
T{ 0 0 1 0 D- -> -1 -1 }T
T{ 0 1 1 0 D- -> -1  0 }T

\ small double integers
T{  0 S>D  5 S>D D- -> -5 S>D }T
T{  5 S>D  0 S>D D- ->  5 S>D }T
T{  0 S>D -5 S>D D- ->  5 S>D }T
T{  1 S>D  2 S>D D- -> -1 S>D }T
T{  1 S>D -2 S>D D- ->  3 S>D }T
T{ -1 S>D  2 S>D D- -> -3 S>D }T
T{ -1 S>D -2 S>D D- ->  1 S>D }T
T{ -1 S>D -1 S>D D- ->  0 S>D }T

\ mid-range double integers
T{  0  0  0  5 D- ->  0 -5 }T
T{ -1  5  0  0 D- -> -1  5 }T
T{  0  0 -1 -5 D- ->  1  4 }T
T{  0 -5  0  0 D- ->  0 -5 }T
T{ -1  1  0  2 D- -> -1 -1 }T
T{  0  1 -1 -2 D- ->  1  2 }T
T{  0 -1  0  2 D- ->  0 -3 }T
T{  0 -1  0 -2 D- ->  0  1 }T
T{  0  0  0  1 D- ->  0 -1 }T

T{ MIN-N~ 0 2DUP D- -> 0 0 }T
T{ MIN-N~ S>D MAX-N 0 D- -> 1 MAX-U }T

\ large double integers
T{ MAX-2INT MAX-2INT D- -> 0 S>D }T
T{ MIN-2INT MIN-2INT D- -> 0 S>D }T
T{ MAX-2INT HI-2INT  D- -> LO-2INT DNEGATE }T
T{ HI-2INT  LO-2INT  D- -> MAX-2INT }T
T{ LO-2INT  HI-2INT  D- -> MIN-2INT 1 S>D D+ }T
T{ MIN-2INT MIN-2INT D- -> 0 S>D }T
T{ MIN-2INT LO-2INT  D- -> LO-2INT }T
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
T{ 100 99 2 1 4 3 6 5 2ROT -> 100 99 4 3 6 5 2 1 }T
T{ MAX-D MIN-D 0xCAFE 0xBABE 2ROT -> MIN-D 0xCAFE 0xBABE MAX-D }T
[UNDEFINED] _fs cell-bits 64 < AND [IF]
\ Assumes floats are double cells on the data-stack.
T{ 1. 2. 3. 2ROT -> 2. 3. 1. }T
[THEN]
test_group_end

.( 2CONSTANT ) test_group
0xCAFE 0xBABE 2CONSTANT tv_pair tv_pair 0xCAFE 0xBABE D= assert
T{ 1 2 2CONSTANT tv_2c1 -> }T
T{ tv_2c1 -> 1 2 }T
T{ : tw_cd1 tv_2c1 ; -> }T
T{ tw_cd1 -> 1 2 }T
T{ : tw_cd2 2CONSTANT ; -> }T
T{ -1 -2 tw_cd2 tv_2c2 -> }T
T{ tv_2c2 -> -1 -2 }T
T{ 4 5 2CONSTANT tv_2c3 IMMEDIATE tv_2c3 -> 4 5 }T
T{ : tw_cd6 tv_2c3 2LITERAL ; tw_cd6 -> 4 5 }T
test_group_end

.( 2VARIABLE 2! 2@ ) test_group
2VARIABLE var_pair var_pair 2@ D0= assert
0xCAFE 0xBABE var_pair 2! var_pair 2@ 0xCAFE 0xBABE D= assert
0xDEAD 0xBEEF var_pair 2! var_pair 2@ 0xCAFE 0xBABE D= assert_not
var_pair 2@ 0xDEAD 0xBEEF D= assert
T{ 2VARIABLE tv_2v1 -> }T
T{ 0 S>D tv_2v1 2! -> }T
T{ tv_2v1 2@ -> 0 S>D }T
T{ -1 -2 tv_2v1 2! -> }T
T{ tv_2v1 2@ -> -1 -2 }T
T{ : tw_cd2 2VARIABLE ; -> }T
T{ tw_cd2 tv_2v2 -> }T
T{ : tw_cd3 tv_2v2 2! ; -> }T
T{ -2 -1 tw_cd3 -> }T
T{ tv_2v2 2@ -> -2 -1 }T
T{ 2VARIABLE tv_2v3 IMMEDIATE 5 6 tv_2v3 2! -> }T
T{ tv_2v3 2@ -> 5 6 }T
test_group_end

.( 2LITERAL 2VARIABLE 2! 2@ ) test_group
T{ : tw_cd1 [ MAX-D ] 2LITERAL ; -> }T
T{ tw_cd1 -> MAX-D }T
T{ 2VARIABLE tv_2v4 IMMEDIATE 5 6 tv_2v4 2! -> }T
T{ : tw_cd7 tv_2v4 [ 2@ ] 2LITERAL ; tw_cd7 -> 5 6 }T
T{ : tw_cd8 [ 6 7 ] tv_2v4 [ 2! ] ; tv_2v4 2@ -> 6 7 }T
test_group_end

.( 2VALUE TO 2SWAP ) test_group
T{ 1 2 2VALUE tv_t2val -> }T
T{ tv_t2val -> 1 2 }T
T{ 3 4 TO tv_t2val -> }T
T{ tv_t2val -> 3 4 }T
T{ : tw_sett2val tv_t2val 2SWAP TO tv_t2val ; -> }T
T{ 5 6 tw_sett2val tv_t2val -> 3 4 5 6 }T
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
