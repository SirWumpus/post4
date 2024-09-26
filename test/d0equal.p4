
INCLUDE ../test/assert.p4

MARKER rm_d0equal_words

.( S>D ) test_group
t{  0 S>D ->  0  0 }t
t{  1 S>D ->  1  0 }t
t{  2 S>D ->  2  0 }t
t{ -1 S>D -> -1 -1 }t
t{ -2 S>D -> -2 -1 }t
t{ MIN-N S>D -> MIN-N -1 }t
t{ MAX-N S>D -> MAX-N  0 }t
test_group_end

.( D>S ) test_group
t{  1234  0 D>S ->  1234 }t
t{ -1234 -1 D>S -> -1234 }t
t{ MAX-N  0 D>S -> MAX-N }t
t{ MIN-N -1 D>S -> MIN-N }t
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
t{  0  0  0  0 D= ->  TRUE }t
t{  2  1  2  1 D= ->  TRUE }t
t{  2  1  4  3 D= -> FALSE }t
t{ -1 -1 -1 -1 D= ->  TRUE }t
t{ -2 -1 -2 -1 D= ->  TRUE }t
t{ -2 -1  2  1 D= -> FALSE }t

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

T{ MIN-INT 0 2DUP D+ -> 0 1 }T
T{ MIN-INT S>D MIN-INT 0 D+ -> 0 0 }T

\ large double integers
T{ HI-2INT 1 S>D D+ -> 0 HI-INT 1+ }T
T{ HI-2INT 2DUP D+ -> 1S 1- MAX-N }T
T{ MAX-2INT MIN-2INT D+ -> -1 S>D }T
T{ MAX-2INT LO-2INT D+ -> HI-2INT }T
T{ LO-2INT 2DUP D+ -> MIN-2INT }T
T{ HI-2INT MIN-2INT D+ 1 S>D D+ -> LO-2INT }T
test_group_end

.( DNEGATE ) test_group
T{ 0 S>D DNEGATE -> 0 S>D }T
T{ 1 S>D DNEGATE -> -1 S>D }T
T{ -1 S>D DNEGATE -> 1 S>D }T
T{ $1234 S>D DNEGATE $-1234 S>D }T
T{ $-1234 S>D DNEGATE $1234 S>D }T
T{ MAX-2INT DNEGATE -> MIN-2INT SWAP 1+ SWAP }T
T{ MIN-2INT SWAP 1+ SWAP DNEGATE -> MAX-2INT }T
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

T{ MIN-INT 0 2DUP D- -> 0 0 }T
T{ MIN-INT S>D MAX-INT 0 D- -> 1 1S }T

\ large double integers
T{ MAX-2INT MAX-2INT D- -> 0 S>D }T
T{ MIN-2INT MIN-2INT D- -> 0 S>D }T
T{ MAX-2INT HI-2INT  D- -> LO-2INT DNEGATE }T
T{ HI-2INT  LO-2INT  D- -> MAX-2INT }T
T{ LO-2INT  HI-2INT  D- -> MIN-2INT 1 S>D D+ }T
T{ MIN-2INT MIN-2INT D- -> 0 S>D }T
T{ MIN-2INT LO-2INT  D- -> LO-2INT }T
test_group_end

MAX-INT 2/ CONSTANT HALF-MAX-N
MIN-INT 2/ CONSTANT HALF-MIN-N
MAX-2INT 2/ 2CONSTANT HALF-MAX-D
MIN-2INT 2/ 2CONSTANT HALF-MIN-D

.( DU< ) test_group
t{  1 S>D  1 S>D DU< -> FALSE }t
t{  1 S>D -1 S>D DU< -> TRUE }t
t{ -1 S>D  1 S>D DU< -> FALSE }t
t{ -1 S>D -2 S>D DU< -> FALSE }t
t{ MAX-D HALF-MAX-D DU< -> FALSE }t
t{ HALF-MAX-D MAX-D DU< -> TRUE }t
t{ MAX-D MIN-D DU< -> TRUE }t
t{ MIN-D MAX-D DU< -> FALSE }t
t{ MIN-D HALF-MIN-D DU< -> TRUE }t
test_group_end

.( D< ) test_group
t{  0 S>D  1 S>D D< -> TRUE }t
t{  0 S>D  0 S>D D< -> FALSE }t
t{  1 S>D  0 S>D D< -> FALSE }t
t{ -1 S>D  1 S>D D< -> TRUE }t
t{ -1 S>D  0 S>D D< -> TRUE }t
t{ -2 S>D -1 S>D D< -> TRUE }t
t{ -1 S>D -2 S>D D< -> FALSE }t
t{ -1 S>D MAX-D D< -> TRUE }t
t{ MIN-D  MAX-D D< -> TRUE }t
t{ MAX-D 1S S>D D< -> FALSE }t
t{ MAX-D MIN-D  D< -> FALSE }t
t{ MAX-D 2DUP -1 S>D D+ D< -> FALSE }t
t{ MIN-D 2DUP  1 S>D D+ D< -> TRUE }t
test_group_end

.( 2DROP ) test_group
t{ 1 2 3 4 2DROP -> 1 2 }t
test_group_end

.( 2DUP ) test_group
t{ 1 2 2DUP -> 1 2 1 2 }t
test_group_end

.( 2OVER ) test_group
t{ 100 99 1 2 3 4 2OVER -> 100 99 1 2 3 4 1 2 }t
test_group_end

.( 2SWAP ) test_group
t{ 100 99 1 2 3 4 2SWAP -> 100 99 3 4 1 2 }t
test_group_end

.( 2ROT ) test_group
\ :NONAME 2 1 2ROT ; CATCH -4 = assert
T{ 100 99 2 1 4 3 6 5 2ROT -> 100 99 4 3 6 5 2 1 }T
T{ MAX-D MIN-D $CAFE $BABE 2ROT -> MIN-D $CAFE $BABE MAX-D }T
T{ 1 S>D 2 S>D 3 S>D 2ROT -> 2 S>D 3 S>D 1 S>D }T
test_group_end

.( 2CONSTANT ) test_group
T{ $CAFE $BABE 2CONSTANT tv_pair tv_pair -> $CAFE $BABE }T
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
t{ 2VARIABLE var_pair var_pair 2@ -> 0 0 }t
t{ $CAFE $BABE var_pair 2! var_pair 2@ -> $CAFE $BABE }t
t{ $DEAD $BEEF var_pair 2! -> }t
t{ var_pair 2@ -> $DEAD $BEEF }t
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
T{  1 S>D DABS -> 1 0 }T
T{ -1 S>D DABS -> 1 0 }T
T{ MAX-2INT DABS -> MAX-2INT }T
T{ MIN-2INT 1 S>D D+ DABS -> MAX-2INT }T
test_group_end

.( DMAX ) test_group
T{  1 S>D  2 S>D DMAX ->  2 S>D }T
T{  1 S>D  0 S>D DMAX ->  1 S>D }T
T{  1 S>D -1 S>D DMAX ->  1 S>D }T
T{  1 S>D  1 S>D DMAX ->  1 S>D }T
T{  0 S>D  1 S>D DMAX ->  1 S>D }T
T{  0 S>D -1 S>D DMAX ->  0 S>D }T
T{ -1 S>D  1 S>D DMAX ->  1 S>D }T
T{ -1 S>D -2 S>D DMAX -> -1 S>D }T
T{ MAX-D HALF-MAX-D DMAX -> MAX-D }T
T{ MAX-D MIN-D DMAX -> MAX-D }T
T{ MIN-D MAX-D DMAX -> MAX-D }T
T{ MIN-D HALF-MIN-D DMAX HALF-MIN-D }T
T{ MAX-2INT  1 S>D DMAX -> MAX-2INT }T
T{ MAX-2INT -1 S>D DMAX -> MAX-2INT }T
T{ MIN-2INT  1 S>D DMAX ->  1 S>D }T
T{ MIN-2INT -1 S>D DMAX -> -1 S>D }T
test_group_end

.( DMIN ) test_group
T{  1 S>D  2 S>D DMIN ->  1 S>D }T
T{  1 S>D  0 S>D DMIN ->  0 S>D }T
T{  1 S>D -1 S>D DMIN -> -1 S>D }T
T{  1 S>D  1 S>D DMIN ->  1 S>D }T
T{  0 S>D  1 S>D DMIN ->  0 S>D }T
T{  0 S>D -1 S>D DMIN -> -1 S>D }T
T{ -1 S>D  1 S>D DMIN -> -1 S>D }T
T{ -1 S>D -2 S>D DMIN -> -2 S>D }T
T{ MAX-D HALF-MAX-D DMIN -> HALF-MAX-D }T
T{ MAX-D MIN-D DMIN -> MIN-D }T
T{ MIN-D MAX-D DMIN -> MIN-D }T
T{ MIN-D HALF-MIN-D DMIN -> MIN-D }T
T{ MAX-2INT  1 S>D DMIN ->  1 S>D }T
T{ MAX-2INT -1 S>D DMIN -> -1 S>D }T
T{ MIN-2INT  1 S>D DMIN -> MIN-2INT }T
T{ MIN-2INT -1 S>D DMIN -> MIN-2INT }T
test_group_end

.( M+) test_group
T{ HI-2INT   1 M+ -> HI-2INT   1 S>D D+ }T
T{ MAX-2INT -1 M+ -> MAX-2INT -1 S>D D+ }T
T{ MIN-2INT  1 M+ -> MIN-2INT  1 S>D D+ }T
T{ LO-2INT  -1 M+ -> LO-2INT  -1 S>D D+ }T
test_group_end

rm_d0equal_words
