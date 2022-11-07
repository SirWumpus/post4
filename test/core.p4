
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
 0  0 = assert
 1  1 = assert
-1 -1 = assert
 1  0 = assert_not
-1  0 = assert_not
 0  1 = assert_not
 0 -1 = assert_not
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

.( MAX-U ) test_group
MAX-U -1 = assert
test_group_end

.( MAX-N ) test_group
MAX-N 0 INVERT 1 RSHIFT = assert
test_group_end

.( MN-N ) test_group
MIN-N MAX-N INVERT 1 + = assert
test_group_end

.( BL ) test_group
BL $20 = assert
BL '\s' = assert
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

.( * ) test_group
0 3 * 0 = assert
1 3 * 3 = assert
2 3 * 6 = assert
-1 -1 * 1 = assert
-1 -3 * 3 = assert
 2 -3 * -6 = assert
test_group_end

.( U< ) test_group
0 1 U< assert
1 2 U< assert
0 MAX-N U< assert
0 MAX-U U< assert
MAX-N MAX-U U< assert
0 0 U< assert_not
1 0 U< assert_not
1 1 U< assert_not
2 1 U< assert_not
MAX-N 0 U< assert_not
MAX-U 0 U< assert_not
MAX-U MAX-N U< assert_not
test_group_end

.( < ) test_group
 0 1 < assert
 1 2 < assert
-1 0 < assert
-1 1 < assert
MIN-N 0 < assert
MIN-N MAX-N < assert
 0 MAX-N < assert
 0 0 < assert_not
 1 1 < assert_not
 1 0 < assert_not
 2 1 < assert_not
 0 -1 < assert_not
 1 -1 < assert_not
 0 MIN-N < assert_not
MAX-N MIN-N < assert_not
MAX-N 0 < assert_not
test_group_end

.( > ) test_group
 0 1 > assert_not
 1 2 > assert_not
-1 0 > assert_not
-1 1 > assert_not
MIN-N 0 > assert_not
MIN-N MAX-N > assert_not
 0 MAX-N > assert_not
 0 0 > assert_not
 1 1 > assert_not
 1 0 > assert
 2 1 > assert
 0 -1 > assert
 1 -1 > assert
 0 MIN-N > assert
MAX-N MIN-N > assert
MAX-N 0 > assert
test_group_end

.( DROP ) test_group
12 34 DROP 12 = assert
test_group_end

.( DUP ) test_group
12 DUP = assert
test_group_end

.( ?DUP ) test_group
-1 ?DUP = assert
0 ?DUP 0= assert
1 ?DUP = assert
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

.( >R R@ R> ) test_group
1 >R R> 1 = assert
2 >R R@ R> 2 = assert 2 = assert
3 4 >R >R R> R> 4 = assert 3 = assert
MAX-U >R R> MAX-U = assert
test_group_end

.( DEPTH ) test_group
\ Stack might not be empty, so compare DEPTH before / after.
DEPTH DEPTH - -1 = assert
DEPTH 0 DEPTH >R DROP R> - -2 = assert
DEPTH 0 1 DEPTH  >R DROP DROP R> - -3 = assert
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

.( BASE HEX DECIMAL ) test_group
BASE @ >R
HEX BASE @ #16 = assert
DECIMAL BASE @ #10 = assert
[DEFINED] OCTAL [IF]
OCTAL BASE @ #8 = assert
BINARY BASE @ #2 = assert
[THEN]
R> BASE !
test_group_end

.( CHAR ) test_group
CHAR @ $40 = assert
CHAR allo $61 = assert
'a' $61 = assert
test_group_end

.( CHAR+ ) test_group
0 CHAR+ 1 = assert
test_group_end

.( CELL+ ) test_group
test_group_end

.( CHARS ) test_group
1 CHARS 1 < assert_not
1 CHARS 1 CELLS > assert_not
1 CHARS 1 = assert
test_group_end

.( CELLS ) test_group
1 CELLS 1 < assert_not
1 CELLS 1 CHARS MOD 0= assert
test_group_end

.( ' EXECUTE ) test_group
: tick_value 1234 ;
' tick_value EXECUTE 1234 = assert
test_group_end

.( CREATE >BODY HERE ) test_group
CREATE create_empty
' create_empty >BODY HERE = assert
[DEFINED] >HERE [IF]
>HERE 0 = assert
[THEN]
test_group_end

.( ABS ) test_group
 0 ABS 0 = assert
 1 ABS 1 = assert
-1 ABS 1 = assert
MIN-N ABS MAX-N = assert
test_group_end

.( MAX ) test_group
0 1 MAX 1 = assert
1 2 MAX 2 = assert
-1 0 MAX 0 = assert
-1 1 MAX 1 = assert
MIN-N 0 MAX 0 = assert
MIN-N MAX-N MAX MAX-N = assert
0 MAX-N MAX MAX-N = assert
0 0 MAX 0 = assert
1 1 MAX 1 = assert
1 0 MAX 1 = assert
2 1 MAX 2 = assert
0 -1 MAX 0 = assert
1 -1 MAX 1 = assert
0 MIN-N MAX 0 = assert
MAX-N MIN-N MAX MAX-N = assert
MAX-N 0 MAX MAX-N = assert
test_group_end

.( MIN ) test_group
0 1 MIN 0 = assert
1 2 MIN 1 = assert
-1 0 MIN -1 = assert
-1 1 MIN -1 = assert
MIN-N 0 MIN MIN-N = assert
MIN-N MAX-N MIN MIN-N = assert
0 MAX-N MIN 0 = assert
0 0 MIN 0 = assert
1 1 MIN 1 = assert
1 0 MIN 0 = assert
2 1 MIN 1 = assert
0 -1 MIN -1 = assert
1 -1 MIN -1 = assert
0 MIN-N MIN MIN-N = assert
MIN-N MIN-N MIN MIN-N = assert
MIN-N 0 MIN MIN-N = assert
test_group_end

.( S" S\\" EVALUATE ) test_group
: eval_0 EVALUATE ;
: eval_1 S" 9876" EVALUATE ;
S" 123" EVALUATE 123 = assert
S\" 123\n432" EVALUATE 123 432 D= assert
S" 456" eval_0 456 = assert
eval_1 9876 = assert
test_group_end

.( EXIT ) test_group
: exit_0 IF 123 EXIT THEN 456 ;
FALSE exit_0 456 = assert
TRUE exit_0 123 = assert
test_group_end

.( PAD FILL ) test_group
PAD 0 $23 FILL PAD C@ $23 <> assert
PAD 1 $24 FILL PAD C@ $24 = assert
PAD 2 $25 FILL PAD C@ $25 = assert PAD CHAR+ C@ $25 = assert
\ Fill the whole PAD
PAD /PAD $26 FILL
\ Check last char in buffer is set and no overflow.
PAD /PAD 1 - + DUP C@ $26 = assert CHAR+ C@ $26 <> assert
test_group_end

rm_core_words
