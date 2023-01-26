
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

.( LSHIFT ) test_group
1 0 LSHIFT 1 = assert
1 1 LSHIFT 2 = assert
1 2 LSHIFT 4 = assert
1 $F LSHIFT $8000 = assert
-1 1 LSHIFT 1 XOR -1 = assert
MIN-N~ 1 LSHIFT 0 = assert
test_group_end

.( RSHIFT ) test_group
1 0 RSHIFT 1 = assert
1 1 RSHIFT 0 = assert
2 1 RSHIFT 1 = assert
4 2 RSHIFT 1 = assert
$8000 $F RSHIFT 1 = assert
MIN-N~ 1 RSHIFT MIN-N~ AND 0 = assert
MIN-N~ 1 RSHIFT 2* MIN-N~ = assert
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

.( MIN-N ) test_group
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

.( 1+ ) test_group
 0 1+ 1 = assert
-1 1+ 0 = assert
 1 1+ 2 = assert
MAX-N 1+ MAX-N 1 + = assert
MAX-N 1+ 0 INVERT 1 RSHIFT INVERT = assert
test_group_end

.( 1- ) test_group
2 1-  1 = assert
1 1-  0 = assert
0 1- -1 = assert
MAX-N 1 + 1- MAX-N = assert
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
DEPTH 0 = assert
1 DEPTH >R DROP R> 1 = assert
1 2 DEPTH  >R 2DROP R> 2 = assert
test_group_end

.( CONSTANT ) test_group
0xCAFE CONSTANT java java 0xCAFE = assert
test_group_end

.( VARIABLE ! @ +! ) test_group
VARIABLE tw_var_0 tw_var_0 @ 0= assert
0xCAFE tw_var_0 ! tw_var_0 @ 0xCAFE = assert
0xDEAD tw_var_0 ! tw_var_0 @ 0xCAFE = assert_not
tw_var_0 @ 0xDEAD = assert
0 tw_var_0 ! DEPTH 0= assert
1 tw_var_0 +! DEPTH 0= assert
tw_var_0 @ 1 = assert
-1 tw_var_0 +! tw_var_0 @ 0= assert
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

.( [CHAR] ) test_group
: tw_char_0 [CHAR] @ ;
: tw_char_1 [CHAR] allo ;
: tw_char_2 'a' ;
tw_char_0 $40 = assert
tw_char_1 $61 = assert
tw_char_2 $61 = assert
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
: tw_tick_value 1234 ;
' tw_tick_value EXECUTE 1234 = assert
test_group_end

.( :NONAME ) test_group
VARIABLE tw_var_1
VARIABLE tw_var_2
:NONAME 1234 ; tw_var_1 !
:NONAME 9876 ; tw_var_2 !
tw_var_1 @ EXECUTE 1234 = assert
tw_var_2 @ EXECUTE 9876 = assert
test_group_end

.( COMPILE, ) test_group
t{ :NONAME DUP + ; CONSTANT tw_compile_0 -> }t
t{ : tw_compile_1 tw_compile_0 COMPILE, ; -> }t
t{ : tw_compile_2 [ tw_compile_1 ] ; -> }t
t{ 123 tw_compile_2 -> 246 }t
test_group_end

.( CREATE >BODY HERE ) test_group
CREATE tw_create_empty
' tw_create_empty >BODY HERE = assert
[DEFINED] >HERE [IF]
>HERE 0 = assert
[THEN]
test_group_end

.( DEFER DEFER! DEFER@ IS ACTION-OF ) test_group
t{ DEFER tw_defer_0 -> }t
t{ : tw_actionof_0 ACTION-OF tw_defer_0 ; -> }t
t{ ' * ' tw_defer_0 DEFER! -> }t
t{ 2 3 tw_defer_0 -> 6 }t
t{ ' tw_defer_0 DEFER@ -> ' * }t
t{ ACTION-OF tw_defer_0 -> ' * }t
t{ tw_actionof_0 -> ' * }t
t{ ' + IS tw_defer_0 -> }t
t{ 1 2 tw_defer_0 -> 3 }t
t{ ' tw_defer_0 DEFER@ -> ' + }t
t{ ACTION-OF tw_defer_0 -> ' + }t
t{ tw_actionof_0 -> ' + }t
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
: tw_eval_0 EVALUATE ;
: tw_eval_1 S" 9876" EVALUATE ;
S" 123" EVALUATE 123 = assert
S\" 123\n432" EVALUATE 123 432 D= assert
S" 456" tw_eval_0 456 = assert
tw_eval_1 9876 = assert
test_group_end

.( IF ELSE THEN ) test_group
: tw_ifthen_0 IF 123 THEN ;
: tw_ifthen_1 IF 234 ELSE 345 THEN ;
: tw_ifthen_2 IF 1 ELSE 2 ELSE 3 ELSE 4 ELSE 5 THEN ;
377  0 tw_ifthen_0 377 = assert
377  1 tw_ifthen_0 123 = assert 377 = assert
377 -1 tw_ifthen_0 123 = assert 377 = assert
 0 tw_ifthen_1 345 = assert
 1 tw_ifthen_1 234 = assert
-1 tw_ifthen_1 234 = assert
FALSE tw_ifthen_2 4 = assert 2 = assert
TRUE  tw_ifthen_2 5 = assert 3 = assert 1 = assert
test_group_end

.( EXIT ) test_group
: tw_exit_0 IF 123 EXIT THEN 456 ;
FALSE tw_exit_0 456 = assert
TRUE tw_exit_0 123 = assert
test_group_end

.( PAD C@ C! FILL ) test_group
'!' PAD C! DEPTH 0= assert
PAD C@ '!' = assert
PAD 0 '#' FILL PAD C@ '#' <> assert
PAD 1 '$' FILL PAD C@ '$' = assert
PAD 2 '%' FILL PAD C@ '%' = assert PAD CHAR+ C@ '%' = assert
\ Fill the whole PAD
PAD /PAD '&' FILL
\ Check last char in buffer is set and no overflow.
PAD /PAD 1 - + DUP C@ '&' = assert CHAR+ C@ '&' <> assert
test_group_end

.( SOURCE ) test_group
: tw_source_0 S" SOURCE" 2DUP EVALUATE >R SWAP >R = R> R> = ;
tw_source_0 assert assert
test_group_end

.( STATE ) test_group
: tw_state_0 STATE @ ; IMMEDIATE
: tw_state_1 tw_state_0 LITERAL ;
tw_state_0 0= assert
tw_state_1 0= assert_not
test_group_end

.( UNTIL ) test_group
: tw_until_0 BEGIN DUP 1+ DUP 5 > UNTIL ;
3 tw_until_0 5 6 D= assert 3 4 D= assert
5 tw_until_0 5 6 D= assert
6 tw_until_0 6 7 D= assert
test_group_end

.( WHILE ) test_group
: tw_while_0 BEGIN DUP 3 < WHILE DUP 1+ REPEAT ;
0 tw_while_0 2 3 D= assert 0 1 D= assert
2 tw_while_0 2 3 D= assert
3 tw_while_0 3 = assert
4 tw_while_0 4 = assert
: tw_while_1 BEGIN DUP 2 > WHILE DUP 5 < WHILE DUP 1+ REPEAT 123 ELSE 345 THEN ;
1 tw_while_1 1 345 D= assert
2 tw_while_1 2 345 D= assert
3 tw_while_1 5 123 D= assert 3 4 D= assert
4 tw_while_1 5 123 D= assert 4 = assert
5 tw_while_1 5 123 D= assert
test_group_end

.( WORD ) test_group
: tw_word_0 WORD COUNT SWAP C@ ;
BL tw_word_0 HELLO 5 CHAR H D= assert
CHAR " tw_word_0 GOODBYE" 7 CHAR G D= assert
BL tw_word_0
	\ Test case split by newline, blank lines return zero-length strings.
	DROP 0= assert
test_group_end

.( C" COUNT ) test_group
: tw_cquote_0 C" " ;
: tw_cquote_1 C" 123" ;
377 tw_cquote_0 COUNT EVALUATE 377 = assert
tw_cquote_1 COUNT EVALUATE 123 = assert
test_group_end


.( [ ] ) test_group
: tw_square_0 [ tw_char_0 ] LITERAL ;
tw_square_0 '@' = assert
test_group_end

.( ['] ) test_group
: tw_square_tick_0 ['] tw_tick_value ; IMMEDIATE
tw_square_tick_0 EXECUTE 1234 = assert
test_group_end

.( ( ) test_group
: tw_paren_0 ( A comment)1234 ;		\ There is no space either side of the ).
( A comment)1234			\ There is no space either side of the ).
tw_paren_0 = assert
test_group_end

.( BUFFER: ) test_group
t{ 127 CHARS BUFFER: tw_buf_0 -> }t
t{ 127 CHARS BUFFER: tw_buf_1 -> }t

\ Buffer is aligned
t{ tw_buf_0 ALIGNED -> tw_buf_0 }t

\ Buffers do not overlap
t{ tw_buf_1 tw_buf_0 - ABS 127 CHARS < -> FALSE }t

\ Buffer can be written to
: tw_buf_full? ( c-addr n char -- flag )
	TRUE 2SWAP CHARS OVER + SWAP ?DO
	OVER I C@ = AND
	/CHAR +LOOP NIP
;

t{ tw_buf_0 127 CHAR * FILL -> }t
t{ tw_buf_0 127 CHAR * tw_buf_full? -> TRUE }t
t{ tw_buf_0 127 0 FILL -> }t
t{ tw_buf_0 127 0 tw_buf_full? -> TRUE }t
test_group_end

.( CASE OF ENDOF ENDCASE ) test_group
: tw_case_0
	CASE
	1 OF 111 ENDOF
	2 OF 222 ENDOF
	3 OF 333 ENDOF
	>R 999 R>
	ENDCASE
;
T{ 1 tw_case_0 -> 111 }T
T{ 2 tw_case_0 -> 222 }T
T{ 3 tw_case_0 -> 333 }T
T{ 4 tw_case_0 -> 999 }T
: tw_case_1 ( case1 case2 -- )
	>R
	CASE
	  -1 OF
	    CASE
	      R@
	      1 OF 100 ENDOF
	      2 OF 200 ENDOF
	      >R -300 R>
	    ENDCASE
	  ENDOF
	  -2 OF
	    CASE
	      R@
	      1 OF -99 ENDOF
	      >R -199 R>
	    ENDCASE
	  ENDOF
	  >R 299 R>
	ENDCASE
	R> DROP
;
T{ -1 1 tw_case_1 ->  100 }T
T{ -1 2 tw_case_1 ->  200 }T
T{ -1 3 tw_case_1 -> -300 }T
T{ -2 1 tw_case_1 -> -99 }T
T{ -2 2 tw_case_1 -> -199 }T
T{  0 2 tw_case_1 ->  299 }T
test_group_end

.( strcmp ) test_group
: tw_0 S" " ;
: tw_a S" A" ;
: tw_b S" B" ;
: tw_z S" Z" ;
: tw_ab S" AB" ;
: tw_az S" AZ" ;
: tw_abc S" ABC" ;
T{ tw_0 tw_0 strcmp -> 0 }T
T{ tw_0 tw_a strcmp -> -1 }T
T{ tw_a tw_0 strcmp ->  1 }T
T{ tw_0 tw_z strcmp -> -1 }T
T{ tw_z tw_0 strcmp ->  1 }T
T{ tw_a tw_a strcmp -> 0 }T
T{ tw_b tw_b strcmp -> 0 }T
T{ tw_ab tw_ab strcmp -> 0 }T
T{ tw_abc tw_abc strcmp -> 0 }T
T{ tw_a tw_b strcmp -> -1 }T
T{ tw_b tw_a strcmp ->  1 }T
T{ tw_a tw_z strcmp -> -1 }T
T{ tw_z tw_a strcmp ->  1 }T
T{ tw_a tw_ab strcmp -> -1 }T
T{ tw_ab tw_a strcmp ->  1 }T
T{ tw_a tw_abc strcmp -> -1 }T
T{ tw_abc tw_a strcmp ->  1 }T
T{ tw_ab tw_abc strcmp -> -1 }T
T{ tw_abc tw_ab strcmp ->  1 }T
T{ tw_z tw_ab strcmp ->   1 }T
T{ tw_ab tw_z strcmp ->  -1 }T
T{ tw_az tw_ab strcmp ->  1 }T
T{ tw_ab tw_az strcmp -> -1 }T
T{ tw_z tw_abc strcmp ->  1 }T
T{ tw_abc tw_z strcmp -> -1 }T
T{ tw_az tw_abc strcmp ->  1 }T
T{ tw_abc tw_az strcmp -> -1 }T
test_group_end

.( strrev ) test_group
: tw_ba S" BA" ;
: tw_cba S" CBA" ;
T{ tw_0 strrev tw_0 tw_0 strcmp -> 0 }T
T{ tw_a strrev tw_a tw_a strcmp -> 0 }T
T{ tw_ab strrev tw_ab tw_ba strcmp -> 0 }T
T{ tw_abc strrev tw_abc tw_cba strcmp -> 0 }T
T{ tw_ab strrev tw_ab tw_ba strcmp -> -1 }T	\ revert tw_ab
T{ tw_abc strrev tw_abc tw_cba strcmp -> -1 }T	\ revert tw_abc
test_group_end

.( COMPARE ) test_group
: tw_str_0 S" abcdefghijklmnopqrstuvwxyz" ;
: tw_str_1 S" 12345" ;
: tw_str_2 S" 0abc" ;
: tw_str_3 S" 0aBc" ;
T{ tw_str_0 tw_str_0 COMPARE -> 0 }T
T{ tw_str_0 PAD SWAP CMOVE -> }T \ Copy tw_str_0 to PAD
T{ tw_str_0 PAD OVER COMPARE -> 0 }T
T{ tw_str_0 PAD 6 COMPARE -> 1 }T
T{ PAD 10 tw_str_0 COMPARE -> -1 }T
T{ tw_str_0 PAD 0 COMPARE -> 1 }T
T{ PAD 0 tw_str_0 COMPARE -> -1 }T
T{ tw_str_0 tw_str_1 COMPARE -> 1 }T
T{ tw_str_1 tw_str_0 COMPARE -> -1 }T
: "abdde" S" abdde" ;
: "abbde" S" abbde" ;
: "abcdf" S" abcdf" ;
: "abcdee" S" abcdee" ;
T{ tw_str_0 "abdde"  COMPARE -> -1 }T
T{ tw_str_0 "abbde"  COMPARE ->  1 }T
T{ tw_str_0 "abcdf"  COMPARE -> -1 }T
T{ tw_str_0 "abcdee" COMPARE ->  1 }T
T{ tw_str_2 tw_str_3 COMPARE ->  1 }T
T{ tw_str_3 tw_str_2 COMPARE -> -1 }T
test_group_end

.( starts-with ) test_group
T{ s" " s" " starts-with -> TRUE }T
T{ s" " s" foo" starts-with -> FALSE }T
T{ s" hello" s" bar" starts-with -> FALSE }T
T{ s" hello" s" hel" starts-with -> TRUE }T
test_group_end

.( BLANK ) test_group
T{ PAD 25 CHAR a FILL -> }T		\ Fill PAD with 25 'a's
T{ PAD 5 CHARS + 6 BLANK -> }T		\ Put 6 spaced from character 5
T{ PAD 12 S" aaaaa      a" COMPARE -> 0 }T
test_group_end

.( /STRING ) test_group
: tw_str_0 S" abcdefghijklmnopqrstuvwxyz" ;
T{ tw_str_0 5 /STRING -> tw_str_0 SWAP 5 + SWAP 5 - }T
T{ tw_str_0 10 /STRING -4 /STRING -> tw_str_0 6 /STRING }T
T{ tw_str_0 0 /STRING -> tw_str_0 }T
test_group_end

.( -TRAILING ) test_group
: tw_str_0 S" " ;
: tw_str_1 S" abcdefghijklmnopqrstuvwxyz" ;
: tw_str_2 S" abc  " ;
: tw_str_3 S"      " ;
: tw_str_4 S"    a " ;
T{ tw_str_0 -TRAILING -> tw_str_0 }T
T{ tw_str_2 -TRAILING -> tw_str_2 2 - }T
T{ tw_str_0 -TRAILING -> tw_str_0 }T
T{ tw_str_3 -TRAILING -> tw_str_3 DROP 0 }T
T{ tw_str_4 -TRAILING -> tw_str_4 1- }T
test_group_end

.( SEARCH ) test_group
: tw_str_0 S" " ;
: tw_str_1 S" abcdefghijklmnopqrstuvwxyz" ;
: tw_str_2 S" abc" ;
: tw_str_3 S" jklmn" ;
: tw_str_4 S" z" ;
: tw_str_5 S" mnoq" ;
: tw_str_6 S" 12345" ;
T{ tw_str_1 tw_str_0 SEARCH -> tw_str_1 TRUE }T
T{ tw_str_1 tw_str_2 SEARCH -> tw_str_1 TRUE }T
T{ tw_str_1 tw_str_3 SEARCH -> tw_str_1 9 /STRING TRUE }T
T{ tw_str_1 tw_str_4 SEARCH -> tw_str_1 25 /STRING TRUE }T
T{ tw_str_1 tw_str_5 SEARCH -> tw_str_1 FALSE }T
T{ tw_str_1 tw_str_6 SEARCH -> tw_str_1 FALSE }T
test_group_end

.( SLITERAL ) test_group
: tw_str_0 S" abcdefghijklmnopqrstuvwxyz" ;
: tw_str_1 [ tw_str_0 ] SLITERAL ;
T{ tw_str_0 tw_str_1 COMPARE -> 0 }T
T{ tw_str_0 tw_str_1 ROT = ROT ROT = -> TRUE FALSE }T
test_group_end

.( PARSE-NAME ) test_group
T{ PARSE-NAME abcd S" abcd" COMPARE 0= -> TRUE }T
T{ PARSE-NAME   abcde S" abcde" COMPARE 0= -> TRUE }T
\ test empty parse area
T{ PARSE-NAME
	NIP -> 0 }T	\ empty line
T{ PARSE-NAME
	NIP -> 0 }T	\ line with white space
T{ : tw_parse_name_0 ( "name1" "name2" -- n )
	PARSE-NAME PARSE-NAME COMPARE 0= ; -> }T
T{ tw_parse_name_0 abcd abcd -> TRUE }T
T{ tw_parse_name_0   abcd abcd -> TRUE }T
T{ tw_parse_name_0 abcde abcdf -> FALSE }T
T{ tw_parse_name_0 abcdf abcde -> FALSE }T
T{ tw_parse_name_0 abcde abcde
	-> TRUE }T
T{ tw_parse_name_0 abcde abcde
	-> TRUE }T	\ line with white space
test_group_end

rm_core_words
