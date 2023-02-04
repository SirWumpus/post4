
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

.( VALUE TO ) test_group
T{ 111 VALUE tv_v1 -> }T
T{ -999 VALUE tv_v2 -> }T
T{ tv_v1 -> 111 }T
T{ tv_v2 -> -999 }T
T{ 222 TO tv_v1 -> }T
T{ tv_v1 -> 222 }T
T{ : tw_vd1 tv_v1 ; -> }T
T{ tw_vd1 -> 222 }T
T{ : tw_vd2 TO tv_v2 ; -> }T
T{ tv_v2 -> -999 }T
T{ -333 tw_vd2 -> }T
T{ tv_v2 -> -333 }T
T{ tv_v1 -> 222 }T
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

.( CELL+ /CELL ) test_group
T{ 0 CELL+ -> /CELL }T
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
t{ CREATE tw_create_empty -> }t
t{ ' tw_create_empty >BODY -> HERE }t
[DEFINED] >HERE [IF]
t{ >HERE -> 8 }t		\ CREATE reserves 1st data cell for DOES>
[THEN]
test_group_end

.( CREATE C, C@ C! >BODY ) test_group
T{ CREATE tv_char CHAR * C, -> }T
T{ ' tv_char >BODY -> tv_char }T
T{ tv_char C@ -> '*' }T
T{ '&' tv_char C! -> }T
T{ tv_char C@ -> CHAR & }T
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

.( <# # #> ) test_group
: tw_pattern_0 <# 1 0 # # #> S" 01" COMPARE ;
T{ tw_pattern_0 -> 0 }T
test_group_end

.( <# #S #> ) test_group
24 CONSTANT MAX-BASE
: tw_match COMPARE 0= ;
: tw_gp4 <# 1 0 #S #> S" 1" tw_match ;
T{ tw_gp4 -> TRUE }T
: tw_gp5
	BASE @ TRUE
	  MAX-BASE 1+ 2 DO		\ FOR EACH POSSIBLE BASE
	  I BASE ! 			\ TBD: ASSUMES BASE WORKS
	  I 0 <# #S #> S" 10" tw_match AND
	LOOP
	SWAP BASE !
;
T{ tw_gp5 -> TRUE }T
: tw_gp6
	BASE @ >R 2 BASE !
	MAX-U MAX-U <# #S #>		\ MAXIMUM UD TO BINARY
	R> BASE ! 			\ S: caddr u
	DUP cell-bits 2* = SWAP
	0 DO 				\ S: caddr flag
	  OVER C@ [CHAR] 1 = AND	\ ALL ONES
	  >R CHAR+ R>
	LOOP SWAP DROP
;
T{ tw_gp6 -> TRUE }T
: tw_gp7
	BASE @ >R MAX-BASE BASE !
	TRUE
	$A 0 DO
	  I 0 <# #S #>
	  1 = SWAP C@ I $30 + = AND AND
	LOOP
	MAX-BASE $A DO
	  I 0 <# #S #>
	  1 = SWAP C@ $41 I $A - + = AND AND
	LOOP
	R> BASE !
;
T{ tw_gp7 -> TRUE }T
test_group_end

.( <# HOLD #> ) test_group
: tw_gp1 <# $41 HOLD $42 HOLD 0 0 #> S" BA" COMPARE 0= ;
T{ tw_gp1 -> TRUE }T
test_group_end

.( <# HOLDS #> ) test_group
T{ 0 S>D <# S" Test" HOLDS #> S" Test" COMPARE -> 0 }T
test_group_end

.( <# SIGN #> ) test_group
: tw_gp1 <# -1 SIGN 0 SIGN -1 SIGN 0 0 #> S" --" COMPARE 0= ;
T{ tw_gp1 -> TRUE }T
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
: tw_word0 WORD COUNT SWAP C@ ;
T{ BL tw_word0 HELLO -> 5 CHAR H }T
T{ CHAR " tw_word0 GOODBYE" -> 7 CHAR G }T
T{ BL tw_word0
   \ Blank lines return zero-length strings
   DROP -> 0 }T
test_group_end

.( C" COUNT ) test_group
: tw_cquote_0 C" " ;
: tw_cquote_1 C" 123" ;
377 tw_cquote_0 COUNT EVALUATE 377 = assert
tw_cquote_1 COUNT EVALUATE 123 = assert
test_group_end


.( [ ] ) test_group
: tw_square_0 [ '@' ] LITERAL ;
tw_square_0 '@' = assert
test_group_end

.( ['] ) test_group
: tw_square_tick_value 1234 ;
: tw_square_tick_0 ['] tw_square_tick_value ; IMMEDIATE
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

.( >IN ) test_group
\ The original draft test suite assumes HEX.
HEX
VARIABLE tv_scans
: tw_rescan? -1 tv_scans +! tv_scans @ IF 0 >IN ! THEN ;

\ This test is sensitive to the layout.
T{   2 tv_scans !
   345 tw_rescan?
-> 345 345 }T

: tw_in1 5 tv_scans ! S" 123 tw_rescan?" EVALUATE ;
T{ tw_in1 -> 123 123 123 123 123 }T

\ These tests must start on a new line
DECIMAL
T{ 123456 DEPTH OVER 9 < 35 AND + 3 + >IN !
-> 123456 23456 3456 456 56 6 }T
T{ 14145 8115 ?DUP 0= 34 AND >IN +! TUCK MOD 14 >IN !
-> 15 }T
test_group_end

.( SOURCE >IN ) test_group
: tw_src0 S" SOURCE" 2DUP EVALUATE >R SWAP >R = R> R> = ;
T{ tw_src0 -> TRUE TRUE }T
: tw_src1 SOURCE >IN ! DROP ;
\ This test is sensitive to the layout.
T{ tw_src1 123 456
   -> }T
test_group_end

.( ( ) test_group
T{ 99
   ( 1 2 3
     4 5 6
     7 8 9 ) 11 22 33 -> 99 11 22 33 }T
test_group_end

.( SOURCE-ID ) test_group
T{ SOURCE-ID DUP -1 = SWAP 0= OR -> FALSE }T
test_group_end

.( INCLUDE INCLUDED ) test_group
T{ 0 INCLUDE ../test/data/increment_tos.p4 -> 1 }T
T{ 2 S" ../test/data/increment_tos.p4" INCLUDED  -> 3 }T
test_group_end

.( SAVE-INPUT RESTORE-INPUT ) test_group
\ Testing with a file source.
VARIABLE tv_si0 TRUE tv_si0 !

: tw_not_executed ." This should never be executed" ABORT ;

T{ 11111 SAVE-INPUT
	tv_si0 @
	[IF]
	  FALSE tv_si0 !
	  RESTORE-INPUT
	  tw_not_executed
	[ELSE]
	  \ Testing the ELSE part is executed
	  22222
	[THEN]
	\ FALSE comes from RESTORE-INPUT
	-> 11111 FALSE 22222
}T

\ Testing nesting.
VARIABLE tv_si_inc 0 tv_si_inc !
: tw_si1 tv_si_inc @ >IN +! 15 tv_si_inc ! ;
: tw_str S" SAVE-INPUT tw_si1 RESTORE-INPUT 12345" ;
T{ tw_str EVALUATE tv_si_inc @ -> 0 2345 15 }T
test_group_end

rm_core_words
