
INCLUDE ../test/assert.p4

MARKER rm_core_words

.( 0= ) test_group
t{  0 0= ->  TRUE }t
t{  1 0= -> FALSE }t
t{ -1 0= -> FALSE }t
test_group_end

.( 0< ) test_group
t{  0 0< -> FALSE }t
t{  1 0< -> FALSE }t
t{ -1 0< ->  TRUE }t
t{ -2 0< ->  TRUE }t
test_group_end

.( = ) test_group
t{  0  0 = ->  TRUE }t
t{  1  1 = ->  TRUE }t
t{ -1 -1 = ->  TRUE }t
t{  1  0 = -> FALSE }t
t{ -1  0 = -> FALSE }t
t{  0  1 = -> FALSE }t
t{  0 -1 = -> FALSE }t
test_group_end

.( FALSE ) test_group
t{ FALSE 0= -> TRUE }t
test_group_end

.( TRUE ) test_group
t{ TRUE -1 = -> TRUE }t
test_group_end

.( INVERT ) test_group
t{ FALSE INVERT -> TRUE }t
t{ TRUE INVERT -> FALSE }t
t{ 0 INVERT -> -1 }t
test_group_end

.( AND ) test_group
t{ 0 0 AND -> 0 }t
t{ 0 1 AND -> 0 }t
t{ 1 0 AND -> 0 }t
t{ 1 1 AND -> 1 }t
t{ %01 %10 AND -> 0 }t
t{ %10 %10 AND -> 2 }t
t{ %11 %10 AND -> 2 }t
t{ 0 INVERT 1 AND -> 1 }t
t{ 1 INVERT 1 AND -> 0 }t
test_group_end

.( OR ) test_group
t{ 0 0 OR -> 0 }t
t{ 0 1 OR -> 1 }t
t{ 1 0 OR -> 1 }t
t{ 1 1 OR -> 1 }t
t{ %01 %10 OR -> 3 }t
test_group_end

.( XOR ) test_group
t{ 0 0 XOR -> 0 }t
t{ 0 1 XOR -> 1 }t
t{ 1 0 XOR -> 1 }t
t{ 1 1 XOR -> 0 }t
t{ %01 %10 XOR -> 3 }t
t{ %11 %01 XOR -> 2 }t
test_group_end

.( LSHIFT ) test_group
T{ 1   0 LSHIFT -> 1 }T
T{ 1   1 LSHIFT -> 2 }T
T{ 1   2 LSHIFT -> 4 }T
T{ 1  $F LSHIFT -> $8000 }T
T{ 1S  1 LSHIFT -> 1 XOR 1S }T
T{ MSB 1 LSHIFT -> 0 }T
test_group_end

.( RSHIFT ) test_group
T{ 1   0 RSHIFT -> 1 }T
T{ 1   1 RSHIFT -> 0 }T
T{ 2   1 RSHIFT -> 1 }T
T{ 4   2 RSHIFT -> 1 }T
T{ $8000 $F RSHIFT -> 1 }T
T{ MSB 1 RSHIFT MSB AND -> 0 }T
T{ MSB 1 RSHIFT 2* -> MSB }T
test_group_end

.( Numeric notation ) test_group
t{ #1289 -> 1289 }t
\ t{ #12346789. -> 12346789. }t
t{ #-1289 -> -1289 }t
\ t{ #-12346789. -> -12346789. }t
t{ $12eF -> 4847 }t
\ t{ $12aBcDeF. -> 313249263. }t
t{ $-12eF -> -4847 }t
\ t{ $-12AbCdEf. -> -313249263. }t
t{ %10010110 -> 150 }t
\ t{ %10010110. -> 150. }t
t{ %-10010110 -> -150 }t
\ Post4 extensions.
t{ 0x12eF -> 4847 }t
t{ 0x-12eF -> -4847 }t
t{ 0377 -> 255 }t
t{ 0-377 -> -255 }t
t{ 0177777 -> 65535 }t
test_group_end

.( + ) test_group
t{ 0 0 + -> 0 }t
t{ 1 0 + -> 1 }t
t{ 0 1 + -> 1 }t
t{ 1 1 + -> 2 }t
t{ -1 1 + -> 0 }t
t{ 1 -1 + -> 0 }t
test_group_end

.( MAX-U ) test_group
T{ MAX-U -> 1S }T
T{ MAX-U -> -1 }T
test_group_end

.( MAX-N ) test_group
T{ MAX-N -> MAX-INT }T
T{ MAX-N -> 0 INVERT 1 RSHIFT }T
test_group_end

.( MIN-N ) test_group
T{ MIN-N -> MIN-INT }T
T{ MIN-N -> MAX-INT INVERT }T
test_group_end

.( BL ) test_group
t{ BL -> $20 }t
t{ BL -> '\s' }t
test_group_end

.( NEGATE ) test_group
T{  0 NEGATE ->  0 }T
T{  1 NEGATE -> -1 }T
T{ -1 NEGATE ->  1 }T
T{  2 NEGATE -> -2 }T
T{ -2 NEGATE ->  2 }T
T{ MAX-INT NEGATE -> MIN-INT 1 + }T
T{ MIN-INT 1 + NEGATE -> MAX-INT }T
test_group_end

.( - ) test_group
t{ 0  0 - ->  0 }t
t{ 1  0 - ->  1 }t
t{ 2  1 - ->  1 }t
t{ 0  1 - -> -1 }t
t{ 1 -1 - ->  2 }t
t{ test_group_end

.( * ) test_group
t{  0  3 * ->  0 }t
t{  1  3 * ->  3 }t
t{  2  3 * ->  6 }t
t{ -1 -1 * ->  1 }t
t{ -1 -3 * ->  3 }t
t{  2 -3 * -> -6 }t
test_group_end

.( 1+ ) test_group
t{  0 1+ -> 1 }t
t{ -1 1+ -> 0 }t
t{  1 1+ -> 2 }t
t{ MAX-N 1+ -> MAX-N 1 + }t
t{ MAX-N 1+ -> 0 INVERT 1 RSHIFT INVERT }t
test_group_end

.( 1- ) test_group
t{ 2 1- ->  1 }t
t{ 1 1- ->  0 }t
t{ 0 1- -> -1 }t
t{ MAX-N 1 + 1- -> MAX-N }t
test_group_end

.( U< ) test_group
t{ 0 1 U< -> TRUE }t
t{ 1 2 U< -> TRUE }t
t{ 0 MAX-N U< -> TRUE }t
t{ 0 MAX-U U< -> TRUE }t
t{ MAX-N MAX-U U< -> TRUE }t
t{ 0 0 U< -> FALSE }t
t{ 1 0 U< -> FALSE }t
t{ 1 1 U< -> FALSE }t
t{ 2 1 U< -> FALSE }t
t{ MAX-N 0 U< -> FALSE }t
t{ MAX-U 0 U< -> FALSE }t
t{ MAX-U MAX-N U< -> FALSE }t
test_group_end

.( < ) test_group
t{  0 1 < -> TRUE }t
t{  1 2 < -> TRUE }t
t{ -1 0 < -> TRUE }t
t{ -1 1 < -> TRUE }t
t{ MIN-N 0 < -> TRUE }t
t{ MIN-N MAX-N < -> TRUE }t
t{  0 MAX-N < -> TRUE }t
t{  0 0 < -> FALSE }t
t{  1 1 < -> FALSE }t
t{  1 0 < -> FALSE }t
t{  2 1 < -> FALSE }t
t{  0 -1 < -> FALSE }t
t{  1 -1 < -> FALSE }t
t{  0 MIN-N < -> FALSE }t
t{ MAX-N MIN-N < -> FALSE }t
t{ MAX-N 0 < -> FALSE }t
test_group_end

.( > ) test_group
t{  0 1 > -> FALSE }t
t{  1 2 > -> FALSE }t
t{ -1 0 > -> FALSE }t
t{ -1 1 > -> FALSE }t
t{ MIN-N 0 > -> FALSE }t
t{ MIN-N MAX-N > -> FALSE }t
t{  0 MAX-N > -> FALSE }t
t{  0 0 > -> FALSE }t
t{  1 1 > -> FALSE }t
t{  1 0 > -> TRUE }t
t{  2 1 > -> TRUE }t
t{  0 -1 > -> TRUE }t
t{  1 -1 > -> TRUE }t
t{  0 MIN-N > -> TRUE }t
t{ MAX-N MIN-N > -> TRUE }t
t{ MAX-N 0 > -> TRUE }t
test_group_end

.( WITHIN ) test_group
t{  0  0  0 WITHIN -> FALSE }t
t{  0  0  1 WITHIN -> TRUE  }t
t{  1  0  0 WITHIN -> FALSE }t
t{  1  0  1 WITHIN -> FALSE }t
t{  1  0  2 WITHIN -> TRUE  }t
t{ -1 -1  0 WITHIN -> TRUE  }t
t{ -2 -1  0 WITHIN -> FALSE }t
t{  0  0  MAX-N WITHIN -> TRUE }t
t{  0  0  MAX-U WITHIN -> TRUE }t
t{ MAX-N 1 - 0 MAX-N WITHIN -> TRUE }t
t{ MAX-U 1 - 0 MAX-U WITHIN -> TRUE }t
t{ MIN-N MIN-N 0 WITHIN -> TRUE }t
t{ MAX-N MIN-N MAX-N WITHIN FALSE }t
t{ MAX-N MIN-N MAX-N 1 + WITHIN FALSE }t
t{ MAX-N 0 MAX-U WITHIN -> TRUE }t
t{ MAX-U 0 MAX-U WITHIN -> FALSE }t
test_group_end

.( DROP ) test_group
t{ 12 34 DROP -> 12 }t
test_group_end

.( DUP ) test_group
t{ 12 DUP -> 12 12 }t
test_group_end

.( ?DUP ) test_group
t{ -1 ?DUP -> -1 -1 }t
t{  0 ?DUP -> 0 }t
t{  1 ?DUP -> 1 1 }t
test_group_end

.( OVER ) test_group
t{ 12 34 OVER -> 12 34 12 }t
test_group_end

.( SWAP ) test_group
t{ 12 34 SWAP -> 34 12 }t
test_group_end

.( ROT ) test_group
t{ 12 34 56 ROT -> 34 56 12 }t
test_group_end

.( >R R@ R> ) test_group
t{ 1 >R R> -> 1 }t
t{ 2 >R R@ R> -> 2 2 }t
t{ 3 4 >R >R R> R> -> 3 4 }t
t{ MAX-U >R R> -> MAX-U }t
test_group_end

.( DEPTH ) test_group
t{ DEPTH -> 0 }t
t{ 12 DEPTH -> 12 1 }t
t{ 12 34 DEPTH -> 12 34 2 }t
t{ 12 DEPTH >R DROP R> -> 1 }t
t{ 12 34 DEPTH >R 2DROP R> -> 2 }t
test_group_end

.( CONSTANT ) test_group
t{ 0xCAFE CONSTANT java java -> 0xCAFE }t
test_group_end

.( VARIABLE ! @ +! ) test_group
t{ VARIABLE tw_var_0 tw_var_0 @ -> 0 }t
t{ 0xCAFE tw_var_0 ! tw_var_0 @ -> 0xCAFE }t
t{ 0xDEAD tw_var_0 ! tw_var_0 @ -> 0xDEAD }t
t{ 0 tw_var_0 ! DEPTH -> 0 }t
t{ 1 tw_var_0 +! DEPTH -> 0 }t
t{ tw_var_0 @ -> 1 }t
t{ -1 tw_var_0 +! tw_var_0 @ -> 0 }t
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
t{ BASE @ >R -> }t
t{ HEX BASE @ -> #16 }t
t{ DECIMAL BASE @ -> #10 }t
[DEFINED] OCTAL [IF]
t{ OCTAL BASE @ -> #8 }t
[THEN]
[DEFINED] BINARY [IF]
t{ BINARY BASE @ -> #2 }t
[THEN]
t{ R> BASE ! -> }t
test_group_end

.( CHAR ) test_group
t{ CHAR @ -> $40 }t
t{ CHAR allo -> $61 }t
t{ 'a' -> $61 }t
test_group_end

.( [CHAR] ) test_group
t{ : tw_char_0 [CHAR] @ ; -> }t
t{ : tw_char_1 [CHAR] allo ; -> }t
t{ : tw_char_2 'a' ; -> }t
t{ tw_char_0 -> $40 }t
t{ tw_char_1 -> $61 }t
t{ tw_char_2 -> $61 }t
test_group_end

.( CHAR+ CHARS ) test_group
t{ 0 CHAR+ -> 1 CHARS }t
t{ PAD CHAR+ -> PAD 1 CHARS + }t
test_group_end

.( CELL+ /CELL CELLS ) test_group
T{ 0 CELL+ -> /CELL }T
T{ 0 CELL+ -> 1 CELLS }T
test_group_end

.( CHARS ) test_group
t{ 1 CHARS 1 < -> FALSE }t
t{ 1 CHARS 1 CELLS > -> FALSE }t
t{ 1 CHARS -> /CHAR }t
test_group_end

.( CELLS ) test_group
t{ 1 CELLS -> /CELL }t
t{ 1 CELLS 1 < -> FALSE }t		\ 1 <= AU
t{ 1 CELLS 1 CHARS MOD -> 0 }t		\ multiple of CHAR size
t{ 1S $FFFF U< -> FALSE }t		\ >= 16 bits
test_group_end

.( ' EXECUTE ) test_group
t{ : tw_tick_value 1234 ; -> }t
t{ ' tw_tick_value EXECUTE -> 1234 }t
test_group_end

.( :NONAME ) test_group
VARIABLE tw_var_1
VARIABLE tw_var_2
t{ :NONAME 1234 ; tw_var_1 ! -> }t
t{ :NONAME 9876 ; tw_var_2 ! -> }t
t{ tw_var_1 @ EXECUTE -> 1234 }t
t{ tw_var_2 @ EXECUTE -> 9876 }t

\ GH-4
\ https://github.com/ForthHub/discussion/discussions/159
\ https://rosettacode.org/wiki/Function_composition#Forth
\
t{ : tw_compose ( xt1 xt2 -- xt3 )
  >R >R :NONAME
    R> COMPILE,
    R> COMPILE,
    POSTPONE ;
; -> }t
t{ : tw_f1 5 + ; -> }t
t{ : tw_f2 20 - ; -> }t
t{ 10 ' tw_f1 ' tw_f2 tw_compose execute -> -5 }t
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
T{  0 ABS -> 0 }T
T{  1 ABS -> 1 }T
T{ -1 ABS -> 1 }T
T{ MIN-INT ABS -> MIN-INT }T
T{ MIN-INT 1 + ABS -> MAX-INT }T
test_group_end

.( MAX ) test_group
t{ 0 1 MAX -> 1 }t
t{ 1 2 MAX -> 2 }t
t{ -1 0 MAX -> 0 }t
t{ -1 1 MAX -> 1 }t
t{ MIN-N 0 MAX -> 0 }t
t{ MIN-N MAX-N MAX -> MAX-N }t
t{ 0 MAX-N MAX -> MAX-N }t
t{ 0 0 MAX -> 0 }t
t{ 1 1 MAX -> 1 }t
t{ 1 0 MAX -> 1 }t
t{ 2 1 MAX -> 2 }t
t{ 0 -1 MAX -> 0 }t
t{ 1 -1 MAX -> 1 }t
t{ 0 MIN-N MAX -> 0 }t
t{ MAX-N MIN-N MAX -> MAX-N }t
t{ MAX-N 0 MAX -> MAX-N }t
test_group_end

.( MIN ) test_group
t{ 0 1 MIN -> 0 }t
t{ 1 2 MIN -> 1 }t
t{ -1 0 MIN -> -1 }t
t{ -1 1 MIN -> -1 }t
t{ MIN-N 0 MIN -> MIN-N }t
t{ MIN-N MAX-N MIN -> MIN-N }t
t{ 0 MAX-N MIN -> 0 }t
t{ 0 0 MIN -> 0 }t
t{ 1 1 MIN -> 1 }t
t{ 1 0 MIN -> 0 }t
t{ 2 1 MIN -> 1 }t
t{ 0 -1 MIN -> -1 }t
t{ 1 -1 MIN -> -1 }t
t{ 0 MIN-N MIN -> MIN-N }t
t{ MIN-N MIN-N MIN -> MIN-N }t
t{ MIN-N 0 MIN -> MIN-N }t
test_group_end

.( IMMEDIATE ) test_group
T{ 123 CONSTANT tw_iw1 IMMEDIATE tw_iw1 -> 123 }T
T{ : tw_iw2 tw_iw1 LITERAL ; tw_iw2 -> 123 }T

T{ VARIABLE tw_iw3 IMMEDIATE 234 tw_iw3 ! tw_iw3 @ -> 234 }T
T{ : tw_iw4 tw_iw3 [ @ ] LITERAL ; tw_iw4 -> 234 }T

T{ :NONAME [ 345 ] tw_iw3 [ ! ] ; DROP tw_iw3 @ -> 345 }T
T{ CREATE tw_iw5 456 , IMMEDIATE -> }T
T{ :NONAME tw_iw5 [ @ tw_iw3 ! ] ; DROP tw_iw3 @ -> 456 }T

T{ : tw_iw6 CREATE , IMMEDIATE DOES> @ 1+ ; -> }T
T{ 111 tw_iw6 tw_iw7 tw_iw7 -> 112 }T
T{ : tw_iw8 tw_iw7 LITERAL 1+ ; tw_iw8 -> 113 }T

T{ : tw_iw9 CREATE , DOES> @ 2 + IMMEDIATE ; -> }T
T{ : tw_find_iw BL WORD FIND NIP ; -> }T
T{ 222 tw_iw9 tw_iw10 tw_find_iw tw_iw10 -> -1 }T	\ tw_iw10 is not immediate
T{ tw_iw10 tw_find_iw tw_iw10 -> 224 1 }T		\ tw_iw10 becomes immediate
test_group_end

.( S" S\\" EVALUATE ) test_group
: tw_eval_0 EVALUATE ;
: tw_eval_1 S" 9876" EVALUATE ;

t{ S" 123" EVALUATE -> 123 }t
t{ S\" 123\n432" EVALUATE -> 123 432 }t
t{ S" 456" tw_eval_0 -> 456 }t
t{ tw_eval_1 -> 9876 }t

\ See F.6.1.1360
: tw_eval_2 S" 123" ; IMMEDIATE
: tw_eval_3 S" 123 1+" ; IMMEDIATE
: tw_eval_4 S" : tw_eval_5 345 ;" ;
: tw_eval_6 EVALUATE ; IMMEDIATE

\ Test evaluate in interpreter state.
t{ tw_eval_2 EVALUATE -> 123 }t
t{ tw_eval_3 EVALUATE -> 124 }t
t{ tw_eval_4 EVALUATE ->     }t
t{ tw_eval_5          -> 345 }t

\ Test evaluate in compiler state.
t{ : tw_eval_7 tw_eval_2 tw_eval_6 ; -> }t
t{   tw_eval_7        -> 123 }t
t{ : tw_eval_8 tw_eval_3 tw_eval_6 ; -> }t
t{   tw_eval_8        -> 124 }t
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
t{ 377  0 tw_ifthen_0 -> 377 }t
t{ 377  1 tw_ifthen_0 -> 377 123 }t
t{ 377 -1 tw_ifthen_0 -> 377 123 }t
t{  0 tw_ifthen_1 -> 345 }t
t{  1 tw_ifthen_1 -> 234 }t
t{ -1 tw_ifthen_1 -> 234 }t
t{ FALSE tw_ifthen_2 -> 2 4 }t
t{ TRUE  tw_ifthen_2 -> 1 3 5 }t
test_group_end

.( EXIT ) test_group
: tw_exit_0 IF 123 EXIT THEN 456 ;
t{ FALSE tw_exit_0 -> 456 }t
t{ TRUE tw_exit_0  -> 123 }t
test_group_end

.( PAD C@ C! FILL ) test_group
t{ '!' PAD C! -> }t
t{ PAD C@ -> '!' }t
t{ PAD 0 '#' FILL PAD C@ '#' <> -> TRUE }t
t{ PAD 1 '$' FILL PAD C@ -> '$' }t
t{ PAD 2 '%' FILL PAD C@ PAD CHAR+ C@ -> '%' '%' }t
\ Fill the whole PAD
t{ PAD /PAD '&' FILL -> }t
\ Check last char in buffer is set and no overflow.
PAD /PAD 1 - + DUP C@ '&' = assert CHAR+ C@ '&' <> assert
test_group_end

.( SOURCE ) test_group
: tw_source_0 S" SOURCE" 2DUP EVALUATE >R SWAP >R = R> R> = ;
t{ tw_source_0 -> TRUE TRUE }t
: tw_source_1 SOURCE >IN ! DROP ;
t{ tw_source_1 123 456 -> }t
test_group_end

.( STATE ) test_group
: tw_state_0 STATE @ ; IMMEDIATE
: tw_state_1 tw_state_0 LITERAL ;
t{ tw_state_0 -> FALSE }t
t{ tw_state_1 -> TRUE }t
test_group_end

.( UNTIL ) test_group
: tw_until_0 BEGIN DUP 1+ DUP 5 > UNTIL ;
t{ 3 tw_until_0 -> 3 4 5 6 }t
t{ 5 tw_until_0 -> 5 6 }t
t{ 6 tw_until_0 -> 6 7 }t
test_group_end

.( WHILE ) test_group
: tw_while_0 BEGIN DUP 3 < WHILE DUP 1+ REPEAT ;
t{ 0 tw_while_0 -> 0 1 2 3 }t
t{ 2 tw_while_0 -> 2 3 }t
t{ 3 tw_while_0 -> 3 }t
t{ 4 tw_while_0 -> 4 }t
: tw_while_1 BEGIN DUP 2 > WHILE DUP 5 < WHILE DUP 1+ REPEAT 123 ELSE 345 THEN ;
t{ 1 tw_while_1 -> 1 345 }t
t{ 2 tw_while_1 -> 2 345 }t
t{ 3 tw_while_1 -> 3 4 5 123 }t
t{ 4 tw_while_1 -> 4 5 123 }t
t{ 5 tw_while_1 -> 5 123 }t
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
t{ 377 tw_cquote_0 COUNT EVALUATE -> 377 }t
t{ tw_cquote_1 COUNT EVALUATE -> 123 }t
test_group_end

.( [ ] ) test_group
: tw_square_0 [ '@' ] LITERAL ;
t{ tw_square_0 -> '@' }t
test_group_end

.( ['] ) test_group
: tw_square_tick_value 1234 ;
: tw_square_tick_0 ['] tw_square_tick_value ; IMMEDIATE
t{ tw_square_tick_0 EXECUTE -> 1234 }t
test_group_end

.( ( ) test_group
: tw_paren_0 ( A comment)1234 ;		\ There is no space either side of the ).
t{ ( A comment)1234			\ There is no space either side of the ).
   -> tw_paren_0 }t
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

.( RECURSE ) test_group
\ Regular word defintion.
T{ : tw_recurse0 ( N -- 0, 1, ... N )
	DUP IF DUP >R 1- RECURSE R> THEN
; -> }T
T{ 0 tw_recurse0 -> 0 }T
T{ 1 tw_recurse0 -> 0 1 }T
T{ 2 tw_recurse0 -> 0 1 2 }T
T{ 3 tw_recurse0 -> 0 1 2 3 }T
T{ 4 tw_recurse0 -> 0 1 2 3 4 }T

\ Noname word defintion.
T{ :NONAME ( n -- 0, 1, .., n )
	DUP IF DUP >R 1- RECURSE R> THEN
; CONSTANT tw_recurse1 -> }T
T{ 0 tw_recurse1 EXECUTE -> 0 }T
T{ 4 tw_recurse1 EXECUTE -> 0 1 2 3 4 }T

:NONAME ( n -- n1 )
	1- DUP
	CASE 0 OF EXIT ENDOF
	  1 OF 11 SWAP RECURSE ENDOF
	  2 OF 22 SWAP RECURSE ENDOF
	  3 OF 33 SWAP RECURSE ENDOF
	  DROP ABS RECURSE EXIT
	ENDCASE
; CONSTANT tw_recurse2
T{ 1 tw_recurse2 EXECUTE -> 0 }T
T{ 2 tw_recurse2 EXECUTE -> 11 0 }T
T{ 4 tw_recurse2 EXECUTE -> 33 22 11 0 }T
T{ 25 tw_recurse2 EXECUTE -> 33 22 11 0 }T
test_group_end

rm_core_words
