MARKER rm_core_words

: \ '\n' 0 _parse DROP DROP ; IMMEDIATE

\ Post4 Copyright 2007, 2023 by Anthony Howe.  All rights reserved.

\ ... BYE ...
\
\ ( -- )
\
: BYE 0 bye-code ;

\ ... ABORT ...
\
\  ( i*x -- ) ( R: j*x -- )
\
: ABORT -1 _longjmp ;

\ ... QUIT ...
\
\  ( -- ) ( R: i*x -- )
\
\ See https://github.com/ForthHub/discussion/discussions/116#discussioncomment-3541822
\
\ Expected standard behaviour:
\
\ 1.	ok :noname 123 [: 456 -56 throw ;] catch . . ;  execute
\	-56 123 ok
\
\ 2.	ok : foo [: 123 quit ;] catch 456 . throw ;  foo
\	ok .s
\	123
\	ok
\
: QUIT -56 _longjmp ;

\ ... .S ...
\
\ ( -- )
\
: .S 'd' EMIT 's' EMIT '\r' EMIT '\n' EMIT _ds DROP _stack_dump ;

\ ... .RS ...
\
\ ( -- )
\
: .RS 'r' EMIT 's' EMIT '\r' EMIT '\n' EMIT _rs DROP 1 - _stack_dump ;

\ ... PARSE ...
\ ... parse-escape ...
\
\ ( char -- caddr u )
\
: PARSE 0 _parse ;
: parse-escape 1 _parse ;


\ ( spaces>name -- )
\
: SEE ' _seext ;

\ (C: xu ... x1 x0 u -- xu ... x1 x0 xu )
: CS-PICK PICK ; compile-only

\ (C: xu xu-1 ... x0 u -- xu-1 ... x0 xu )
: CS-ROLL ROLL ; compile-only

\ (S: u -- addr )
: reserve DUP ALLOT HERE SWAP - ;

\ ( x -- )
: , ALIGN 1 CELLS reserve ! ;

\ ( char -- )
: C, 1 CHARS reserve C! ;

\ ( xt -- )
: COMPILE, , ; compile-only

\ value CONSTANT name
\
\ (C: x <spaces>name -- ) (S: -- x )
\
: CONSTANT CREATE , DOES> @ ;

\ ... FALSE ...
\ ... TRUE ...
\
\ (S: -- flag )
\
0 CONSTANT FALSE
FALSE INVERT CONSTANT TRUE

\ ... /CHAR ...
\ ... /CELL ...
\
\ (S: -- n )
\
1 CHARS CONSTANT /CHAR
1 CELLS CONSTANT /CELL

\ ... BL ...
\
\ (S: -- ' ' )
\
'\s' CONSTANT BL


\ ( -- u )
-3 2 / -2 - 0= CONSTANT floored
1 CELLS address-unit-bits * CONSTANT cell-bits
2 CELLS address-unit-bits * 2 + CONSTANT /hold
1 address-unit-bits LSHIFT 1 - CONSTANT MAX-CHAR
0 INVERT 1 RSHIFT CONSTANT MAX-N	\ 0x7fff...ffff
MAX-N INVERT CONSTANT MIN-N		\ 0x8000...0000
0 INVERT CONSTANT MAX-U			\ 0xffff...ffff

_rs CONSTANT return-stack-cells DROP DROP
_ds CONSTANT stack-cells DROP DROP

\ ( u "<spaces>name" -- addr )
: BUFFER: CREATE ALLOT ;

\ ... PAD ...
\
\ ( -- )
\
/PAD CHARS BUFFER: PAD

\ VARIABLE name
\
\ (C: <spaces>name -- ) \ (S: -- aaddr )
\
: VARIABLE CREATE 0 , ;

\ 2VARIABLE name
\
\ (C: <spaces>name -- ) \ (S: -- aaddr )
\
: 2VARIABLE CREATE 0 , 0 , ;

: [ FALSE STATE ! ; IMMEDIATE \ allow interpret
: ] TRUE STATE ! ; \ allow interpret

\ ... CELL+ ...
\
\ (S: aaddr1 -- aaddr2 )
\
: CELL+ /CELL + ;
: CELL- /CELL - ;

\ ... R@ ...
\
\ ( -- x )(R: x -- x)
\
: R@ R> R> DUP >R SWAP >R ; \ allow interpret

\ ... DROPALL ...
\
\ ( i*x -- )
\
: dropall _ds DROP DROP CELL- _dsp! ;

\ ... NEGATE ...
\
\ (S: n1 -- n2 )
\
: NEGATE INVERT 1 + ;

\ ... ALIGNED ...
\
\ (S: addr -- aaddr )
\
\ 	(addr + (pow2-1)) & -pow2
\
: ALIGNED /CELL 1 - + /CELL NEGATE AND ;

\ ... CHAR+ ...
\
\ (S: caddr1 -- caddr2 )
\
: CHAR+ /CHAR + ;
: CHAR- /CHAR - ;

\ ... DECIMAL ...
\
\ (S: -- )
\
: DECIMAL #10 BASE ! ;

\ ... HEX ...
\
\ (S: -- )
\
: HEX #16 BASE ! ;

\ ... OCTAL ...
\
\ (S: -- )
\
: octal #8 BASE ! ;

\ ... BINARY ...
\
\ (S: -- )
\
: binary #2 BASE ! ;

\ ... NIP ...
\
\ (S: x1 x2 -- x2 )
\
: NIP SWAP DROP ;

\ ... OVER ...
\
\ (S: x1 x2 -- x1 x2 x1 )
\
: OVER 1 PICK ;

\ ... ROT ...
\
\ (S: a b c -- b c a )
\
: ROT 2 ROLL ;

\ (S: x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
: 2ROT 5 ROLL 5 ROLL ;

\ ... S>D ...
\
\ ( n -- d )
\
\ @note
\	This assumes that 0< returns a proper flag (all bits 1) for true
\	as oppose simply any non-zero value for true.
\
: S>D DUP 0< ;		\ Sign extend into high word.

\ @note
\	More useful to as way of explaining in code what is being done
\	rather than rely on a zero value being pushed to the stack.
\	Consider ` 123456789 0 67 UM/MOD ` vs ` 123456789 U>D 67 UM/MOD `
\
\ ( u -- d )
: u>d 0 ;

\ @note
\	More useful to as way of explaining in code what is being done
\	rather than rely on a zero value being pushed to the stack.
\	Consider ` 123456789 0 67 UM/MOD ` vs ` 123456789 U>D 67 UM/MOD `
\
\ ( d -- u )
: d>u DROP ;

\ ... TUCK ...
\
\ (S: x1 x2 -- x2 x1 x2 )
\
: TUCK SWAP OVER ;

\ ... +! ...
\
\ (S: n addr --  )
\
: +! DUP @ ROT + SWAP ! ;

\ ... /STRING ...
\
\ (S: caddr u n -- caddr' u' )
\
: /STRING >R R@ - SWAP R> CHARS + SWAP ;

\ ... 1+ ...
\
\ (S: nu1 -- nu2 )
\
: 1+ 1 + ;

\ ... 1- ...
\
\ (S: nu1 -- nu2 )
\
: 1- 1 - ;

\ ... 2! ...
\
\ (S: lo hi aaddr -- )
\
: 2! TUCK ! CELL+ ! ;

\ ... 2@ ...
\
\ (S: aaddr -- lo hi )
\
\ Fetch from aaddr the two cells, hi lo, and place on stack lo hi.
\
: 2@ DUP CELL+ @ SWAP @ ;

\ ... 2* ...
\
\ (S: x1 -- x2 )
\
: 2* 1 LSHIFT ;

\ ... 2/ ...
\
\ (S: x1 -- x2 )
\
\	int s = -((unsigned) x >> LONG_BIT-1);
\	int sar = (s^x) >> n ^ s;
\
: 2/
	DUP cell-bits 1 -		\ S: x x bits
	RSHIFT NEGATE			\ S: x s
	DUP >R XOR 1 RSHIFT R> XOR	\ S: x'
;

\ x y  2CONSTANT name
\
\ (C: x y <spaces>name -- ) (S: -- x y )
\
: 2CONSTANT CREATE , , DOES> 2@ ;

MAX-U MAX-U 2CONSTANT MAX-UD
MAX-U MAX-N 2CONSTANT MAX-D
0 MIN-N 2CONSTANT MIN-D

\ ... 2DROP ...
\
\ (S: x1 x2 -- )
\
: 2DROP DROP DROP ;

\ ... 2DUP ...
\
\ (S: x1 x2 -- x1 x2 x1 x2 )
\
: 2DUP OVER OVER ;

\ ... 2OVER ...
\
\ (S: x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
\
: 2OVER 3 PICK 3 PICK ;

\ ... 2SWAP ...
\
\ (S: x1 x2 x3 x4 -- x3 x4 x1 x2 )
\
: 2SWAP 3 ROLL 3 ROLL ;

\ ... 2>R ...
\
\ (S: x1 x2 -- )(R: -- x1 x2 )
\
: 2>R
	R> ROT 			\ S: x2 ip x1  R: --
	>R SWAP			\ S: ip x2  R: x1
	>R >R			\ S: --  R: x1 x2 ip
; \ allow interpret

\ ... 2R> ...
\
\ (S: -- x1 x2 )(R: x1 x2 -- )
\
: 2R>
	R> R> R>		\ S: ip x2 x1  R: --
	ROT			\ S: x2 x1 ip  R: --
	>R SWAP			\ S: x1 x2  R: ip
; \ allow interpret

\ ... 2R@ ...
\
\ (S: -- x1 x2 )(R: x1 x2 -- x1 x2 )
\
: 2R@
	R> 2R>			\ S: ip x1 x2  R: --
	2DUP 2>R		\ S: ip x1 x2  R: x1 x2
	ROT >R			\ S: x1 x2  R: x1 x2 ip
; \ allow interpret

\ ... /MOD ...
\
\ (S: n1 n2 -- rem quot )
\
: /MOD >R S>D R> SM/REM ;

\ ... */ ...
\
\ (S: n1 n2 dsor -- quot )
\
: */ >R M* R> SM/REM SWAP DROP ;

\ ... */MOD ...
\
\ (S: n1 n2 dsor -- rem quot )
\
: */MOD >R M* R> SM/REM ;

\ ... 0<> ...
\
\ (S: nu -- flag )
\
: 0<> 0= 0= ;

\ ... 0> ...
\ Greater than zero.
\
\ (S: n -- flag )
\
: 0> 0 SWAP - 0< ;

\ ... = ...
\
\ (S: nu1 nu2 -- flag )
\
: = - 0= ;

\ ... <> ...
\
\ (S: nu1 nu2 -- flag )
\
: <> = 0= ;

\ ... > ...
\
\ (S: n1 n2 -- flag )
\
: > SWAP < ;

\ ... U> ...
\
\ (S: n1 n2 -- flag )
\
: U> SWAP U< ;

\ ... <= ...
\
\ (S: n1 n2 -- flag )
\
: <= > 0= ;

\ ... >= ...
\
\ (S: n1 n2 -- flag )
\
: >= < 0= ;

\ ... WITHIN ...
\
\ (S: nu1 nu2 nu3 -- flag )
\
\ @note
\	True if nu2 <= nu1 < nu3, otherwise false.
\
: WITHIN OVER - >R - R> U< ;

\ (S: xl xh -- bool )
: D0= OR 0= ;

\ (S: xl xh -- bool )
: D0< NIP 0< ;

\ (S: xl xh yl yh -- bool )
: D=
	2>R R>			\ S: xl xh yh	R: yl
	-			\ S: x1 dh	R: yl
	SWAP R>			\ S: dh x1 yl	R: --
	-			\ S: dh dl
	D0=			\ S: bool
;

\ ... CR ...
\
\ (S: -- )
\
: CR '\r' EMIT '\n' EMIT ;

\ ... DEPTH ...
\
\  ( -- u )
\
: DEPTH _ds DROP NIP ;

\ ... SPACE ...
\
\ (S: -- )
\
: SPACE BL EMIT ;

\ ... COUNT ...
\
\ (S: caddr1 -- caddr2 u )
\
: COUNT DUP CHAR+ SWAP C@ ;

\ ...  CHAR  ...
\
\ (S: <spaces>name -- char )
\
: CHAR PARSE-NAME DROP C@ ;

\ Compile LIT xt into the current word, which pushes xt when run.
\ (C: <spaces>name -- ) (S: -- xt )
: ['] LIT [ ' LIT COMPILE, ] COMPILE, ' COMPILE, ; IMMEDIATE compile-only

\ (C: x -- ) (S: x -- )
: LIT, ['] LIT COMPILE, , ;

\ ... : name ... [ x ] LITERAL ... ;
\
\  (C: x -- ) (S: -- x )
\
: LITERAL LIT, ; IMMEDIATE compile-only

\ ... test IF ... THEN ...
\ ... test IF ... ELSE ... THEN ...
\
\  (C: -- forw ) (S: flag -- )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: IF ['] _branchz COMPILE, >HERE 0 , ; IMMEDIATE compile-only

\ ... AHEAD ... THEN ...
\ ... test IF ... THEN ...
\ ... test IF ... ELSE ... THEN ...
\
\  (C: forw -- )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: THEN				\  C: forw_off
	>HERE SWAP -		\  C: dist
	DUP			\  C: dist dist
	HERE SWAP -		\  C: dist forw_addr
	!			\  C: --
; IMMEDIATE compile-only

\ ... ?DUP ...
\
\ (S: x -- 0 | x x )
\
: ?DUP DUP IF DUP THEN ;

\ (S: xl xh yl yh -- bool )
: DU<
	ROT 2DUP = IF		\ S: xl yl yh xh
	  2DROP U< EXIT		\ S: bool
	THEN U> >R 2DROP R>
;

\ (S: xl xh yl yh -- bool )
: D<
	ROT 2DUP = IF
	  2DROP < EXIT
	THEN > >R 2DROP R>
;

\ ... NAME>COMPILE ...
\
\ ( nt -- xt xt-compile )
\
\ @note
\	In Post4 an name token `nt` is the same as an execution token `xt`.
\
\ @see
\	A.15.6.2.1909.10 NAME>COMPILE
\
: NAME>COMPILE
	DUP immediate?
	IF
	  ['] EXECUTE EXIT
	THEN
	['] COMPILE,
;

\ ... NAME>INTERPRET ...
\
\ ( nt -- xt | 0 )
\
\ @note
\	In Post4 an name token `nt` is the same as an execution token `xt`.
\
: NAME>INTERPRET
	DUP compile-only?
	IF
	  DROP 0 EXIT
	THEN
;

\ ... FM/MOD ...
\
\ Dividend Divisor Remainder Quotient
\       10       7         3        1
\      -10       7         4       -2
\       10      -7        -4       -2
\      -10      -7        -3        1
\
\ See https://github.com/MitchBradley/cforth/blob/master/src/cforth/util.fth
\
\ ( dend dsor -- mod quot )
: FM/MOD
	\ Dividend and divisor have different signs?
	2DUP XOR 0< IF
	  \ Yep
	  DUP >R SM/REM
	  OVER IF
	    \ mod != 0, mod--, quot + dvr.
	    1- SWAP R> + SWAP EXIT
	  THEN
	  \ mod == 0
	  R> DROP
	  EXIT
	THEN
	\ No, same sign, SM/REM result same as FM/MOD.
	SM/REM
;

\ ... DEFER name ...
\
\ (S: <spaces>name -- )
\
: DEFER CREATE ['] ABORT , DOES> @ EXECUTE ;

\ ... DEFER! ...
\
\ (S: xt2 xt1 -- )
\
: DEFER! >BODY ! ;

\ ... DEFER@ ...
\
\ (S: xt1 -- xt2 )
\
: DEFER@ >BODY @ ;

VARIABLE catch_frame

DEFER _fsp@
DEFER _fsp!

:NONAME $dead ; ' _fsp@ DEFER!
:NONAME DROP ; ' _fsp! DEFER!

\ ... CATCH ...
\
\ ( i*x xt -- j*x 0 | i*x n )
\
: CATCH				\ S: xt   R: ip
	_dsp@ >R		\ S: xt   R: ip ds
	_fsp@ >R		\ S: xt   R: ip ds fs
	catch_frame @ >R	\ S: xt   R: ip ds fs cf
	_rsp@ catch_frame !	\ S: xt   R: ip ds fs cf
	EXECUTE			\ S: --   R: ip ds fs cf
	R> catch_frame !	\ S: --   R: ip ds fs
	2R> 2DROP 		\ S: --   R: ip
	0			\ S: 0    R: ip
; compile-only

\ ... THROW ...
\
\ ( k*x n -- k*x | i*x n )
\
: THROW				\ S: n    R:
	\ 0 THROW is a no-op.
	?DUP IF			\ S: n    R:
	  \ When no catch frame, throw to C.
	  catch_frame @ 0= IF	\ S: n    R:
	    _longjmp		\ S: --   R: --
	  THEN
	  \ Restore return stack of CATCH at EXECUTE.
	  catch_frame @ _rsp!	\ S: n    R: ip ds fs cf
	  R> catch_frame !	\ S: n    R: ip ds fs
	  R> _fsp!		\ S: n    R: ip ds fs
	  R> SWAP >R		\ S: ds   R: ip n
	  \ Restore data stack at start of CATCH
	  _dsp!			\ S: xt   R: ip n
	  DROP R>		\ S: n    R: ip
	THEN
; compile-only

\ ( xt -- )
: execute-compiling
	STATE @ IF EXECUTE EXIT THEN
	TRUE STATE ! EXECUTE FALSE STATE !
;

\ (S: <spaces>name -- )
\
\ @note
\	: bar state @ 0<> . ; immediate  : foo postpone bar ;  foo
\	Standard system should print -1.
\
\ @see
\	https://github.com/ForthHub/discussion/discussions/105
\
: POSTPONE
	PARSE-NAME FIND-NAME	\ S: xt | 0
	DUP 0= -13 AND THROW	\ S: xt
	DUP LIT, immediate?	\ S: bool
	IF			\ S: --
	  ['] execute-compiling
	  COMPILE, EXIT
	THEN
	['] COMPILE, COMPILE,
; IMMEDIATE compile-only

\ ...  [CHAR]  ...
\
\  (C: <spaces>name -- ) \ (S: -- char )
\
: [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE compile-only

\ ... BEGIN ... AGAIN
\ ... BEGIN ... test UNTIL ...
\ ... BEGIN ... test WHILE ... REPEAT ...
\
\  (C: -- dest )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: BEGIN >HERE ; IMMEDIATE compile-only

\ ... BEGIN ... AGAIN
\
\  (C: dest -- )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: AGAIN POSTPONE _branch >HERE - , ; IMMEDIATE compile-only

\ ... BEGIN ... test UNTIL ...
\
\  (C: dest -- ) \ (S: flag -- )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: UNTIL POSTPONE _branchz >HERE - , ; IMMEDIATE compile-only

\ ... AHEAD ... THEN ...
\
\  (C: -- forw )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: AHEAD POSTPONE _branch >HERE 0 , ; IMMEDIATE compile-only

\ ... test IF ... ELSE ... THEN ...
\
\  (C: forw1 -- forw2 )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: ELSE				\  C: forw1
	POSTPONE AHEAD		\  C: forw1 forw2
	1 CS-ROLL		\  C: forw2 forw1
	POSTPONE THEN		\  C: forw2
; IMMEDIATE compile-only

\ ... BEGIN ... test WHILE ... REPEAT ...
\
\  (C: dest -- forw dest ) \ (S: flag -- )
\ ... BEGIN ... test WHILE ... test WHILE ... REPEAT THEN ...
\ ... BEGIN ... test WHILE ... test WHILE ... AGAIN THEN THEN ...
\
\  Multiple WHILE possible to provide short-circuit testing, but each
\  additional WHILE needs a THEN in order to resolve each forward
\  reference remaining on the stack.
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: WHILE				\  C: dest
	POSTPONE IF		\  C: dest forw
	1 CS-ROLL		\  C: forw dest
; IMMEDIATE compile-only

\ ... BEGIN ... test WHILE ... REPEAT ...
\
\  (C: forw dest -- )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: REPEAT			\  C: forw dest
	POSTPONE AGAIN		\  C: forw
	POSTPONE THEN		\  C: --
; IMMEDIATE compile-only

\
\ (R: -- ip )
\
: RECURSE
	POSTPONE _call
	>HERE NEGATE ,
; IMMEDIATE compile-only

\ ... ABS ...
\
\ (S: n -- u )
\
: ABS DUP 0< IF NEGATE THEN ;

\ ... MAX ...
\
\ (S: n1 n2 -- n3 )
\
: MAX 2DUP < IF SWAP THEN DROP ;

\ ... MIN ...
\
\ (S: n1 n2 -- n3 )
\
: MIN 2DUP > IF SWAP THEN DROP ;

\ (S: d1 d2 -- d3 )
: DMAX 2OVER 2OVER D< IF 2SWAP THEN 2DROP ;
: DMIN 2OVER 2OVER D< INVERT IF 2SWAP THEN 2DROP ;

\ (S: dl dh -- dl' dh' )
: DNEGATE
	>R DUP
	IF
	  NEGATE R> INVERT
	ELSE
	  R> NEGATE
	THEN
;

\ (S: dl dh -- ul uh )
: DABS DUP 0< IF DNEGATE THEN ;

\ (S: xl xh yl yh -- zl zh )
: D+ ROT + >R DUP >R + DUP R> U< NEGATE R> + ;

\ (S: xl xh yl yh -- zl zh )
: D- DNEGATE D+ ;

\ (S: xl xh -- yl yh )
: D2* 2DUP D+ ;

\ (S: xl xh -- yl yh )
: D2/
	DUP 1 AND		\ S: xl xh lsb
	cell-bits 1 -		\ S: xl xh lsb bits
	LSHIFT SWAP 2/ >R	\ S: xl msb	R: xh'
	SWAP 1 RSHIFT		\ S: msb x1'	R: xh'
	OR R>			\ S: xl' xh'
;

\ (S: xl xh n -- yl yh )
: M+ S>D D+ ;

\ ( ul uh u -- tl tm th )
: ut*
	ROT OVER		\ S: uh u ul u
	UM* 2SWAP		\ S: tl tm uh u
	UM* SWAP		\ S: tl tm th tt
	0 D+			\ S: tl tm' th'
;

\ ( tl tm th u -- ud )
: ut/
	DUP >R			\ S: tl tm th u		R: u
	UM/MOD			\ S: tl r0 q0		R: u
	ROT ROT R>		\ S: q0 tl r0 u
	UM/MOD			\ S: q0 r1 q1
	NIP SWAP		\ S: q1 q0
;

\ ( d1 n1 n2 -- d2 )
: M*/
	>R 2DUP XOR >R		\ S: dl dh n1		R: n2 sign
	ABS >R			\ S: dl dh		R: n2 sign u1
	DABS R>			\ S: ul uh u1		R: n2 sign
	ut*			\ S: tl tm th 		R: n2 sign
	2R> >R			\ S: tl tm th n2	R: sign
	ut/			\ S: ud			R: sign
	R> 0< IF DNEGATE THEN	\ S: d2
;

\ ... : name ... [ x1 x2 ] 2LITERAL ... ;
\
\  (C: x1 x2 -- ) (S: -- x1 x2 )
\
: 2LITERAL SWAP POSTPONE LITERAL POSTPONE LITERAL ; IMMEDIATE compile-only

\ (S: x*i i -- )
: n,
	DUP ,			\ S: x1 .. xi i
	BEGIN ?DUP WHILE	\ S: x1 .. xi i
	  1- SWAP ,		\ S: x1 .. xi' i'
	REPEAT
;

\ (S: aaddr -- x*i )
: n@
	DUP @ SWAP		\ S: i addr
	OVER CELLS + SWAP	\ S: addr' i
	BEGIN ?DUP WHILE	\ S: ... addr' i
	  1- >R			\ S: addr'  R: i'
	  DUP @ SWAP CELL-	\ S: xi ... addr" R: i'
	  R>			\ S: xi ... addr" i'
	REPEAT DROP		\ S: x*i
;

\ (S: x*i aaddr -- )
: n!
	DUP @			\ S: x1 .. xi addr i
	BEGIN ?DUP WHILE	\ S: x1 .. xi addr i
	  1- >R			\ S: x1 .. xi addr R: i'
	  CELL+ TUCK !		\ S: x1 .. xi' addr' R: i'
	  R>			\ S: x1 .. xi' i'
	REPEAT DROP		\ S: --
;

\  value VALUE name
\
\  (C: x <spaces>name -- ) (S: -- x )
\
\ @note
\	Similar definition to CONSTANT.  Essentially VALUE when defined does:
\
\		VARIABLE name value name !
\
\	Referencing name does:
\
\		name @
\
\ @see
\	TO
\
: VALUE CREATE 1 n, DOES> n@ ;

\  lo hi 2VALUE name
\
\  (C: lo hi <spaces>name -- ) (S: -- lo hi )
\
\ @see
\	TO
\
: 2VALUE CREATE 2 n, DOES> n@ ;

\ ... x TO name ...
\
\ (S: i*x <spaces>name -- )
\
\ @note
\
\	x name !
\
\ @see
\	VALUE
\
: TO
	' >BODY
	STATE @ IF
	  POSTPONE LITERAL
	  POSTPONE n!
	  EXIT
	THEN
	n!
; IMMEDIATE

\ ( -- caddr u )
: source-remaining SOURCE >IN @ /STRING ;

\ ( delim -- bool )
\
\ Scan the input buffer character at a time until either the input
\ is exhusted, returning true; or an input character matches delim,
\ returning false.
\
: parse-more
	BEGIN				\ S: delim
	  source-remaining 0= IF	\ S: delim caddr
	    2DROP TRUE EXIT		\ empty input buffer
	  THEN
	  1 >IN +!			\ S: delim caddr
	  C@ OVER = IF			\ S: delim
	    DROP FALSE EXIT		\ input char matches delim
	  THEN
	AGAIN
;

\ ... ( comment) ...
\
\ (S: ccc<paren> -- )
\
\ Test cases:
\
\	( )		empty string
\	( abc)		single line string
\	( abc 123	multiline string
\	def 456
\	)
\
: (
	[CHAR] )
	BEGIN
	  DUP parse-more	\ find delim in input buffer
	WHILE			\ found delim yet?
	  REFILL 0=		\ read more input; EOF yet?
	UNTIL THEN
	DROP
; IMMEDIATE

\ ... \ comment to end of line
\
\ (S: ccc<eol>" -- )
\
: \
	BLK @ IF		( Block input source? )
	  >IN @ $3F OR 1+ >IN !	(   Advance >IN to next line in 16x64 block. )
	ELSE			( Streaming input... )
	  '\n' PARSE 2DROP	(   Skip up to and including newline. )
	THEN
; IMMEDIATE

\ ... TYPE ...
\
\ (S: caddr u -- )
\
: TYPE
	BEGIN DUP 0> WHILE	\  S: caddr u
	  1- SWAP		\  S: u' caddr
	  DUP C@ EMIT		\  S: u' caddr
	  CHAR+ SWAP		\  S: caddr' u'
	REPEAT 2DROP		\  S: --
;

\ ... SPACES ...
\
\ (S: n -- )
\
: SPACES
	BEGIN DUP 0> WHILE	\  S: n
	  SPACE 1-		\  S: n'
	REPEAT DROP		\  S: --
;

\ ... CMOVE ...
\
\ (S: src tar u -- )
: CMOVE
	BEGIN ?DUP WHILE	\ S: src tar u
	  1- >R			\ S: src tar R: u'
	  2DUP SWAP		\ S: src tar tar src R: u'
	  C@ SWAP C!            \ S: src tar R: u'
	  CHAR+ SWAP		\ S: tar' src R: u'
	  CHAR+ SWAP		\ S: src' tar' R: u'
	  R>			\ S: src' tar' u' R --
	REPEAT 2DROP
;

\ ... CMOVE> ...
\
\ (S: src tar u -- )
: CMOVE>
	BEGIN ?DUP WHILE	\ S: src tar u
	  1- >R			\ S: src tar R: u'
	  2DUP SWAP		\ S: src tar tar src R: u'
	  C@ SWAP C!            \ S: src tar R: u'
	  CHAR- SWAP		\ S: tar' src R: u'
	  CHAR- SWAP		\ S: src' tar' R: u'
	  R>			\ S: src' tar' u' R --
	REPEAT 2DROP
;

\ ... FILL ...
\
\ (S: caddr u char -- )
\
: FILL
	>R			\ S: caddr u		R: ch
	BEGIN ?DUP WHILE
	  1- SWAP		\ S: u' caddr		R: ch
	  R@ OVER >R		\ S: u' caddr ch	R: ch caddr
	  SWAP C! R> CHAR+	\ S: u'	caddr'		R: ch
	  SWAP			\ S: caddr' u'		R: ch
	REPEAT
	R> 2DROP
;

\ ... BLANK ...
\
\ (S: caddr u -- )
\
: BLANK BL FILL ;

\ ... ERASE ...
\
\ (S: addr u -- )
\
: ERASE 0 FILL ;

\ ... strrev ...
\
\ ( caddr u -- )
\
: strrev
	CHARS OVER +		\ S: x y
	BEGIN
	  CHAR- 2DUP <		\ S: x y' bool
	WHILE
	  2DUP 2DUP		\ S: x y' x y' x y'
	  C@ >R C@		\ S: x y' x y' cx R: cy
	  SWAP C! R>		\ S: x y' x cy
	  SWAP C!		\ S: x y'
	  SWAP CHAR+ SWAP	\ S: x' y'
	REPEAT
	2DROP			\ S: --
;

\ ... -TRAILING ...
\
\ (S: caddr u -- caddr u' )
\
: -TRAILING
	BEGIN DUP 0> WHILE
	  2DUP 1- +
	  C@ BL <> IF
	    EXIT
	  THEN
	  1-
	REPEAT
;

\
\ ( S: char -- value | 127 )
\
: _digit_value
	\ Is upper case?
	DUP 'A' 'Z' 1+ WITHIN		\ S: char bool
	\ Convert to lower case.
	IF $20 OR THEN			\ S: char'
	DUP '0' '9' 1+ WITHIN IF	\ S: char
	  '0' - EXIT			\ S: value
	ELSE DUP 'a' 'z' 1+ WITHIN IF	\ S: char
	  'a' - #10 + EXIT		\ S: value
	THEN THEN			\ S: char
	\ Not found.
	DROP #127			\ S: 127
;

\ ... >NUMBER ...
\
\ ( S: ud1 caddr len -- ud2 caddr' len' )
\
: >NUMBER
	BEGIN
	  DUP 0>			\ S: udl udh caddr len
	WHILE
	  OVER C@			\ S: udl udh caddr len char
	  _digit_value			\ S: udl udh caddr len digit
	  DUP BASE @ >= IF		\ S: udl udh caddr len digit
	    DROP EXIT			\ S: ud' caddr' len'
	  THEN				\ S: udl udh caddr len digit
	  ROT CHAR+ ROT 1-		\ S: ud1 udh digit caddr' len'
	  2>R >R BASE @			\ S: udl udh base	R: caddr' len' digit
	  UM* ROT BASE @		\ S: hl hh udl base	R: caddr' len' digit
	  UM*				\ S: hl hh ll lh	R: caddr' len' digit
	  R> M+				\ S: hl hh ll' lh'	R: caddr' len'
	  D+ 2R>			\ S: udl' udh' caddr' len'
	REPEAT
;

/HOLD CHARS BUFFER: _pic

VARIABLE _>pic

: _dumppic _pic /HOLD dump ;

\ ( char -- )
: HOLD _pic _>pic @ + C! 1 _>pic +! ;

\ ( caddr u -- )
: HOLDS BEGIN DUP WHILE 1- 2DUP + C@ HOLD REPEAT 2DROP ;

\ ( n -- )
: SIGN 0< IF '-' HOLD THEN ;

\ ( -- )
: <# _pic /HOLD BLANK 0 _>pic ! ;

\ ( xd -- caddr u )
: #> 2DROP _pic _>pic @ 2DUP strrev ;

\ ( d# n -- rem d#quot )
\
\ Thanks to Mitch Bradley
\ https://github.com/ForthHub/discussion/discussions/129#discussioncomment-3973896
\
: mu/mod
	>R 0 R@			\ S: ud0 ud1 0 n    R: n
	UM/MOD R>		\ S: ud0 r q n
	SWAP >R			\ S: ud0 r n	    R: q
	UM/MOD R>		\ S: r' q' q
;

\ ( ud1 -- ud2 )
: #
	BASE @ mu/mod ROT	\ S: q0 q1 rem
	DUP 0 #10 WITHIN	\ S: q0 q1 rem bool
	IF			\ S: q0 q1 rem
	  '0' +			\ S: q0 q1 digit
	ELSE
	  #10 - 'A' +		\ S: q0 q1 digit
	THEN
	HOLD			\ S: q0 q1
;

\ ( ud -- 0 0 )
: #S BEGIN # 2DUP D0= UNTIL ;

\ (S: dl dh -- n ) assume twos complement`
: D>S DROP ;

\ ( n -- )
: . DUP >R S>D DABS <# #S R> SIGN #> TYPE SPACE ;

\ (S: char "<chars>ccc" -- "ccc" )
: skip_chars
	BEGIN
	  SOURCE >IN @		\ S: char caddr u off
	  <= SWAP >IN @ + C@	\ S: char len_ge_off input
	  2 PICK <>		\ S: char len_ge_off char_neq
	  OR DUP 0= IF		\ S: char bool
	    1 >IN +!		\ S: char bool
	  THEN
	UNTIL			\ S: char
	DROP			\ S: --
;

\ ... char WORD ...
\
\ (S: char "<chars>ccc<char>" -- caddr )
\
\ See 3.3.3.6 Other transient regions paragraph 2.
\
: WORD				\ S: char
	DUP skip_chars		\ S: char
	PARSE 			\ S: caddr u
	DUP _pic C!		\ S: caddr u
	_pic CHAR+ SWAP		\ S: caddr pic' u
	MOVE _pic		\ S: caddr
;

\ ( caddr -- caddr 0 | xt 1 | xt -1 )
: FIND
	DUP COUNT FIND-NAME	\ S: caddr 0 | caddr xt
	DUP IF
	  NIP DUP immediate? IF	\ S: xt
	    1			\ S: xt 1
	  ELSE
	    -1			\ S: xt -1
	  THEN
	THEN
;

\ ...  U.  ...
\
\ (S: u -- )
\
: U. 0 <# #S #> TYPE SPACE ;

\ ... U.R  ...
\
\ (S: u w -- )
\
: U.R >R 0 <# #S #> R> OVER - SPACES TYPE ;

\ ... .R  ...
\
\ (S: n w -- )
\
: .R >R DUP >R ABS S>D <# #S R> SIGN #> R> OVER - SPACES TYPE ;

\ (S: d -- )
\
\ $dead $beef DECIMAL D.
\ 901658403578849173495469
\
\ $ffffffffffffffff $7fffffffffffffff DECIMAL D.
\ 170141183460469231731687303715884105727
\
: D. TUCK DABS <# #S ROT SIGN #> TYPE SPACE ;

\ (S: d# w -- )
: D.R >R TUCK DABS <# #S ROT SIGN #> R> OVER - SPACES TYPE ;

\ ( delim -- bool )
\
\ Scan the input buffer character at a time until either the input
\ is exhusted, returning true; or an input character matches delim,
\ returning false.
\
: emit-more
	BEGIN				\ S: delim
	  source-remaining 0= IF	\ S: delim caddr
	    2DROP TRUE EXIT		\ empty input buffer
	  THEN
	  1 >IN +!			\ S: delim caddr
	  DUP C@ [CHAR] \ = IF		\ escape next char?
	    1 >IN +! CHAR+ C@		\ S: delim ch
	  ELSE
	    C@ 2DUP = IF		\ S: delim ch
	      2DROP FALSE EXIT		\ input char matches delim
	    THEN
	  THEN
	  EMIT				\ S: delim
	AGAIN
;

\
\ .( ccc)
\
\ (S: ccc<paren> -- )
: .(
	[CHAR] )
	BEGIN
	  DUP emit-more		\ find delim in input buffer
	WHILE			\ found delim yet?
	  REFILL 0=		\ read more input; EOF yet?
	UNTIL THEN
	DROP
; IMMEDIATE

: .. ( x -- )
	BASE @ >R
	DUP HEX [CHAR] 0 EMIT [CHAR] x EMIT U. SPACE
	DUP HEX [CHAR] $ EMIT . SPACE
	DUP DECIMAL [CHAR] # EMIT . SPACE
	DUP OCTAL [CHAR] 0 EMIT . SPACE
	DUP BINARY [CHAR] % EMIT . SPACE
	DUP $20 $7F WITHIN IF [CHAR] ' EMIT EMIT [CHAR] ' EMIT ELSE DROP THEN
	CR R> BASE !
;

\ ... ? ...
\
\ (S: aaddr -- )
\
: ? BASE @ SWAP HEX [CHAR] $ EMIT DUP . @ SPACE BASE ! .. ;

\ ... N>R ...
\
\ (S: i*x n –– ) (R: –– i*x n )
\
: N>R				\  S: i*x n R: ip
	R> SWAP DUP		\  S: i*x ip n n R:
	BEGIN DUP 0> WHILE	\  S: j*x ip n j
	  3 ROLL			\  S: j*x ip n j x
	  >R 1-			\  S: j*x ip n j' R: j*x
	REPEAT
	DROP >R >R		\  S: -- R: j*x +n ip (j*x reverse of start i*x)
; \ allow interpret

\ ... NR> ...
\
\ (S: –– i*x +n ) (R: i*x +n –– )
\
\  The original stack order of i*x prior to the matching N>R is restored.
\
: NR>				\  S: -- R: i*x n ip (i*x reverse of original)
	R> R> DUP		\  S: ip n i R: i*x
	BEGIN DUP 0> WHILE	\  S: ip j*x n i R: i*x
	  R> 			\  S: ip j*x n i x' R: i*x
	  ROT			\  S: ip j*x i x' n R: i*x
	  ROT			\  S: ip j*x x' n i R: i*x
	  1-			\  S: ip j*x n i' R: i'*x
	REPEAT
	DROP DUP 1+ ROLL >R	\  S: ip j*x n R: ip
; \ allow interpret

\ ... limit first DO ... LOOP ...
\
\ (C: -- dest )(R: -- count) || (S: limit first -- ) (R: -- limit first )
\
: DO				\ C: --  R: ip
	POSTPONE 2>R		\ S: --  R: limit first
	R> 0 >R	>R		\ C: --  R: 0 ip
	POSTPONE BEGIN		\ C: dest R: 0 ip
; IMMEDIATE compile-only

\ ... limit first ?DO ... LOOP ...
\
\ (C: -- dest ) (R: -- forw 1 ) || (S: limit first -- ) (R: -- limit first )
\
: ?DO				\ C: --  R: ip
	POSTPONE 2>R		\ S: --  R: limit first
	POSTPONE 2R@		\ S: limit first  R: limit first
	POSTPONE <>		\ S: flag  R: limit first
	R>			\ C: ip  R: --
	POSTPONE IF >R 1 >R	\ C: ip  R: forw 1
	>R			\ C: --  R: forw 1 ip
	POSTPONE BEGIN		\ C: dest  R: forw 1 ip
; IMMEDIATE compile-only

\ ... limit first DO ... IF ... LEAVE THEN ... LOOP ...
\
\ (C: dest -- dest ) (R: n*forw n -- n'*forw n' )
\
: LEAVE				\ C: dest  R: n*forw n ip
	R> R> 1+		\ C: dest ip n'  R: n*forw
	POSTPONE AHEAD		\ C: dest ip n' forw  R: n*forw
	>R >R >R		\ C: dest  R: n'*forw n' ip
; IMMEDIATE compile-only

\ ... limit first DO ... test ?LEAVE ... LOOP ...
\
\ (C: dest -- dest ) (R: n*forw n -- n'*forw n' )
\
: ?LEAVE			\ C: dest flag  R: n*forw n ip
	POSTPONE IF		\ C: dest  R: n*forw n ip
	POSTPONE LEAVE		\ C: dest  R: n'*forw n' ip
	POSTPONE THEN		\ C: --  R: n'*forw n' ip
; compile-only

\ : X ... limit first DO ... test IF ... UNLOOP EXIT THEN ... LOOP ... ;
\
\ (S: --  ) (R: limit index ip -- ip )
\
: UNLOOP R> 2R> 2DROP >R ; compile-only

\ ... limit first DO ... LOOP ...
\
\ (S: -- flag ) (R: limit index ip -- limit index' ip )
\
\ @note
\	Can count from zero up to the unsigned maximum possible in one cell,
\	therefore 0 0 DO ... LOOP iterates UINT_MAX+1 times.
\
: _loop_inc_test
	R> 2R> 1+		\ S: ip limit index' R: --
	2DUP 2>R		\ S: ip limit index' R: limit index'
	=			\ S: ip flag R: limit index'
	SWAP >R			\ S: flag R: limit index' ip
;

\ ... limit first DO ... LOOP ...
\
\ (C: dest -- ) (R: n*forw n ip -- ip )
\
: LOOP				\ C: dest  R: n*forw n ip
	POSTPONE _loop_inc_test
	POSTPONE UNTIL		\ C: --  R: n*forw n ip

	\ Resolve LEAVE forward references.
	R> R>			\ C: ip n  R: n*forw
	BEGIN ?DUP WHILE        \ C: ip n  R: n*forw
	  1-			\ C: ip n' R: n*forw
	  R>			\ C: ip n' forw  R: n'*forw
	  POSTPONE THEN		\ C: ip n'  R: n'*forw
	REPEAT
	>R			\ C: -- R: ip

	\  LEAVE branches to just after UNTIL and before UNLOOP.
	POSTPONE UNLOOP
; IMMEDIATE compile-only

\ ... limit first DO ... LOOP ...
\
\ (S: -- index ) (R: limit index ip -- limit index ip )
\
: I R> R@ SWAP >R ; compile-only

\ ... limit first DO ... LOOP ...
\
\ (S: -- index1 ) (R: limit1 index1 limit2 index2 ip -- limit1 index1 limit2 index2 ip )
\
: J				\ S: --  R: l1 i1 l2 i2 ip
	R> R> R> R@		\ S: ip i2 l2 i1  R: l1 i1
	3 ROLL 3 ROLL 3 ROLL	\ S: i1 ip i2 l2  R: l1 i1
	>R >R >R		\ S: i1  R: l1 i1 l2 i2 ip
; compile-only

\ ... limit first DO ... LOOP ...
\
\ (S: n -- flag ) (R: limit index ip -- limit index' ip )
\
: _loop_step_test		\ S: n  R: l x ip
	\ Add step to index.
	R> 2R>			\ S: n ip l x
	3 ROLL +		\ S: ip l x'
	2DUP 2>R		\ S: ip l x'  R: l x'

	\ Has index crossed (limit-1) and limit boundary?
	\ ie. (INT_MIN - limit) & INT_MIN != (INT_MIN - limit + index) & INT_MIN
	SWAP MIN-N SWAP -	\ S: ip x' l'  R: l x'
	DUP MIN-N AND		\ S: ip x' l' sign  R: l x'
	>R + MIN-N AND R>	\ S: ip cross sign  R: l x'
	<> SWAP >R		\ S: flag  R: l x' ip
;

\ ... limit first DO ... LOOP ...
\
\ (C: dest -- ) (R: n*forw n ip -- ip )
\
: +LOOP				\ C: dest  R: n*forw n ip
	\  Loop increment and test.
	POSTPONE _loop_step_test
	POSTPONE UNTIL		\ C: --  R: n*forw n ip

	\ Resolve LEAVE forward references.
	R> R>			\ C: ip n  R: n*forw
	BEGIN ?DUP WHILE        \ C: ip n  R: n*forw
	  1-			\ C: ip n' R: n*forw
	  R>			\ C: ip n' forw  R: n'*forw
	  POSTPONE THEN		\ C: ip n'  R: n'*forw
	REPEAT
	>R			\ C: -- R: ip

	\  LEAVE branches to just after UNTIL and before UNLOOP.
	POSTPONE UNLOOP
; IMMEDIATE compile-only

\ ... x CASE ... ENDCASE
\
\ (C: -- #of ) (S: x -- x )
\
\	CASE
\	  test1 OF ... ENDOF
\	  ...
\	  testN OF ... ENDOF
\	  default action
\	ENDCASE
\
0 CONSTANT CASE IMMEDIATE compile-only

\ ... test OF ... ENDOF ...
\
\ (C: i*forw #of -- j*forw #of' )
\
: OF
	1+ >R			\ C: -- R: #of'
	POSTPONE OVER		\ S: x1 x2 x1
	POSTPONE =		\ S: x1 f
	POSTPONE IF		\ S: x1
	POSTPONE DROP		\ S: --
	R>			\ C: #of'
; IMMEDIATE compile-only

\ ... ENDOF ...
\
\ (C: forw1 #of -- forw2 #of )
\
: ENDOF
	>R			\ C: forw1 R: #of
	POSTPONE ELSE		\ C: forw2 R: #of
	R>			\ C: forw2 #of R: --
; IMMEDIATE compile-only

\ ... CASE ... ENDCASE ...
\
\ (C: i*forw i -- )(S: x -- )
\
: ENDCASE
	POSTPONE DROP		\ S: --
	0 ?DO			\ C: i*forw
	  POSTPONE THEN		\ C: i'*forw
	LOOP
; IMMEDIATE compile-only

\
\ ( char -- ascii )
\
\ @note
\	C backslash literal conventions; not Forth 200x.
\
: _backslash_literal
	CASE			\ S: char
	  \ While alphabetical is nice, more frequent is faster.
	  [CHAR] n OF $0A ENDOF	\ S: ascii char
	  [CHAR] r OF $0D ENDOF	\ S: ascii char
	  [CHAR] t OF $09 ENDOF	\ S: ascii char
	  [CHAR] e OF $1B ENDOF	\ S: ascii char
	  \ Less frequent
	  [CHAR] a OF $07 ENDOF	\ S: ascii char
	  [CHAR] b OF $08 ENDOF	\ S: ascii char
	  [CHAR] f OF $0C ENDOF	\ S: ascii char
	  [CHAR] s OF  BL ENDOF	\ S: ascii char
	  [CHAR] v OF $0B ENDOF	\ S: ascii char
	  [CHAR] ? OF $7F ENDOF	\ S: ascii char
	  [CHAR] 0 OF $00 ENDOF	\ S: ascii char
	  \ identity, ie. \x == x
	  DUP			\ S: ascii char
	ENDCASE			\ S: ascii
;

\
\ ( caddr -- )
\
\ Print a NUL terminated string.
\
\	: greet S\" Hello world.\n" ;
\	greet DROP puts
\
: puts BEGIN DUP C@ ?DUP WHILE EMIT 1+ REPEAT DROP ;

\
\ ( caddr -- u )
\
\ String length of NUL terminated string.
\
: strlen DUP BEGIN DUP C@ WHILE 1+ REPEAT SWAP - ;

\ Maximum for octet addressable units.
MAX-CHAR CONSTANT /COUNTED-STRING

\ ( S: -- caddr )
: _clit 			\ S: -- R: ip
	\ The IP points to counted string, get its length.
	R> DUP DUP C@		\ S: ip ip u R: --
	\ Update IP to point immediate after the counted string.
	1+ CHARS + ALIGNED >R	\ S: caddr R: ip'
;

: _cstring_append
	POSTPONE _clit		\ S: src u
	\ Reserve space for the length and string.
	DUP CHAR+ reserve	\ S: src u dst
	2DUP 2>R		\ S: src u dst   R: u dst
	\ Append the input string just after _clit in the data space.
	CHAR+ SWAP		\ S: src dst' u  R: u dst
	MOVE			\ S: --  R: u dst
	\ Save the string length.
	2R> C!			\ S: --  R: --
;

\ ... C" ccc" ...
\
\ (C: ccc<quote>" -- ) || (S: ccc<quote>" -- caddr )
\
: C" [CHAR] " PARSE _cstring_append ; IMMEDIATE compile-only

\ ... c\" ccc" ...
\
\ (C: ccc<quote>" -- ) || (S: ccc<quote>" -- caddr u )
\
: c\" [CHAR] " parse-escape _cstring_append ; IMMEDIATE compile-only

\ ... cputs ...
\
\ ( caddr -- )
\
\ Print a counted string.
\
\	CREATE greet c\" Hello world.\n"
\	greet cputs
\
: cputs DUP C@ SWAP CHAR+ SWAP TYPE ;

\ Number of transitent string buffers, power of 2.
\ Minimum 2 buffers for S" and S\".
\
\ @see
\	3.3.3.4 Text-literal regions
\	A.6.1.2165 S"
\
2 CONSTANT _str_buf_max

_str_buf_max 1- CONSTANT _str_buf_mask

\ Transient string buffers of size /PAD.
/PAD CHARS _str_buf_max * BUFFER: _str_bufs

\ Offset of last used buffer.
VARIABLE _str_buf_index

\ Next string buffer address.
\
\ ( -- caddr )
\
: _str_buf_next
	_str_buf_index @	\ S: u
	1+ _str_buf_mask AND	\ S: u'
	DUP _str_buf_index !	\ S: u'
	/PAD CHARS *		\ S: offset
	_str_bufs +		\ S: caddr
;

\ ... _slit ...
\
\ (S: -- caddr u )
\
\ @note
\	The current IP is used to find and compute the
\	address and length of the string stored within the word.
\	It is then modified to point to just after the string.
\
: _slit				\ S: -- 		R: ip
	R@ @ R>			\ S: u ip 		R: --
	CELL+ SWAP 2DUP		\ S: caddr u caddr u 	R: --
	CHAR+			\ Account for terminating NUL byte.
	CHARS + ALIGNED		\ S: caddr u ip' 	R: --
	>R			\ S: caddr u 		R: ip'
;

\ (C: src u -- ) (S: src u -- caddr u )
: SLITERAL
	  POSTPONE _slit	\ S: src u
	  \ Append length.
	  DUP ,			\ S: src u
	  \ Append string and NUL terminate for C.
	  DUP >R reserve R>	\ S: src dst u
	  MOVE 0 C, ALIGN	\ S: --
; IMMEDIATE compile-only

: _string0_store
	STATE @ IF
	  POSTPONE SLITERAL
	ELSE
	  \ Select next transient buffer.
	  DUP >R _str_buf_next	\ S: src u dst		R: u
	  \ Copy string from source to transient buffer.
	  DUP >R SWAP		\ S: src dst u		R: u dst
	  MOVE			\ S: -- 		R: u dst
	  \ Add terminating NUL byte for convenience for C.
	  R> R>			\ S: dst u 		R: --
	  2DUP CHARS + 0	\ S: dst u end NUL
	  SWAP C!		\ S: dst u
	THEN
;

\ ... S" ccc" ...
\
\ (C: ccc<quote>" -- ) || (S: ccc<quote>" -- caddr u )
\
: S" [CHAR] " PARSE _string0_store ; IMMEDIATE

\ ... S\" ccc" ...
\
\ (C: ccc<quote>" -- ) || (S: ccc<quote>" -- caddr u )
\
: S\" [CHAR] " parse-escape _string0_store ; IMMEDIATE

\ ... ." ccc" ...
\
\ (S: ccc<quote>" -- )
\
: ." POSTPONE S" POSTPONE TYPE ; IMMEDIATE compile-only

: .\" POSTPONE S\" POSTPONE TYPE ; IMMEDIATE compile-only

\ Same behaviour as C's strcmp().
\
\ (S: caddr1 u1 caddr2 u2 --  )
: strcmp
	ROT SWAP 2DUP -		\ S: s1 s2 u1 u2 du
	DUP >R			\ S: s1 s2 u1 u2 du	R: du
	DUP 0= IF		\ S: s1 s2 u1 u2 du	R: du
	  \ Equal string lengths.
	  2DROP			\ S: s1 s2 u1		R: du
	ELSE 0< IF		\ S: s1 s2 u1 u2	R: du
	  \ s1 shorter than s2.
	  DROP			\ S: s1 s2 u1		R: du
	ELSE
	  \ s1 longer than s2.
	  NIP			\ S: s1 s2 u2		R: du
	THEN THEN
	BEGIN ?DUP WHILE	\ S: s1 s2 u		R: du
	  1- >R			\ S: s1 s2		R: du u'
	  DUP C@ >R CHAR+ SWAP	\ S: s2' s1             R: du u' c2
	  DUP C@ >R CHAR+ SWAP  \ S: s1' s2'		R: du u' c2 c1
	  R> R> - ?DUP IF	\ S: s1' s2' diff	R: du u'
	    \ Different stings at character.
	    2R> 2DROP 		\ S: s1' s2' diff
	    >R 2DROP R>		\ S: diff
	    0< IF -1 ELSE 1 THEN
	    EXIT
	  THEN
	  R>			\ S: s1' s2' u'		R: du
	REPEAT
	\ Matching leading strings.
	2DROP R>		\ S: du
	?DUP IF
	  0< IF -1 ELSE 1 THEN
	ELSE
	  0			\ Equal strings.
	THEN
;

\ ... COMPARE ...
\
\ (S: caddr1 u1 caddr2 u2 -- -1 | 0 | 1 )
\
: COMPARE strcmp ;

\ (S: caddr1 u1 caddr2 u2 -- bool )
: starts-with
	ROT SWAP 2DUP <		\ S: s1 s2 u1 u2
	IF
	  \ String too short to start with prefix.
	  2DROP 2DROP FALSE
	  EXIT			\ S: bool
	THEN
	NIP DUP			\ S: s1 s2 u2 u2
	ROT ROT			\ S: s1 u2 s2 u2
 	strcmp 0=		\ S: bool
;

\ ... SEARCH ...
\
\ (S: caddr1 u1 caddr2 u2 -- caddr u bool )
\
: SEARCH
	2>R 2DUP		\ S: caddr1 u1 caddr1 u1 R: caddr2 u2
	BEGIN ?DUP WHILE
	  2DUP 2R@ starts-with	\ S: caddr1 u1 caddr1 u1 bool R: caddr2 u2
	  IF
	    R> R> 2DROP		\ S: caddr1 u1 caddr1' u1' R: --
	    2SWAP 2DROP	TRUE	\ S: caddr1' u1' true
	    EXIT
	  THEN
	  1- SWAP CHAR+ SWAP	\ S: caddr1 u1 caddr' u1' R: caddr2 u2
	REPEAT
	DROP R> R> 2DROP	\ S: caddr1 u1 R: --
	FALSE			\ S: caddr1 u1 false
;

\ (S: bool caddr u -- )
: _abort_msg?
	ROT IF
	  catch_frame @ 0= IF
	    TYPE CR ABORT
	  THEN
	  -2 THROW
	THEN
	2DROP
;

\ : X ... test ABORT" message" ...
\
\ (C: ccc<quote>" -- ) (S: i*x x1 --  | i*x ) ( R: j*x --  | j*x )
\
: ABORT" POSTPONE S" POSTPONE _abort_msg? ; IMMEDIATE compile-only

\ ... SCR ...
\
\ (S: -- aaddr )
\
VARIABLE SCR

\ ... LIST ...
\
\ (S: u -- )
\
: LIST				\ S: u
	DUP SCR !		\ S: u
	BLOCK			\ S: caddr
	16 0 DO
	  I 1+ 2 .R
	  [CHAR] | EMIT
	  DUP 64 TYPE		\ S: caddr
	  [CHAR] | EMIT CR
	  64 CHARS +		\ S: caddr'
	LOOP DROP		\ S: --
;

\ ... FLUSH ...
\
\ (S: -- )
\
: FLUSH SAVE-BUFFERS EMPTY-BUFFERS ;

\ ... LIST+ ...
\
\ (S: -- )
\
: list+ SCR @ 1+ LIST ;

\ ... LOAD ...
\
\ (S: i*x u -- j*x )
\
: LOAD BLK @ SWAP DUP BLK ! BLOCK 1024 EVALUATE BLK ! ;

\ ... THRU ...
\
\ (S: start end -- )
\
: THRU				\ S: start end
	1+ SWAP			\ S: end' start
	DO			\ S: --
	  I LOAD
	LOOP
;

\ ... AT-XY ...
\
\ (S: column row -- )
\
\ @note
\	ANSI / VT100 terminal assumed.
\
: AT-XY
	S\" \e[" TYPE
	1+ 0 U.R
	';' EMIT
	1+ 0 U.R
	'H' EMIT
;

\ ... PAGE ...
\
\ (S: -- )
\
\ @note
\	ANSI / VT100 terminal assumed.
\
: PAGE 0 0 AT-XY S\" \e[0J" TYPE ;

\ ... INCLUDE filename ...
\
\ (S: <spaces>filename" -- )
\
: INCLUDE PARSE-NAME INCLUDED ;

\ ... ACTION-OF ...
\
\ (S: <spaces>name -- xt )
\
: ACTION-OF
	STATE @ IF
	  POSTPONE [']
	  POSTPONE DEFER@
	ELSE
	  ' DEFER@
	THEN
; IMMEDIATE

\ ... IS name ...
\
\ (S: xt <spaces>name -- )
\
: IS
	STATE @ IF
	  POSTPONE [']
	  POSTPONE DEFER!
	ELSE
	  ' DEFER!
	THEN
; IMMEDIATE

\ ... BEGIN-STRUCTURE name ...
\
\ (C: <spaces>name -- aaddr 0 ) (S: -- size )
\
: BEGIN-STRUCTURE
	CREATE HERE 0 0 ,	\ C: aaddr 0
	DOES> @			\ S: size
;

\ ... END-STRUCTURE ...
\
\ (C: aaddr size -- )
\
: END-STRUCTURE SWAP ! ;

\ ... +FIELD name ...
\
\ (C: offset size <spaces>name -- offset' ) \ (S: addr -- addr' )
\
\ Note does not align items.
\
\ Structure name defined last:
\
\	0			\ initial total byte count
\	  1 CELLS +FIELD p.x	\ single cell field named p.x
\	  1 CELLS +FIELD p.y	\ single cell field named p.y
\	CONSTANT point 		\ save structure size
\
\ Structure name defined first:
\
\	BEGIN-STRUCTURE point	\ create the named structure
\	  1 CELLS +FIELD p.x	\ A single cell filed named p.x
\	  1 CELLS +FIELD p.y	\ A single cell field named p.y
\	END-STRUCTURE
\
: +FIELD
	CREATE OVER , +		\ C: aaddr offset size -- aaddr offset'
	DOES> @ +		\ S: addr -- addr'
;

\ ... CFIELD: name ...
\
\ (C: offset <spaces>name -- offset' ) \ (S: addr -- addr' )
\
: CFIELD: 1 CHARS +FIELD ;

\ ... FIELD: name ...
\
\ (C: offset <spaces>name -- offset' ) \ (S: addr -- addr' )
\
: FIELD: ALIGNED 1 CELLS +FIELD ;

: [DEFINED] ( <space>name -- bool ) PARSE-NAME FIND-NAME 0<> ; IMMEDIATE
: [UNDEFINED] ( <space>name -- bool ) PARSE-NAME FIND-NAME 0= ; IMMEDIATE

: [ELSE] ( -- )
	1 BEGIN 				\ level
	  BEGIN PARSE-NAME DUP WHILE		\ level adr len
	    2DUP S" \" COMPARE 0= IF
	      \ Ignore remainder of comment line.
	      2DROP POSTPONE \
	    ELSE 2DUP S" [IF]" COMPARE 0= IF		\ level adr len
	      2DROP 1+				\ level'
	    ELSE				\ level adr len
	      2DUP S" [ELSE]" COMPARE 0= IF	\ level adr len
	        2DROP 1-			\ level'
	        \ Not yet zero, then restore previous level while nested.
	        DUP IF 1+ THEN			\ level'
	      ELSE 				\ level adr len
	        S" [THEN]" COMPARE 0= IF	\ level
	          1-				\ level'
	        THEN
	      THEN
	    THEN THEN
	    ?DUP 0= IF EXIT THEN		\ level'
	  REPEAT 2DROP				\ level
	REFILL 0= UNTIL				\ level
	DROP
; IMMEDIATE

: [IF] ( flag -- )
	0= IF POSTPONE [ELSE] THEN
; IMMEDIATE

: [THEN] ( -- ) ; IMMEDIATE

[DEFINED] _fs [IF]
_fs CONSTANT floating-stack DROP DROP
[THEN]

BEGIN-STRUCTURE p4_string
	FIELD: str.length
	FIELD: str.string	\ pointer C string
END-STRUCTURE

BEGIN-STRUCTURE p4_word
	FIELD: w.prev		\ pointer previous word
	p4_string +FIELD w.name
	FIELD: w.bits
	FIELD: w.code		\ pointer
	FIELD: w.ndata		\ data length
	FIELD: w.data		\ pointer to data cells
END-STRUCTURE

BEGIN-STRUCTURE p4_block
	FIELD: blk.state	\ 0 free, 1 clean, 2 dirty
	FIELD: blk.number	\ 0 < number
	1024 +FIELD blk.buffer	\ buffer
END-STRUCTURE

BEGIN-STRUCTURE p4_stack
	FIELD: stk.size
	FIELD: stk.top		\ pointer
	FIELD: stk.base		\ pointer
END-STRUCTURE

BEGIN-STRUCTURE p4_input
	FIELD: in.fp		\ pointer
	FIELD: in.blk
	FIELD: in.size
	FIELD: in.length
	FIELD: in.offset
	FIELD: in.buffer	\ pointer
	FIELD: in.unget
END-STRUCTURE

\ Example
\
\	_ctx		 	\ Post4 machine context pointer
\	ctx.words @		\ pointer to most recent word
\	w.name str.string @	\ pointer to word name
\	puts			\ write name
\
BEGIN-STRUCTURE p4_ctx
	p4_stack +FIELD ctx.ds	\ see _ds
	p4_stack +FIELD ctx.rs	\ see _rs
[DEFINED] _fs [IF]
	p4_stack +FIELD ctx.fs	\ see _fs
	FIELD: ctx.precision	\ see PRECISION and SET-PRECISION
[THEN]
[DEFINED] TRACE [IF]
        FIELD: ctx.trace        \ see TRACE
[THEN]
	FIELD: ctx.state	\ see STATE
	FIELD: ctx.words	\ p4_word pointer
	FIELD: ctx.radix	\ see BASE
	FIELD: ctx.argc
	FIELD: ctx.argv
	FIELD: ctx.here		\ see HERE
	FIELD: ctx.end		\ see UNUSED
	FIELD: ctx.mem		\ current data space
	p4_input +FIELD ctx.input
	p4_block +FIELD ctx.block
	FIELD: ctx.block_fd
	/PAD +FIELD ctx.tty	\ buffer, see SOURCE
	0 +FIELD ctx.on_throw	\ size varies by host OS
END-STRUCTURE

\ ... NAME>STRING ...
\
\ ( nt -- caddr u )
\
: NAME>STRING
	  w.name 		\ S: nt
	  DUP str.string @	\ S: nt name
	  SWAP str.length @	\ S: name length
;

\ ... WORDS ...
\
\ ( -- )
\
: WORDS
	0 >R			\ S: --  R: col
	_ctx ctx.words @	\ S: w
	BEGIN
	  DUP NAME>STRING	\ S: w word  R: col
	  DUP R> + 1+		\ S: w name length col'  R: --
	  \ Does current column exceed terminal width?
	  DUP _window NIP >= IF	\ S: w name length col'  R: --
	    CR DROP DUP 1+	\ S: w name length col"  R: --
	  THEN			\ S: w name length col  R: --
	  >R TYPE SPACE		\ S: w  R: col
	  w.prev @ DUP		\ S: w' w'  R: col
	0= UNTIL		\ S: w'  R: col
	R> 2DROP CR		\ S: --  R: --
;

\ : newword ... [: nested words ;] ... ;
\
\ (C: -- quotation-sys colon-sys )
\
: [:
	POSTPONE AHEAD
	\ Pop enclosing defintion being compiled.
	_ctx ctx.words @	\ C: q-sys
	DUP w.prev @		\ C: q-sys word
	_ctx ctx.words !	\ C: q-sys
	\ Start nested definition.
	:NONAME			\ C: q-sys c-sys
; IMMEDIATE compile-only

\ : newword ... [: nested words ;] ... ;
\
\ (C: quotation-sys colon-sys -- )(S: -- xt )
\
: ;]				\ C: q-sys c-sys
	\ Bump the saved RS length to account for ;] return IP
	\ in order to pass the stack depth checks.
	$100 +
	\ End current nested definition.
	POSTPONE ; ]		\ C: q-sys xt
	>R			\ C: q-sys  R: xt
	\ Push previous enclosing defintion being compiled.
	DUP w.prev		\ C: q-sys prev  R: xt
	_ctx ctx.words @	\ C: q-sys prev word  R: xt
	SWAP ! 			\ C: q-sys  R: xt
	_ctx ctx.words !	\ C: --  R: xt
	POSTPONE THEN
	R> POSTPONE LITERAL	\ C: --  R: --
; IMMEDIATE compile-only

[DEFINED] _fs [IF]
' _fsp_get IS _fsp@
' _fsp_put IS _fsp!

: FLOATS CELLS ;
1 FLOATS CONSTANT /FLOAT
: FLOAT+ /FLOAT + ;

: FALIGN ALIGN ;
: FALIGNED ALIGNED ;
: FFIELD: FIELD: ;

: .fs 'f' EMIT 's' EMIT '\r' EMIT '\n' EMIT _fs DROP _stack_dump ;

: fs>ds fs>rs R> ;
: ds>fs >R rs>fs ;

\ ( fu ... f1 f0 u -- fu ... f1 f0 fu )
: fpick _fs drop floats + /float - swap floats - f@ ;

\ (F: f -- )
: F, FALIGN 1 FLOATS reserve F! ;

\ (F: f -- f f )
: FDUP fs>rs R@ ds>fs rs>fs ;

\ (F: f -- )
: FDROP fs>ds DROP ;

\ ( F: f1 f2 -- f2 f1 )
: FSWAP fs>ds fs>ds SWAP ds>fs ds>fs ;

\ (F: f1 f2 -- f1 f2 f1 )
: FOVER FSWAP FDUP fs>rs FSWAP rs>fs ;

\ (F: f1 f2 f3 -- f2 f3 f1 )
: FROT fs>rs FSWAP rs>fs FSWAP ;

\ (F: -- f ) (R: ip -- ip' )
: flit R> DUP FLOAT+ >R F@ ;

\ Similar to LIT,
\ (F: f -- )
: flit, ['] flit COMPILE, F, ;

\ ... : name ... [ x.y ] FLITERAL ... ;
\
\  (C: f -- ) (F: -- f )
\
: FLITERAL flit, ; IMMEDIATE compile-only

\ (C: F:x <spaces>name -- ) (F: -- x )
: FCONSTANT CREATE F, DOES> F@ ;

\ (C: <spaces>name -- ) (S: -- aaddr )
: FVARIABLE VARIABLE ;

: FVALUE CREATE 0 , F, DOES> FLOAT+ F@ ;

\ ( F: f -- ) ( <spaces>name -- )
: fto
	' >BODY CELL+
	STATE @ IF
	  POSTPONE LITERAL
	  POSTPONE F!
	  EXIT
	THEN
	F!
; IMMEDIATE

\ ( F: f -- ) ( <spaces>name -- )
: TO
	>IN @ ' >BODY @ 		\ S: v in type
	SWAP >IN !			\ S: v type
	IF
	  \ type != 0, eg. 1 or 2
	  ['] TO EXECUTE EXIT
	THEN
	\ type == 0
	['] fto EXECUTE
; IMMEDIATE

\ (F: f1 f2 -- ) (S: -- bool)
: F< F- F0< ;

\ (F: f1 f2 -- ) (S: -- bool)
: F= F- F0= ;

\ (S: -- u )
: FDEPTH _fs DROP NIP ;

\ ( F: f1 -- f2 )
: FNEGATE [ 0.0 ] FLITERAL FSWAP F- ;
: FABS FDUP F0< IF FNEGATE THEN ;

\ (S: -- u )
: PRECISION _ctx ctx.precision @ ;

\ (S: u -- )
: SET-PRECISION _ctx ctx.precision ! ;

[THEN]

MARKER rm_user_words
