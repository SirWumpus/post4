MARKER rm_core_words

: \  '\n' PARSE DROP DROP ; IMMEDIATE

\ Post4 Copyright 2007, 2018 by Anthony Howe.  All rights reserved.

\
\ ... _ds ...
\
\ ( -- aaddr u )
\

\
\ ... _rs ...
\
\ ( -- aaddr u )
\

\
\ ... _stack_dump ...
\
\ ( aaddr u -- )
\

\
\ ... .S ...
\
\ ( -- )
\
: .S 'd' EMIT 's' EMIT '\n' EMIT _ds _stack_dump ;

\
\ ... .RS ...
\
\ ( -- )
\
: .RS 'r' EMIT 's' EMIT '\n' EMIT _rs 1 -  _stack_dump ;

\
\  value CONSTANT name
\
\  (C: x <spaces>name -- ) \ (S: -- x )
\
: CONSTANT CREATE , DOES> @ ;

\
\ ... FALSE ...
\
\ (S: -- 0 )
\
0 CONSTANT FALSE

\
\ ... TRUE ...
\
\ (S: -- 1 )
\
FALSE INVERT CONSTANT TRUE

\
\ ... /CHAR ...
\ ... /CELL ...
\
\ (S: -- n )
\
1 CHARS CONSTANT /CHAR
1 CELLS CONSTANT /CELL

\
\ ... BL ...
\
\ (S: -- ' ' )
\
'\s' CONSTANT BL

\ ... /PAD ...
\
\ ( -- n )
\
256 CONSTANT /PAD

\
\ ... PAD ...
\
\  ( -- )
\
/PAD CREATE PAD CHARS ALLOT

\
\  VARIABLE name
\
\  (C: <spaces>name -- ) \ (S: -- aaddr )
\
: VARIABLE CREATE 0 , ;

: [ 0 STATE ! ; IMMEDIATE
: ] 1 STATE ! ;

\
\  value VALUE name
\
\  (C: x <spaces>name -- ) \ (S: -- x )
\
\ @note
\ 	Similar definition to CONSTANT.  Essentially VALUE when defined does:
\
\ 		VARIABLE name value name !
\
\ 	Referencing name does:
\
\ 		name @
\
\ @see
\ 	TO
\
: VALUE CREATE , DOES> @ ;

\
\ ... CELL+ ...
\
\ (S: aaddr1 -- aaddr2 )
\
: CELL+ /CELL + ;

\
\ ... ALIGNED ...
\
\ (S: addr -- aaddr )
\
\  (addr + (pow2-1)) & -pow2
\
: ALIGNED /CELL 1 - + /CELL NEGATE AND ;

\
\ ... CHAR+ ...
\
\ (S: c-addr1 -- c-addr2 )
\
: CHAR+ /CHAR + ;

\
\ ... DECIMAL ...
\
\ (S: -- )
\
: DECIMAL #10 BASE ! ;

\
\ ... HEX ...
\
\ (S: -- )
\
: HEX #16 BASE ! ;

\
\ ... OCTAL ...
\
\ (S: -- )
\
: OCTAL #8 BASE ! ;

\
\ ... NIP ...
\
\ (S: x1 x2 -- x2 )
\
: NIP SWAP DROP ;

\
\ ... OVER ...
\
\ (S: x1 x2 -- x1 x2 x1 )
\
: OVER 1 PICK ;

\
\ ... ROT ...
\
\ (S: a b c -- b c a )
\
: ROT 2 ROLL ;

\
\ ... S>D ...
\
\ ( n -- d )
\
\ @note
\	This assumes that 0< returns a proper flag (all bits 1) for true
\	as oppose simply any non-zero value for true.
\
: S>D DUP 0< SWAP ;

\
\ ... TUCK ...
\
\ (S: x1 x2 -- x2 x1 x2 )
\
: TUCK SWAP OVER ;

\
\ ... +! ...
\
\ (S: n addr --  )
\
: +! DUP @ ROT + SWAP ! ;

\
\ ... 1+ ...
\
\ (S: nu1 -- nu2 )
\
: 1+ 1 + ;

\
\ ... 1- ...
\
\ (S: nu1 -- nu2 )
\
: 1- 1 - ;

\
\ ... 2! ...
\
\ (S: lo hi aaddr -- )
\
: 2! SWAP OVER ! CELL+ ! ;

\
\ ... 2@ ...
\
\ (S: aaddr -- lo hi )
\
\ Fetch from aaddr the two cells, hi lo, and place on stack lo hi.
\
: 2@ DUP CELL+ @ SWAP @ ;

\
\ ... 2* ...
\
\ (S: x1 -- x2 )
\
: 2* 1 LSHIFT ;

\
\ ... 2/ ...
\
\ (S: x1 -- x2 )
\
: 2/ 1 RSHIFT ;

\
\ ... 2DROP ...
\
\ (S: x1 x2 -- )
\
: 2DROP DROP DROP ;

\
\ ... 2DUP ...
\
\ (S: x1 x2 -- x1 x2 x1 x2 )
\
: 2DUP OVER OVER ;

\
\ ... 2OVER ...
\
\ (S: x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
\
: 2OVER 3 PICK 3 PICK ;

\
\ ... 2SWAP ...
\
\ (S: x1 x2 x3 x4 -- x3 x4 x1 x2 )
\
: 2SWAP 3 ROLL 3 ROLL ;

\
\ ... 2>R ...
\
\ (S: x1 x2 -- )(R: -- x1 x2 )
\
: 2>R
	R> ROT 			\ S: x2 ip x1  R: --
	>R SWAP			\ S: ip x2  R: x1
	>R >R			\ S: --  R: x1 x2 ip
;

\
\ ... 2>R ...
\
\ (S: -- x1 x2 )(R: x1 x2 -- )
\
: 2R>
	R> R> R>		\ S: ip x2 x1  R: --
	ROT			\ S: x2 x1 ip  R: --
	>R SWAP			\ S: x1 x2  R: ip
;

\
\ ... 2R@ ...
\
\ (S: -- x1 x2 )(R: x1 x2 -- x1 x2 )
\
: 2R@
	R> 2R>			\ S: ip x1 x2  R: --
	2DUP 2>R		\ S: ip x1 x2  R: x1 x2
	ROT >R			\ S: x1 x2  R: x1 x2 ip
;

\
\ ... 0<> ...
\
\ (S: nu -- flag )
\
: 0<> 0= 0= ;

\
\ ... 0> ...
\
\ (S: n -- flag )
\
: 0> 0 SWAP - 0< ;

\
\ ... = ...
\
\ (S: nu1 nu2 -- flag )
\
: = - 0= ;

\
\ ... <> ...
\
\ (S: nu1 nu2 -- flag )
\
: <> = 0= ;

\
\ ... < ...
\
\ (S: n1 n2 -- flag )
\
: < - 0< ;

\
\ ... > ...
\
\ (S: n1 n2 -- flag )
\
: > - 0> ;

\
\ ... <= ...
\
\ (S: n1 n2 -- flag )
\
: <= - DUP 0= SWAP 0< OR ;

\
\ ... >= ...
\
\ (S: n1 n2 -- flag )
\
: >= < 0= ;

\
\ ... WITHIN ...
\
\ (S: nu1 nu2 nu3 -- flag )
\
\ @note
\ 	True if nu2 <= nu1 < nu3, otherwise false.
\
: WITHIN OVER - >R - R> U< ;

\
\ ... CR ...
\
\ (S: -- )
\
: CR '\r' EMIT '\n' EMIT ;

\
\ ... DEPTH ...
\
\  ( -- u )
\
: DEPTH _ds NIP ;

\
\ ... FILL ...
\
\ (S: c-addr u char -- )
\
: FILL
	2 PICK			\  S: c- u c c-
	! 1- OVER		\  S: c- u' c-
	1+ SWAP			\  S: c- c-' u
	CMOVE			\  S: --
;

\
\ ... BLANK ...
\
\ (S: c-addr u -- )
\
: BLANK BL FILL ;

\
\ ... SPACE ...
\
\ (S: -- )
\
: SPACE BL EMIT ;

\
\ ...  CHAR  ...
\
\ (S: <spaces>name -- char )
\
: CHAR PARSE-NAME DROP C@ ;

\
\ ...  [CHAR]  ...
\
\  (C: <spaces>name -- ) \ (S: -- char )
\
: [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE

\
\ ... ['] name ...
\
\  (C: <spaces>name -- ) \ (S: -- xt )
\
: ['] ' POSTPONE LITERAL ; IMMEDIATE

\
\ ... x TO name ...
\
\ (S: x <spaces>name -- )
\
\ @note
\
\ 	x name !
\
\ @see
\ 	VALUE
\
: TO ' >BODY ! ;

\
\ ... BEGIN ... AGAIN
\
\ ... BEGIN ... test UNTIL ...
\
\ ... BEGIN ... test WHILE ... REPEAT ...
\
\  (C: -- dest )
\
\ @see
\ 	A.3.2.3.2 Control-flow stack
\
: BEGIN >HERE ; IMMEDIATE

\
\ ... BEGIN ... AGAIN
\
\  (C: dest -- )
\
\ @see
\ 	A.3.2.3.2 Control-flow stack
\
: AGAIN POSTPONE _branch >HERE - , ; IMMEDIATE

\
\ ... BEGIN ... test UNTIL ...
\
\  (C: dest -- ) \ (S: flag -- )
\
\ @see
\ 	A.3.2.3.2 Control-flow stack
\
: UNTIL POSTPONE _branchz >HERE - , ; IMMEDIATE

\
\ ... AHEAD ... THEN ...
\
\  (C: -- forw )
\
\ @see
\ 	A.3.2.3.2 Control-flow stack
\
: AHEAD POSTPONE _branch >HERE 0 , ; IMMEDIATE

\
\ ... test IF ... THEN ...
\
\ ... test IF ... ELSE ... THEN ...
\
\  (C: -- forw ) \ (S: flag -- )
\
\ @see
\ 	A.3.2.3.2 Control-flow stack
\
\ @note
\ 	It's possible to put an IF...THEN (or IF...ELSE...THEN) statement
\ 	inside another IF...THEN statement, so long as every IF has one THEN.
\
\ 	DUP test1 IF
\ 		...
\ 	ELSE
\ 		DUP test2 IF
\ 			...
\ 		ELSE
\ 			...
\ 		THEN
\ 	THEN DROP
\
: IF POSTPONE _branchz >HERE 0 , ; IMMEDIATE

\
\ ... AHEAD ... THEN ...
\
\ ... test IF ... THEN ...
\
\ ... test IF ... ELSE ... THEN ...
\
\  (C: forw -- )
\
\ @see
\ 	A.3.2.3.2 Control-flow stack
\
: THEN				\  C: forw_off
	>HERE SWAP -		\  C: dist
	DUP			\  C: dist dist
	HERE SWAP -		\  C: dist forw_addr
	!			\  C: --
; IMMEDIATE

\
\ ... test IF ... ELSE ... THEN ...
\
\  (C: forw1 -- forw2 )
\
\ @see
\ 	A.3.2.3.2 Control-flow stack
\
: ELSE				\  C: forw1
	POSTPONE AHEAD		\  C: forw1 forw2
	1 CS-ROLL		\  C: forw2 forw1
	POSTPONE THEN		\  C: forw2
; IMMEDIATE

\
\ ... BEGIN ... test WHILE ... REPEAT ...
\
\  (C: dest -- forw dest ) \ (S: flag -- )
\
\ ... BEGIN ... test WHILE ... test WHILE ... REPEAT THEN ...
\
\ ... BEGIN ... test WHILE ... test WHILE ... AGAIN THEN THEN ...
\
\  Multiple WHILE possible to provide short-circuit testing, but each
\  additional WHILE needs a THEN in order to resolve each forward
\  reference remaining on the stack.
\
\ @see
\ 	A.3.2.3.2 Control-flow stack
\
: WHILE				\  C: dest
	POSTPONE IF		\  C: dest forw
	1 CS-ROLL		\  C: forw dest
; IMMEDIATE

\
\ ... BEGIN ... test WHILE ... REPEAT ...
\
\  (C: forw dest -- )
\
\ @see
\ 	A.3.2.3.2 Control-flow stack
\
: REPEAT			\  C: forw dest
	POSTPONE AGAIN		\  C: forw
	POSTPONE THEN		\  C: --
; IMMEDIATE

\
\ ... ABS ...
\
\ (S: n -- u )
\
: ABS DUP 0< IF NEGATE THEN ;

\
\ ... MAX ...
\
\ (S: n1 n2 -- n3 )
\
: MAX 2DUP < IF SWAP THEN DROP ;

\
\ ... MIN ...
\
\ (S: n1 n2 -- n3 )
\
: MIN 2DUP > IF SWAP THEN DROP ;

\
\ ... ?DUP ...
\
\ (S: x -- 0 | x x )
\
: ?DUP DUP 0<> IF DUP THEN ;

\
\ ... ( comment) ...
\
\ (S: ccc<paren>" -- )
\
: ( [CHAR] ) PARSE 2DROP ; IMMEDIATE

\
\ ... \ comment to end of line
\
\ (S: ccc<eol>" -- )
\
: \
	BLK @
	IF			( Block input source? )
	 >IN @ $3F OR 1+ >IN !	(   Advance >IN to next line in 16x64 block. )
	ELSE			( Streaming input... )
	 '\n' PARSE 2DROP	(  Skip up to and including newline. )
	THEN
; IMMEDIATE

VARIABLE catch_frame 0 catch_frame !

\ ... CATCH ...
\
\ ( i*x xt -- j*x 0 | i*x n )
\
: CATCH				\ S: xt   R: ip
	_dsp@ >R		\ S: xt   R: ip ds
	catch_frame @ >R	\ S: xt   R: ip ds cf
	_rsp@ catch_frame !	\ S: xt   R: ip ds cf
	EXECUTE			\ S: --   R: ip ds cf
	R> catch_frame !	\ S: --   R: ip ds
	R> DROP			\ S: --   R: ip
	0			\ S: 0    R: ip
;				\ S: 0    R: --

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
	 catch_frame @ _rsp!	\ S: n    R: ip ds cf
	 R> catch_frame !	\ S: n    R: ip ds
	 R> SWAP >R		\ S: ds   R: ip n
	 \ Restore data stack at start of CATCH
	 _dsp!			\ S: xt   R: ip n
	 DROP R>		\ S: n    R: ip
	THEN
;				\ S: 0 | n  R: --

\
\ ... ABORT ...
\
\  ( i*x -- ) ( R: j*x -- )
\
: ABORT -1 THROW ;

\
\ ... ABORT ...
\
\  ( -- ) ( R: i*x -- )
\
: QUIT -56 THROW ;

\
\ ... TYPE ...
\
\ (S: caddr u -- )
\
: TYPE
	BEGIN DUP 0> WHILE	\  S: caddr u
	 1- SWAP		\  S: u' caddr
	 DUP @ EMIT		\  S: u' caddr
	 CHAR+ SWAP		\  S: caddr' u'
	REPEAT 2DROP		\  S: --
;

\
\ ... SPACES ...
\
\ (S: n -- )
\
: SPACES
	BEGIN DUP 0> WHILE	\  S: n
	 SPACE 1-		\  S: n'
	REPEAT DROP		\  S: --
;

\
\ ...  U.  ...
\
\ (S: u -- )
\
: U. <# #S #> TYPE SPACE ;

\
\ ... . ...
\
\ (S: n -- )
\
: . DUP ABS <# #S SWAP SIGN #> TYPE SPACE ;

\
\ ... U.R  ...
\
\ (S: u n -- )
\
: U.R >R <# #S #> R> OVER - SPACES TYPE ;

\
\ ... .R  ...
\
\ (S: n n -- )
\
: .R >R DUP ABS <# #S SWAP SIGN #> R> OVER - SPACES TYPE ;

\
\ ... .( ccc) ...
\
\ (S: ccc<paren>) -- )
\
: .( [CHAR] ) PARSE TYPE ; IMMEDIATE

\
\ ... ? ...
\
\ (S: aaddr -- )
\
: ?
	BASE @ >R HEX
	DUP [CHAR] $ EMIT . SPACE @
	DUP [CHAR] $ EMIT . SPACE
	DUP DECIMAL [CHAR] # EMIT . SPACE
	OCTAL [CHAR] 0 EMIT . CR
	R> BASE !
;

\
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
;				\  S: -- R: j*x +n

\
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
;				\  S: ip j*x n R:

\
\ ... limit first DO ... LOOP ...
\
\ (C: -- dest )(R: -- count) || (S: limit first -- ) (R: -- limit first )
\
: DO				\ C: --  R: ip
	POSTPONE 2>R		\ S: --  R: limit first
	R> 0 >R	>R		\ C: --  R: 0 ip
	POSTPONE BEGIN		\ C: dest R: 0 ip
; IMMEDIATE

\
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
; IMMEDIATE

\
\ ... limit first DO ... IF ... LEAVE THEN ... LOOP ...
\
\ (C: dest -- dest ) (R: n*forw n -- n'*forw n' )
\
: LEAVE				\ C: dest  R: n*forw n ip
	R> R> 1+		\ C: dest ip n'  R: n*forw
	POSTPONE AHEAD		\ C: dest ip n' forw  R: n*forw
	>R >R >R		\ C: dest  R: n'*forw n' ip
; IMMEDIATE

\
\ ... limit first DO ... test ?LEAVE ... LOOP ...
\
\ (C: dest -- dest ) (R: n*forw n -- n'*forw n' )
\
: ?LEAVE			\ C: dest flag  R: n*forw n ip
	POSTPONE IF		\ C: dest  R: n*forw n ip
	POSTPONE LEAVE		\ C: dest  R: n'*forw n' ip
	POSTPONE THEN		\ C: --  R: n'*forw n' ip
;

\
\ : X ... limit first DO ... test IF ... UNLOOP EXIT THEN ... LOOP ... ;
\
\ (S: --  ) (R: limit index ip -- ip )
\
: UNLOOP R> 2R> 2DROP >R ;

\
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

\
\ ... limit first DO ... LOOP ...
\
\ (C: dest -- ) (R: n*forw n ip -- ip )
\
: LOOP				\ C: dest  R: n*forw n ip
	POSTPONE _loop_inc_test
	POSTPONE UNTIL		\ C: --  R: n*forw n ip

	\ Resolve LEAVE forward references.
	R> R>			\ C: ip n  R: n*forw
	BEGIN
	 DUP 0>			\ C: ip n flag  R: n*forw
	WHILE			\ C: ip n  R: n*forw
	 1-			\ C: ip n' R: n*forw
	 R>			\ C: ip n' forw  R: n'*forw
	 POSTPONE THEN		\ C: ip n'  R: n'*forw
	REPEAT
	DROP >R			\ C: -- R: ip

	\  LEAVE branches to just after UNTIL and before UNLOOP.
	POSTPONE UNLOOP
; IMMEDIATE

\
\ ... limit first DO ... LOOP ...
\
\ (S: -- index ) (R: limit index ip -- limit index ip )
\
: I R> R@ SWAP >R ;

\
\ ... limit first DO ... LOOP ...
\
\ (S: -- index1 ) (R: limit1 index1 limit2 index2 ip -- limit1 index1 limit2 index2 ip )
\
: J				\ S: --  R: l1 i1 l2 i2 ip
	R> R> R> R@		\ S: ip i2 l2 i1  R: l1 i1
	3 LLOR			\ S: i1 ip l2 i2  R: l1 i1
\	3 ROLL 3 ROLL 3 ROLL
	>R >R >R		\ S: i1  R: l1 i1 l2 i2 ip
;

-1 1 RSHIFT CONSTANT int_max	\ 0x7F...FF
int_max INVERT CONSTANT int_min	\ 0x80...00

\
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
	SWAP int_min SWAP -	\ S: ip x' l'  R: l x'
	DUP int_min AND		\ S: ip x' l' sign  R: l x'
	>R + int_min AND R>	\ S: ip cross sign  R: l x'
	<> SWAP >R		\ S: flag  R: l x' ip
;

\
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
	BEGIN
	 DUP 0>			\ C: ip n flag  R: n*forw
	WHILE			\ C: ip n  R: n*forw
	 1-			\ C: ip n' R: n*forw
	 R>			\ C: ip n' forw  R: n'*forw
	 POSTPONE THEN		\ C: ip n'  R: n'*forw
	REPEAT
	DROP >R			\ C: -- R: ip

	\  LEAVE branches to just after UNTIL and before UNLOOP.
	POSTPONE UNLOOP
; IMMEDIATE

\
\ ... reserve ...
\
\ (S: u -- aaddr )
\
\ @note
\ 	During the compiliation of a word in C based implementations
\ 	data-space regions may be relocated when they are enlarged,
\ 	thus invalidating previous values of HERE.  Therefore:
\
\ 	... HERE 100 ALLOT ... \ fill in allotment
\
\ 	Should ALLOT enlarge and relocate the data-space, the address
\ 	saved by HERE on the stack will now point into invalid memory.
\
\ 	With RESERVE the address of the region just reserved is on
\ 	top of the stack insuring that the address is valid until the
\ 	next enlargement of the data-space by RESERVE, comma (,),
\ 	c-comma (C,), compile-comma (COMPILE,), or ALIGN.
\
: reserve DUP ALLOT HERE SWAP - ;

\
\ ( caddr u -- )
\
: _append_string DUP >R reserve R> MOVE 0 C, ;

\
\ ... ," ccc" ...
\
\ (S: -- )
\
\ Append NUL terminated string to the most recent word's data space.
\
\ 	CREATE greet ," Hello world.\n"
\
: ," [CHAR] " parse-escape _append_string ALIGN ;

\
\ ( caddr -- )
\
\ Print a NUL terminated string.
\
\ 	CREATE greet ," Hello world.\n"
\	greet TYPE0
\
: type0 BEGIN DUP @ ?DUP WHILE EMIT 1+ REPEAT DROP ;

\
\ ( caddr -- u )
\
\ String length of NUL terminated string.
\
: strlen DUP BEGIN DUP @ WHILE 1+ REPEAT SWAP - ;

\
\ ... _slit ...
\
\ (S: -- caddr u )
\
\ @note
\ 	The caller's return address is used to find and compute the
\ 	address and length of the string stored within the word.
\ 	It is then modified to point to just after the string.
\
: _slit					\ S: -- R: ip
	R@				\ S: ip R: ip
	@				\ S: u  R: ip
	R> CELL+ SWAP 2DUP		\ S: caddr u caddr u R: --
	CHAR+				\ account for NUL from _append_string
	CHARS + ALIGNED			\ S: caddr u ip' R: --
	>R				\ S: caddr u R: ip'
;

\
\ (S: caddr u -- ; -- caddr u )
\
: SLITERAL
	POSTPONE _slit DUP ,	\ S: caddr u
	_append_string ALIGN		\ S: --
; IMMEDIATE

\
\ ... S" ccc" ...
\
\ (C: ccc<quote>" -- ) || (S: -- caddr u )
\
: S" [CHAR] " PARSE STATE @ IF POSTPONE SLITERAL THEN ; IMMEDIATE

\
\ ... S\" ccc" ...
\
\ (C: ccc<quote>" -- ) || (S: -- c-addr u )
\
: S\" [CHAR] " parse-escape STATE @ IF POSTPONE SLITERAL THEN ; IMMEDIATE

\
\ ... ." ccc" ...
\
\ (S: ccc<quote>" -- )
\
: ." POSTPONE S" POSTPONE TYPE ; IMMEDIATE

\
\  : X ... test ABORT" message" ...
\
\  (C: ccc<quote>" -- ) \ (S: i*x x1 --  | i*x ) ( R: j*x --  | j*x )
\
: ABORT" IF POSTPONE ." SPACE -2 THROW THEN ;

\
\ ... SCR ...
\
\ (S: -- aaddr )
\
VARIABLE SCR 0 SCR !

\
\ ... LIST ...
\
\ (S: u -- )
\
: LIST				\ S: u
	DUP SCR !		\ S: u
	BLOCK			\ S: aaddr
	16 0 DO
	 I 1+ 2 .R
	 [CHAR] | EMIT
	 DUP 64 TYPE		\ S: aaddr
	 [CHAR] | EMIT CR
	 64 CHARS +		\ S: aaddr'
	LOOP DROP		\ S: --
;

\
\ ... FLUSH ...
\
\ (S: -- )
\
: FLUSH SAVE-BUFFERS EMPTY-BUFFERS ;

\
\ ... LIST+ ...
\
\ (S: -- )
\
: LIST+ SCR @ 1+ LIST ;

\
\ ... AT-XY ...
\
\ (S: column row -- )
\
\ @note
\ 	ANSI / VT100 terminal assumed.
\
: AT-XY
	S\" \e[" TYPE
	1+ 0 U.R
	';' EMIT
	1+ 0 U.R
	'H' EMIT
;

\
\ ... PAGE ...
\
\ (S: -- )
\
\ @note
\ 	ANSI / VT100 terminal assumed.
\
: PAGE 0 0 AT-XY S\" \e[0J" TYPE ;

\
\ ... INCLUDE filename ...
\
\ (S: <spaces>filename" -- )
\
: INCLUDE PARSE-NAME INCLUDED ;

\
\ ... INCL filename ...
\
\ (S: <spaces>filename" -- )
\
\ @note
\	Convenience that marks word boundary before the include.
\
: incl S" MARKER rm_incl" EVALUATE INCLUDE ;

\
\ ... THRU ...
\
\ (S: start end -- )
\
: THRU				\  S: start end
	1+ SWAP			\  S: end' start
	DO			\  S: --
	 I LOAD
	LOOP
;

\
\ ... BUFFER: name
\
\ ( u "<spaces>name" -- ; -- aaddr )
\
\ @note
\	Choose not to use ALLOCATE since removing marked dictionary words
\	would require extra handling.
\
: BUFFER: CREATE ALLOT ;

MARKER rm_user_words

\ .( Post4 Copyright 2007, 2018 by Anthony Howe.  All rights reserved. )
