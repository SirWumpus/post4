MARKER rm_core_words

: \  '\n' PARSE DROP DROP ; IMMEDIATE

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
\ @standard ANS-Forth 1994, Tools
\
: .S _ds _stack_dump ;

\
\ ... .RS ...
\
\ ( -- )
\
\ @standard p4
\
: .RS _rs _stack_dump ;

\
\ ... BYE ...
\
\  ( i*x -- i*x )
\
\ @standard ANS-Forth 1994, Core
\
: BYE -256 THROW ;

\
\ ... ABORT ...
\
\  ( i*x –– ) ( R: j*x –– )
\
\ @standard ANS-Forth 1994, Core
\
: ABORT -1 THROW ;

\
\ ... ABORT ...
\
\  ( –– ) ( R: i*x –– )
\
\ @standard ANS-Forth 1994, Core
\
: QUIT -56 THROW ;

\
\  value CONSTANT name
\
\  (C: x <spaces>name -- ) \ (S: -- x )
\
\ @standard ANS-Forth 1994, Core
\
: CONSTANT CREATE , DOES> @ ;

\
\ ... FALSE ...
\
\ (S: -- 0 )
\
\ @standard ANS-Forth 1994, Core
\
0 CONSTANT FALSE

\
\ ... TRUE ...
\
\ (S: -- 1 )
\
\ @standard ANS-Forth 1994, Core
\
FALSE INVERT CONSTANT TRUE

\
\ ... /CHAR ...
\ ... /CELL ...
\
\ (S: -- n )
\
\ @standard p4
\
1 CHARS CONSTANT /CHAR
1 CELLS CONSTANT /CELL

\
\ ... BL ...
\
\ (S: -- ' ' )
\
\ @standard ANS-Forth 1994, Core
\
'\s' CONSTANT BL

\
\  VARIABLE name
\
\  (C: <spaces>name -- ) \ (S: -- aaddr )
\
\ @standard ANS-Forth 1994, Core
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
\ @standard ANS-Forth 1994, Core
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
\ @standard ANS-Forth 1994, Core
\
: CELL+ /CELL + ;

\
\ ... ALIGNED ...
\
\ (S: addr -- aaddr )
\
\ @standard ANS-Forth 1994, Core
\
\  (addr + (pow2-1)) & -pow2
\
: ALIGNED /CELL 1 - + /CELL NEGATE AND ;

\
\ ... CHAR+ ...
\
\ (S: c-addr1 -- c-addr2 )
\
\ @standard ANS-Forth 1994, Core
\
: CHAR+ /CHAR + ;

\
\ ... DECIMAL ...
\
\ (S: -- )
\
\ @standard ANS-Forth 1994, Core
\
: DECIMAL #10 BASE ! ;

\
\ ... HEX ...
\
\ (S: -- )
\
\ @standard ANS-Forth 1994, Core
\
: HEX #16 BASE ! ;

\
\ ... OCTAL ...
\
\ (S: -- )
\
\ @standard p4
\
: OCTAL #8 BASE ! ;

\
\ ... NIP ...
\
\ (S: x1 x2 -- x2 )
\
\ @standard ANS-Forth 1994, Core
\
: NIP SWAP DROP ;

\
\ ... OVER ...
\
\ (S: x1 x2 -- x1 x2 x1 )
\
\ @standard ANS-Forth 1994, Core
\
: OVER 1 PICK ;

\
\ ... ROT ...
\
\ (S: a b c -- b c a )
\
\ @standard ANS-Forth 1994, Core
\
: ROT 2 ROLL ;

\
\ ... TUCK ...
\
\ (S: x1 x2 -- x2 x1 x2 )
\
\ @standard ANS-Forth 1994, Core
\
: TUCK SWAP OVER ;

\
\ ... +! ...
\
\ (S: n addr --  )
\
\ @standard ANS-Forth 1994, Core
\
: +! DUP @ ROT + SWAP ! ;

\
\ ... 1+ ...
\
\ (S: nu1 -- nu2 )
\
\ @standard ANS-Forth 1994, Core
\
: 1+ 1 + ;

\
\ ... 1- ...
\
\ (S: nu1 -- nu2 )
\
\ @standard ANS-Forth 1994, Core
\
: 1- 1 - ;

\
\ ... 2! ...
\
\ (S: lo hi aaddr -- )
\
\ @standard ANS-Forth 1994, Core
\
: 2! SWAP OVER ! CELL+ ! ;

\
\ ... 2@ ...
\
\ (S: aaddr -- lo hi )
\
\ Fetch from aaddr the two cells, hi lo, and place on stack lo hi.
\
\ @standard ANS-Forth 1994, Core
\
: 2@ DUP CELL+ @ SWAP @ ;

\
\ ... 2* ...
\
\ (S: x1 -- x2 )
\
\ @standard ANS-Forth 1994, Core
\
: 2* 1 LSHIFT ;

\
\ ... 2/ ...
\
\ (S: x1 -- x2 )
\
\ @standard ANS-Forth 1994, Core
\
: 2/ 1 RSHIFT ;

\
\ ... 2DROP ...
\
\ (S: x1 x2 -- )
\
\ @standard ANS-Forth 1994, Core
\
: 2DROP DROP DROP ;

\
\ ... 2DUP ...
\
\ (S: x1 x2 -- x1 x2 x1 x2 )
\
\ @standard ANS-Forth 1994, Core
\
: 2DUP OVER OVER ;

\
\ ... 2OVER ...
\
\ (S: x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
\
\ @standard ANS-Forth 1994, Core
\
: 2OVER 3 PICK 3 PICK ;

\
\ ... 2SWAP ...
\
\ (S: x1 x2 x3 x4 -- x3 x4 x1 x2 )
\
\ @standard ANS-Forth 1994, Core
\
: 2SWAP 3 ROLL 3 ROLL ;

\
\ ... 2>R ...
\
\ (S: x1 x2 -- )(R: -- x1 x2 )
\
\ @standard ANS-Forth 1994, Core Ext
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
\ @standard ANS-Forth 1994, Core Ext
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
\ @standard ANS-Forth 1994, Core Ext
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
\ @standard ANS-Forth 1994, Core
\
: 0<> 0= 0= ;

\
\ ... 0> ...
\
\ (S: n -- flag )
\
\ @standard ANS-Forth 1994, Core Ext
\
: 0> 0 SWAP - 0< ;

\
\ ... = ...
\
\ (S: nu1 nu2 -- flag )
\
\ @standard ANS-Forth 1994, Core
\
: = - 0= ;

\
\ ... <> ...
\
\ (S: nu1 nu2 -- flag )
\
\ @standard ANS-Forth 1994, Core
\
: <> = 0= ;

\
\ ... < ...
\
\ (S: n1 n2 -- flag )
\
\ @standard ANS-Forth 1994, Core
\
: < - 0< ;

\
\ ... > ...
\
\ (S: n1 n2 -- flag )
\
\ @standard ANS-Forth 1994, Core
\
: > - 0> ;

\
\ ... <= ...
\
\ (S: n1 n2 -- flag )
\
\ @standard p4
\
: <= - DUP 0= SWAP 0< OR ;

\
\ ... >= ...
\
\ (S: n1 n2 -- flag )
\
\ @standard p4
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
\ @standard Forth 200x Draft 16.1, Core Ext
\
: WITHIN OVER - >R - R> U< ;

\
\ ... CR ...
\
\ (S: -- )
\
\ @standard ANS-Forth 1994, Core
\
: CR '\r' EMIT '\n' EMIT ;

\
\ ... DEPTH ...
\
\  ( -- u )
\
\ @standard ANS-Forth 1994, Core
\
: DEPTH _ds NIP ;

\
\ ... FILL ...
\
\ (S: c-addr u char -- )
\
\ @standard ANS-Forth 1994, String
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
\ @standard ANS-Forth 1994, String
\
: BLANK BL FILL ;

\
\ ... SPACE ...
\
\ (S: -- )
\
\ @standard ANS-Forth 1994, Core
\
: SPACE BL EMIT ;

\
\ ...  POSTPONE  ...
\
\  (C: <spaces>name -- )
\
\ @standard ANS-Forth 1994, Core
\
: POSTPONE ' COMPILE, ; IMMEDIATE

\
\ ...  CHAR  ...
\
\ (S: <spaces>name -- char )
\
\ @standard ANS-Forth 1994, Core
\
: CHAR PARSE-NAME DROP C@ ;

\
\ ...  [CHAR]  ...
\
\  (C: <spaces>name -- ) \ (S: -- char )
\
\ @standard ANS-Forth 1994, Core
\
: [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE

\
\ ... POSTPONE name ...
\
\  (C: <spaces>name -- ) \ (S: -- xt )
\
\ @standard ANS-Forth 1994, Core
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
\ @standard ANS-Forth 1994, Core, Local
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
\ @standard ANS-Forth 1994, Core
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
\ @standard ANS-Forth 1994, Core
\
\ @see
\ 	A.3.2.3.2 Control-flow stack
\
: AGAIN ['] _branch COMPILE, >HERE - , ; IMMEDIATE

\
\ ... BEGIN ... test UNTIL ...
\
\  (C: dest -- ) \ (S: flag -- )
\
\ @standard ANS-Forth 1994, Core
\
\ @see
\ 	A.3.2.3.2 Control-flow stack
\
: UNTIL ['] _branchz COMPILE, >HERE - , ; IMMEDIATE

\
\ ... AHEAD ... THEN ...
\
\  (C: -- forw )
\
\ @standard ANS-Forth 1994, Core
\
\ @see
\ 	A.3.2.3.2 Control-flow stack
\
: AHEAD ['] _branch COMPILE, >HERE 0 , ; IMMEDIATE

\
\ ... test IF ... THEN ...
\
\ ... test IF ... ELSE ... THEN ...
\
\  (C: -- forw ) \ (S: flag -- )
\
\ @standard ANS-Forth 1994, Core
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
: IF ['] _branchz COMPILE, >HERE 0 , ; IMMEDIATE

\
\ ... AHEAD ... THEN ...
\
\ ... test IF ... THEN ...
\
\ ... test IF ... ELSE ... THEN ...
\
\  (C: forw -- )
\
\ @standard ANS-Forth 1994, Core
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
\ @standard ANS-Forth 1994, Core
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
\ @standard ANS-Forth 1994, Core
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
\ @standard ANS-Forth 1994, Core
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
\ @standard ANS-Forth 1994, Core
\
: ABS DUP 0< IF NEGATE THEN ;

\
\ ... MAX ...
\
\ (S: n1 n2 -- n3 )
\
\ @standard ANS-Forth 1994, Core
\
: MAX 2DUP < IF SWAP THEN DROP ;

\
\ ... MIN ...
\
\ (S: n1 n2 -- n3 )
\
\ @standard ANS-Forth 1994, Core
\
: MIN 2DUP > IF SWAP THEN DROP ;

\
\ ... ?DUP ...
\
\ (S: x -- 0 | x x )
\
\ @standard ANS-Forth 1994, Core
\
: ?DUP DUP 0<> IF DUP THEN ;

\
\ ... ( comment) ...
\
\ (S: ccc<paren>" -- )
\
\ @standard ANS-Forth 1994, Core, File
\
: ( [CHAR] ) PARSE 2DROP ; IMMEDIATE

\
\ ... \ comment to end of line
\
\ (S: ccc<eol>" -- )
\
\ @standard ANS-Forth 1994, Core, Block
\
: \
	BLK @
	IF			( Block input source? )
	 >IN @ $3F OR 1+ >IN !	(   Advance >IN to next line in 16x64 block. )
	ELSE			( Streaming input... )
	 '\n' PARSE 2DROP	(  Skip up to and including newline. )
	THEN
; IMMEDIATE

\
\ ... TYPE ...
\
\ (S: caddr u -- )
\
\ @standard ANS-Forth 1994, Core
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
\ @standard ANS-Forth 1994, Core
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
\ @standard ANS-Forth 1994, Core
\
: U. <# #S #> TYPE SPACE ;

\
\ ... . ...
\
\ (S: n -- )
\
\ @standard ANS-Forth 1994, Core
\
: . DUP ABS <# #S SWAP SIGN #> TYPE SPACE ;

\
\ ... U.R  ...
\
\ (S: u n -- )
\
\ @standard ANS-Forth 1994, Core
\
: U.R >R <# #S #> R> OVER - SPACES TYPE SPACE ;

\
\ ... .R  ...
\
\ (S: n n -- )
\
\ @standard ANS-Forth 1994, Core
\
: .R >R DUP ABS <# #S SWAP SIGN #> R> OVER - SPACES TYPE SPACE ;

\
\ ... .( ccc) ...
\
\ (S: ccc<paren>) -- )
\
\ @standard ANS-Forth 1994, Core, extended
\
: .( [CHAR] ) PARSE TYPE ; IMMEDIATE

\
\ ... ? ...
\
\ (S: aaddr -- )
\
\ @standard ANS-Forth 1994, Tools
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
\ @standard ANS-Forth 1994, Tools
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
\ @standard ANS-Forth 1994, Tools
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
\ @standard ANS-Forth 1994, Core
\
: DO				\ C: --  R: ip
	['] 2>R COMPILE,	\ S: --  R: limit first
	R> 0 >R	>R		\ C: --  R: 0 ip
	POSTPONE BEGIN		\ C: dest R: 0 ip
; IMMEDIATE

\
\ ... limit first ?DO ... LOOP ...
\
\ (C: -- dest ) (R: -- forw 1 ) || (S: limit first -- ) (R: -- limit first )
\
\ @standard ANS-Forth 1994, Core
\
: ?DO				\ C: --  R: ip
	['] 2>R COMPILE,	\ S: --  R: limit first
	['] 2R@ COMPILE,	\ S: limit first  R: limit first
	['] <> COMPILE,		\ S: flag  R: limit first
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
\ @standard ANS-Forth 1994, Core
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
\ @standard ANS-Forth 1994, Core
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
\ @standard ANS-Forth 1994, Core
\
: UNLOOP R> 2R> 2DROP >R ;

\
\ ... limit first DO ... LOOP ...
\
\ (S: -- flag ) (R: limit index ip -- limit index' ip )
\
\ @standard internal
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
\ @standard ANS-Forth 1994, Core
\
: LOOP				\ C: dest  R: n*forw n ip
	['] _loop_inc_test COMPILE,
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
	['] UNLOOP COMPILE,
; IMMEDIATE

\
\ ... limit first DO ... LOOP ...
\
\ (S: -- index ) (R: limit index ip -- limit index ip )
\
\ @standard ANS-Forth 1994, Core
\
: I R> R@ SWAP >R ;

\
\ ... limit first DO ... LOOP ...
\
\ (S: -- index1 ) (R: limit1 index1 limit2 index2 ip -- limit1 index1 limit2 index2 ip )
\
\ @standard ANS-Forth 1994, Core
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
\ @standard internal
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
\ @standard ANS-Forth 1994, Core
\
: +LOOP				\ C: dest  R: n*forw n ip
	\  Loop increment and test.
	['] _loop_step_test COMPILE,
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
	['] UNLOOP COMPILE,
; IMMEDIATE

MARKER rm_untested

\
\ ... reserve ...
\
\ (S: u -- aaddr )
\
\ @standard p4
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
\ ... _slit ...
\
\ (S: -- caddr u )
\
\ @standard internal
\
\ @note
\ 	The caller's return address is used to find and compute the
\ 	address and length of the string stored within the word.
\ 	It is then modified to point to just after the string.
\
: _slit					\  S: -- R: ip
	R@				\  S: ip R: ip
	@				\  S: u  R: ip
	R> SWAP	2DUP			\  S: caddr u caddr u R: --
	+ ALIGNED			\  S: caddr u ip' R: --
	>R				\  S: caddr u R: ip'
;

\
\ ... S" ccc" ...
\
\  (C: ccc<quote>" -- ) \ (S: -- c-addr u )
\
\ @standard ANS-Forth 1994, Core, File, extended
\
: S"
	[CHAR] " PARSE			\  S: caddr u
	STATE @ 0= IF			\  When interpreting, parse and ignore.
	 2DROP EXIT			\  S: --
	THEN				\  Otherwise compile into word the string.
	['] _slit COMPILE, DUP ,	\  S: caddr u
	DUP >R ALIGNED			\  S: caddr u' R: u
	reserve R>			\  S: caddr addr u
	MOVE				\  S: --
; IMMEDIATE

QUIT

\
\ ... S\" ccc" ...
\
\  (C: ccc<quote>" -- ) \ (S: -- c-addr u )
\
\ @standard ANS-Forth 1994, Core, File, extended
\
: S\"
	[CHAR] " PARSE-ESCAPE		\  S: caddr u
	STATE @ 0= IF
	 2DROP EXIT			\  S: --
	THEN
	['] _slit COMPILE, DUP ,	\  S: caddr u
	DUP >R				\  S: caddr u R: u
	1+ ALIGNED			\  S: caddr u' R: u
	reserve				\  S: caddr addr R: u
	R>				\  S: caddr addr u
	MOVE				\  S: --
; IMMEDIATE

\
\ ... ." ccc" ...
\
\ (S: ccc<quote>" -- )
\
\ @standard ANS-Forth 1994, Core, extended
\
: ." POSTPONE S" ['] TYPE COMPILE, ; IMMEDIATE

\
\ ... AT-XY ...
\
\ (S: column row -- )
\
\ @standard ANS-Forth 1994, Facility
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
\ @standard ANS-Forth 1994, Facility
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
\ @standard p4
\
: INCLUDE PARSE-NAME INCLUDED ;

\
\ ... SCR ...
\
\ (S: -- aaddr )
\
\ @standard ANS-Forth 1994, Block
\
VARIABLE SCR 0 SCR !

\
\ ... LIST ...
\
\ (S: u -- )
\
\ @standard ANS-Forth 1994, Block
\
: LIST				\  S: u
	DUP SCR !		\  S: u
	BLOCK			\  S: aaddr
	#16 0 DO
	 I 2 .R
	 [CHAR] | EMIT
	 DUP 64 TYPE		\  S: aaddr
	 [CHAR] | EMIT CR
	 #64 CHARS +		\  S: aaddr'
	LOOP DROP		\  S: --
;

\
\ ... LIST+ ...
\
\ (S: -- )
\
\ @standard p4
\
: LIST+ SCR @ 1+ LIST ;

\
\ ... THRU ...
\
\ (S: start end -- )
\
\ @standard ANS-Forth 1994, Block
\
: THRU				\  S: start end
	1+ SWAP			\  S: end' start
	DO			\  S: --
	 I LOAD
	LOOP
;

\
\  : X ... test ABORT" message" ...
\
\  (C: ccc<quote>" -- ) \ (S: i*x x1 --  | i*x ) ( R: j*x --  | j*x )
\
\ @standard ANS-Forth 1994
\
: ABORT" IF ." -2 THROW THEN ;

MARKER rm_user_words

.( Post4 Copyright 2007, 2018 by Anthony Howe.  All rights reserved. ) CR
