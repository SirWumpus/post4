MARKER rm_core_words

: \ '\n' PARSE DROP DROP ; IMMEDIATE

\ Post4 Copyright 2007, 2019 by Anthony Howe.  All rights reserved.

\ ... .S ...
\
\ ( -- )
\
: .S 'd' EMIT 's' EMIT '\n' EMIT _ds _stack_dump ;

\ ... .RS ...
\
\ ( -- )
\
: .RS 'r' EMIT 's' EMIT '\n' EMIT _rs 1 - _stack_dump ;

\ ... reserve ...
\
\ (S: u -- addr )
\
\ @note
\	During the compiliation of a word in C based implementations
\	data-space regions may be relocated when they are enlarged,
\	thus invalidating previous values of HERE.  Therefore:
\
\	... HERE 100 ALLOT ... \ fill in allotment
\
\	Should ALLOT enlarge and relocate the data-space, the address
\	saved by HERE on the stack will now point into invalid memory.
\
\	With RESERVE the address of the region just reserved is on
\	top of the stack insuring that the address is valid until the
\	next enlargement of the data-space by RESERVE, comma (,),
\	c-comma (C,), or ALIGN.
\
: reserve DUP ALLOT HERE SWAP - ;

\ ( x -- )
: , 1 CELLS reserve ! ;

\ ( char -- )
: C, 1 CHARS reserve C! ;

\ ( xt -- )
: COMPILE, , ;

\ value CONSTANT name
\
\ (C: x <spaces>name -- ) \ (S: -- x )
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

\ ... /PAD ...
\
\ ( -- n )
\ Size must be at least 84 characters.
\
128 CONSTANT /PAD

_ds_size CONSTANT STACK-CELLS
_rs_size CONSTANT RETURN-STACK-CELLS

\ ... PAD ...
\
\ ( -- )
\
CREATE PAD /PAD CHARS ALLOT

\ VARIABLE name
\
\ (C: <spaces>name -- ) \ (S: -- aaddr )
\
: VARIABLE CREATE 0 , ;

: [ FALSE STATE ! ; IMMEDIATE
: ] TRUE STATE ! ;

\ ... CELL+ ...
\
\ (S: aaddr1 -- aaddr2 )
\
: CELL+ /CELL + ;
: CELL- /CELL - ;

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

\ ... S>D ...
\
\ ( n -- d )
\
\ @note
\	This assumes that 0< returns a proper flag (all bits 1) for true
\	as oppose simply any non-zero value for true.
\
: S>D DUP 0< SWAP ;		\ Sign extend into high word.

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
: 2/ 1 RSHIFT ;

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
;

\ ... 2R> ...
\
\ (S: -- x1 x2 )(R: x1 x2 -- )
\
: 2R>
	R> R> R>		\ S: ip x2 x1  R: --
	ROT			\ S: x2 x1 ip  R: --
	>R SWAP			\ S: x1 x2  R: ip
;

\ ... 2R@ ...
\
\ (S: -- x1 x2 )(R: x1 x2 -- x1 x2 )
\
: 2R@
	R> 2R>			\ S: ip x1 x2  R: --
	2DUP 2>R		\ S: ip x1 x2  R: x1 x2
	ROT >R			\ S: x1 x2  R: x1 x2 ip
;

\ ... */ ...
\
\ (S: n1 n2 dsor -- quot )
\
: */ >R * R> / ;

\ ... */MOD ...
\
\ (S: n1 n2 dsor -- rem quot )
\
: */MOD >R * R> /MOD ;

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

\ ... < ...
\
\ (S: n1 n2 -- flag )
\
: < - 0< ;

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
: <= - DUP 0= SWAP 0< OR ;

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

\ ... CR ...
\
\ (S: -- )
\
: CR '\r' EMIT '\n' EMIT ;

\ ... DEPTH ...
\
\  ( -- u )
\
: DEPTH _ds NIP ;

\ ... SPACE ...
\
\ (S: -- )
\
: SPACE BL EMIT ;

\ ... COUNT ...
\
\ (S: caddr1 -- caddr2 u )
\
: COUNT DUP C@ SWAP CHAR+ SWAP ;

\ ...  CHAR  ...
\
\ (S: <spaces>name -- char )
\
: CHAR PARSE-NAME DROP C@ ;

\ ... : name ... [ x ] LITERAL ... ;
\
\ (S: x -- )
\
: LITERAL POSTPONE _lit , ; IMMEDIATE

\ ...  [CHAR]  ...
\
\  (C: <spaces>name -- ) \ (S: -- char )
\
: [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE

\ ... ['] name ...
\
\  (C: <spaces>name -- ) \ (S: -- xt )
\
: ['] ' POSTPONE LITERAL ; IMMEDIATE

\ ... BEGIN ... AGAIN
\ ... BEGIN ... test UNTIL ...
\ ... BEGIN ... test WHILE ... REPEAT ...
\
\  (C: -- dest )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: BEGIN >HERE ; IMMEDIATE

\ ... BEGIN ... AGAIN
\
\  (C: dest -- )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: AGAIN POSTPONE _branch >HERE - , ; IMMEDIATE

\ ... BEGIN ... test UNTIL ...
\
\  (C: dest -- ) \ (S: flag -- )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: UNTIL POSTPONE _branchz >HERE - , ; IMMEDIATE

\ ... AHEAD ... THEN ...
\
\  (C: -- forw )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: AHEAD POSTPONE _branch >HERE 0 , ; IMMEDIATE

\ ... test IF ... THEN ...
\ ... test IF ... ELSE ... THEN ...
\
\  (C: -- forw ) \ (S: flag -- )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
\ @note
\	It's possible to put an IF...THEN (or IF...ELSE...THEN) statement
\	inside another IF...THEN statement, so long as every IF has one THEN.
\
\	DUP test1 IF
\	  ...
\	ELSE
\	  DUP test2 IF
\	    ...
\	  ELSE
\	    ...
\	  THEN
\	THEN DROP
\
: IF POSTPONE _branchz >HERE 0 , ; IMMEDIATE

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
; IMMEDIATE

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
; IMMEDIATE

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
; IMMEDIATE

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
; IMMEDIATE

\
\ (R: -- ip )
\
: RECURSE
	POSTPONE _call
	>HERE NEGATE ,
; IMMEDIATE

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

\ ... ?DUP ...
\
\ (S: x -- 0 | x x )
\
: ?DUP DUP IF DUP THEN ;

\ (S: i*x i -- )
: n,
	DUP ,
	BEGIN ?DUP WHILE
	  1- SWAP ,
	REPEAT
;

\ (S: aaddr -- i*x )
: n@
	DUP @ SWAP		\ S: n aaddr
	OVER CELLS + SWAP	\ S: aaddr' n
	BEGIN ?DUP WHILE
	  1- >R
	  DUP @ SWAP CELL-
	  R>
	REPEAT DROP
;

\ (S: i*x aaddr -- )
: n!
	DUP @
	BEGIN ?DUP WHILE
	  1- >R
	  CELL+ TUCK !
	  R>
	REPEAT DROP
;

\  value VALUE name
\
\  (C: x <spaces>name -- ) \ (S: -- x )
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
: VALUE CREATE 1 n, DOES> n@ ;	\ CELL+ @
: 2VALUE CREATE 2 n, DOES> n@ ;	\ CELL+ 2@

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
	ELSE
	  n!
	THEN
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
	BEGIN
	  source-remaining 0= IF
	    2DROP TRUE EXIT	\ empty input buffer
	  THEN
	  1 >IN +!
	  C@ OVER = IF
	    DROP FALSE EXIT	\ input char matches delim
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

\ ... ABORT ...
\
\  ( i*x -- ) ( R: j*x -- )
\
: ABORT -1 THROW ;

\ ... ABORT ...
\
\  ( -- ) ( R: i*x -- )
\
: QUIT -56 THROW ;

\ ... HOLDS ...
\
\ (S: caddr u -- )
\
: HOLDS BEGIN DUP WHILE 1- 2DUP + C@ HOLD REPEAT 2DROP ;

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
	2 PICK			\  S: caddr u char caddr
	C! 1- OVER		\  S: caddr u' caddr
	CHAR+ SWAP		\  S: caddr caddr' u
	CMOVE			\  S: --
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

\ ... char WORD ...
\
\ (S: char "<chars>ccc<char>" -- caddr )
\
: WORD				\ S: char
	PARSE 			\ S: caddr u
	>R R@ OVER		\ S: caddr u caddr R: u
	DUP DUP CHAR+ R>	\ S: caddr u caddr caddr caddr' u
	CMOVE>			\ S: caddr u caddr
	C!			\ S: caddr
;

\ ...  U.  ...
\
\ (S: u -- )
\
: U. <# #S #> TYPE SPACE ;

\ ... . ...
\
\ (S: n -- )
\
: . DUP ABS <# #S SWAP SIGN #> TYPE SPACE ;

\ ... U.R  ...
\
\ (S: u n -- )
\
: U.R >R <# #S #> R> OVER - SPACES TYPE ;

\ ... .R  ...
\
\ (S: n n -- )
\
: .R >R DUP ABS <# #S SWAP SIGN #> R> OVER - SPACES TYPE ;

\ ( delim -- bool )
\
\ Scan the input buffer character at a time until either the input
\ is exhusted, returning true; or an input character matches delim,
\ returning false.
\
: emit-more
	BEGIN
	  source-remaining 0= IF
	    CR
	    2DROP TRUE EXIT	\ empty input buffer
	  THEN
	  1 >IN +!
	  C@ 2DUP = IF
	    2DROP FALSE EXIT	\ input char matches delim
	  THEN
	  EMIT
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
	DUP HEX [CHAR] $ EMIT . SPACE
	DUP DECIMAL [CHAR] # EMIT . SPACE
	DUP OCTAL [CHAR] 0 EMIT . SPACE
	DUP BINARY [CHAR] % EMIT . SPACE
	DUP $21 $7F WITHIN IF [CHAR] ' EMIT EMIT [CHAR] ' EMIT ELSE DROP THEN
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
;				\  S: -- R: j*x +n

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

\ ... limit first DO ... LOOP ...
\
\ (C: -- dest )(R: -- count) || (S: limit first -- ) (R: -- limit first )
\
: DO				\ C: --  R: ip
	POSTPONE 2>R		\ S: --  R: limit first
	R> 0 >R	>R		\ C: --  R: 0 ip
	POSTPONE BEGIN		\ C: dest R: 0 ip
; IMMEDIATE

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

\ ... limit first DO ... IF ... LEAVE THEN ... LOOP ...
\
\ (C: dest -- dest ) (R: n*forw n -- n'*forw n' )
\
: LEAVE				\ C: dest  R: n*forw n ip
	R> R> 1+		\ C: dest ip n'  R: n*forw
	POSTPONE AHEAD		\ C: dest ip n' forw  R: n*forw
	>R >R >R		\ C: dest  R: n'*forw n' ip
; IMMEDIATE

\ ... limit first DO ... test ?LEAVE ... LOOP ...
\
\ (C: dest -- dest ) (R: n*forw n -- n'*forw n' )
\
: ?LEAVE			\ C: dest flag  R: n*forw n ip
	POSTPONE IF		\ C: dest  R: n*forw n ip
	POSTPONE LEAVE		\ C: dest  R: n'*forw n' ip
	POSTPONE THEN		\ C: --  R: n'*forw n' ip
;

\ : X ... limit first DO ... test IF ... UNLOOP EXIT THEN ... LOOP ... ;
\
\ (S: --  ) (R: limit index ip -- ip )
\
: UNLOOP R> 2R> 2DROP >R ;

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
; IMMEDIATE

\ ... limit first DO ... LOOP ...
\
\ (S: -- index ) (R: limit index ip -- limit index ip )
\
: I R> R@ SWAP >R ;

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
; IMMEDIATE

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
0 CONSTANT CASE IMMEDIATE

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
; IMMEDIATE

\ ... ENDOF ...
\
\ (C: forw1 #of -- forw2 #of )
\
: ENDOF
	>R			\ C: forw1 R: #of
	POSTPONE ELSE		\ C: forw2 R: #of
	R>			\ C: forw2 #of R: --
; IMMEDIATE

\ ... CASE ... ENDCASE ...
\
\ (C: i*forw i -- )(S: x -- )
\
: ENDCASE
	POSTPONE DROP		\ S: --
	0 ?DO			\ C: i*forw
	  POSTPONE THEN		\ C: i'*forw
	LOOP
; IMMEDIATE

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

\ Number of transitent string buffers, power of 2.
\ Minimum 2 buffers for S" and S\".
2 CONSTANT _str_buf_max

\ Size of each string buffer, min 80 characters.
/PAD CHARS CONSTANT _str_buf_size

_str_buf_max 1- CONSTANT _str_buf_mask

\ Transient string buffers.
CREATE _str_bufs _str_buf_size _str_buf_max * ALLOT

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
	_str_buf_size *		\ S: offset
	_str_bufs +		\ S: caddr
;

\
\ ( caddr u -- )
\
: _allot_cstring DUP >R reserve R> CMOVE 0 C, ;

\
\ ( caddr -- )
\
\ Print a NUL terminated string.
\
\	CREATE greet 0" Hello world.\n"
\	greet puts
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

\ (S: src dst u -- )
: _copy_counted
	DUP /COUNTED-STRING > IF
	  -24 THROW
	THEN			\ S: src dst u
	DUP >R OVER 		\ S: src dst u dst R: u
	C! CHAR+ R>		\ S: src dst' u R: --
	CMOVE			\ S: --
;

\ ( S: -- caddr )
: _clit 			\ S: -- R: ip
	R> DUP DUP C@		\ S: ip ip u R: --
	CHARS + ALIGNED >R	\ S: ip R: ip'
;

\ ( C: src u -- )
: cliteral
	POSTPONE _clit		\ C: src u
	DUP CHAR+ reserve	\ C: src u dst
	SWAP _copy_counted	\ C: --
	ALIGN
; IMMEDIATE

\ (C: src u -- ) || (S: src u -- caddr )
: _store_counted
	STATE @ IF
	  POSTPONE cliteral
	ELSE
	  _str_buf_next DUP >R	\ S: src u dst R: dst
	  SWAP _copy_counted R>	\ S: dst R: --
	THEN
;

\ ... C" ccc" ...
\
\ (C: ccc<quote>" -- ) || (S: ccc<quote>" -- caddr )
\
: C" [CHAR] " PARSE _store_counted ; IMMEDIATE

\ ... c\" ccc" ...
\
\ (C: ccc<quote>" -- ) || (S: ccc<quote>" -- caddr u )
\
: c\" [CHAR] " parse-escape _store_counted ; IMMEDIATE

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

\ ... _slit ...
\
\ (S: -- caddr u )
\
\ @note
\	The caller's return address is used to find and compute the
\	address and length of the string stored within the word.
\	It is then modified to point to just after the string.
\
: _slit				\ S: -- R: ip
	R@ @ R>			\ S: u ip R: --
	CELL+ SWAP 2DUP		\ S: caddr u caddr u R: --
	CHAR+			\ account for NUL from _allot_cstring
	CHARS + ALIGNED		\ S: caddr u ip' R: --
	>R			\ S: caddr u R: ip'
;

\
\ (S: caddr u -- ; -- caddr u )
\
: SLITERAL
	POSTPONE _slit DUP ,	\ S: caddr u
	_allot_cstring ALIGN	\ S: --
; IMMEDIATE

\ (C: src u -- ) || (S: src u -- caddr u )
: _store_str
	STATE @ IF
	  POSTPONE SLITERAL	\ S: --
	ELSE
	  DUP >R _str_buf_next	\ S: src u dst R: u
	  DUP >R SWAP		\ S: src dst u R: u dst
	  CMOVE			\ S: -- R: u dst
	  R> R>			\ S: dst u R: --
	  \ Add terminating NUL byte for convenience for C.
	  2DUP CHARS + 0	\ S: dst u end NUL
	  SWAP C!		\ S: dst u
	THEN
;

\ ... S" ccc" ...
\
\ (C: ccc<quote>" -- ) || (S: ccc<quote>" -- caddr u )
\
: S" [CHAR] " PARSE _store_str ; IMMEDIATE

\ ... S\" ccc" ...
\
\ (C: ccc<quote>" -- ) || (S: ccc<quote>" -- caddr u )
\
: S\" [CHAR] " parse-escape _store_str ; IMMEDIATE

\ ... ." ccc" ...
\
\ (S: ccc<quote>" -- )
\
: ." POSTPONE S" POSTPONE TYPE ; IMMEDIATE

\ (S: caddr1 caddr2 u -- n )
: _strcmp
	BEGIN ?DUP WHILE	\ S: caddr1 caddr2 u
	  1- >R			\ S: caddr1 caddr2 R: u'
	  DUP C@ >R CHAR+ SWAP	\ S: caddr2' caddr1 R: u' c2
	  DUP C@ >R CHAR+ SWAP  \ S: caddr1' caddr2' R: u' c2 c1
	  R> R> - ?DUP IF	\ S: caddr1' caddr2' diff R: u'
	    \ Different strings.
	    R> DROP 		\ S: caddr1' caddr2' diff R: --
	    >R 2DROP R>	        \ S: diff
	    EXIT
	  THEN
	  R>			\ S: caddr1' caddr2' u' R:
	REPEAT
	\ Equal strings.
	2DROP 0			\ S: 0
;

\ ... strcmp ...
\
\ (S: caddr1 u1 caddr2 u2 -- n )
\
: strcmp
	ROT SWAP 2DUP -		\ S: caddr1 caddr2 u1 u2 udiff
	IF
	  \ Different length strings.
	  - >R 2DROP R> EXIT	\ S: udiff
	THEN
	\ Same length strings.
	DROP _strcmp
;

\ ... COMPARE ...
\
\ (S: caddr1 u1 caddr2 u2 -- n )
\
: COMPARE
	strcmp DUP IF
	  0< IF -1 ELSE 1 THEN
	THEN
;

\ (S: caddr1 u1 caddr2 u2 -- bool )
: starts-with
	ROT SWAP 2DUP <		\ S: caddr1 caddr2 u1 u2
	IF
	  \ String too short to start with prefix.
	  2DROP 2DROP FALSE
	  EXIT			\ S: bool
	THEN
	NIP _strcmp 0=		\ S: bool
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
: _abort
	ROT IF
	  \ Only write the exception message if nobody to CATCH.
	  catch_frame @ 0= IF
	    TYPE CR
	  THEN
	  -2 THROW
        ELSE
	  2DROP
	THEN
; IMMEDIATE

\ : X ... test ABORT" message" ...
\
\ (C: ccc<quote>" -- ) \ (S: i*x x1 --  | i*x ) ( R: j*x --  | j*x )
\
: ABORT" POSTPONE S" POSTPONE _abort ; IMMEDIATE

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
: LOAD >R SAVE-INPUT R> DUP BLK ! BLOCK 1024 EVALUATE RESTORE-INPUT ;

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

\ ... INCL filename ...
\
\ (S: <spaces>filename" -- )
\
\ @note
\	Convenience that marks word boundary before the include.
\
: incl S" MARKER rm_incl" EVALUATE INCLUDE ;

\ ... BUFFER: name
\
\ ( u "<spaces>name" -- ; -- aaddr )
\
: BUFFER: CREATE ALLOT ;

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
\ (C: <spaces>name -- aaddr 0 ) \ (S: -- size )
\
: BEGIN-STRUCTURE
	CREATE HERE 0 0 , 	\ C: aaddr 0
	DOES> @ 		\ S: -- size
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
	CREATE OVER , +		\ C: save offset and advance
	DOES> @ +		\ add offset to addr
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

: [DEFINED] ( <space>name -- bool ) BL PARSE-NAME FIND-NAME 0<> ; IMMEDIATE
: [UNDEFINED] ( <space>name -- bool ) POSTPONE [DEFINED] 0= ; IMMEDIATE

: [ELSE] ( -- )
	1 BEGIN 				\ level
	  BEGIN PARSE-NAME DUP WHILE		\ level adr len
	    2DUP S" [IF]" COMPARE 0= IF		\ level adr len
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
	    THEN
	    ?DUP 0= IF EXIT THEN		\ level'
	  REPEAT 2DROP				\ level
	REFILL 0= UNTIL				\ level
	DROP
; IMMEDIATE

: [IF] ( flag -- )
	0= IF POSTPONE [ELSE] THEN
; IMMEDIATE

: [THEN] ( -- ) ; IMMEDIATE

MARKER rm_user_words

.( Post4 Copyright 2007, 2019 by Anthony Howe.  All rights reserved.
)
