: \ '\n' 0 _parse DROP DROP ; IMMEDIATE

\ Post4 Copyright 2007, 2024 by Anthony Howe.	All rights reserved.

\ ( -- ⊥ )
: BYE 0 bye-status ;

\ (S: char u -- )
: EMIT dsp@ 1 TYPE DROP ; $10 _pp!

\ ( -- )
: .S 'd' EMIT 's' EMIT '\r' EMIT '\n' EMIT _ds DROP _stack_dump ;

\ ( -- )
: .RS 'r' EMIT 's' EMIT '\r' EMIT '\n' EMIT _rs DROP 1 - _stack_dump ;

\ ( char -- caddr u )
: PARSE 0 _parse ;
: parse-escape 1 _parse ; $12 _pp!

\ (S: -- addr )
: HERE _ctx 1 CELLS + @ ; $01 _pp!

\ (S: u -- addr )
: reserve HERE SWAP ALLOT ; $11 _pp!

\ (S: nu1 -- nu2 )
: 1+ 1 + ; $11 _pp!
: 1- 1 - ; $11 _pp!

\ (S: n1 -- n2 )
: NEGATE INVERT 1+ ; $11 _pp!

\ (addr + (pow2-1)) & -pow2
\
\ (S: addr -- aaddr )
: ALIGNED 1 CELLS 1- + 1 CELLS NEGATE AND ; $11 _pp!

\ ( -- xt | 0 )
\ xt is also an nt.
: ' PARSE-NAME FIND-NAME ; $01 _pp!

\ ( -- )
' _nop alias ok					\ Ignore copy/paste lines.
' _nop alias post4				\ Implementation name.
' _nop alias CHARS IMMEDIATE	\ Discard no-op; CHARS still used for portability.

\ (S: caddr -- caddr' )
' 1+ alias CHAR+
' 1- alias CHAR-

\ ( -- )
: ALIGN HERE ALIGNED HERE - CHARS ALLOT ;

\ ( x -- )
: , ALIGN 1 CELLS reserve ! ; $10 _pp!

\ ( char -- )
: C, 1 CHARS reserve C! ; $10 _pp!

\ ( xt -- )
' , alias COMPILE, compile-only

\ (C: xu ... x1 x0 u -- xu ... x1 x0 xu )
' PICK alias CS-PICK compile-only

\ (C: xu xu-1 ... x0 u -- xu-1 ... x0 xu )
' ROLL alias CS-ROLL compile-only

\ (S: x -- x' )
' INVERT alias NOT $11 _pp!

\ Just because I keep mistyping .s when other stacks are .rs and .fs
' .s alias .ds

\ (C: x <spaces>name -- ) (S: -- x )
: CONSTANT CREATE , DOES> @ ; $01 _pp!

\ (S: -- flag )
0 CONSTANT FALSE $01 _pp!
FALSE INVERT CONSTANT TRUE $01 _pp!

\ (S: -- n )
1 CHARS CONSTANT /CHAR $01 _pp!
1 CELLS CONSTANT /CELL $01 _pp!

\ (S: -- ' ' )
'\s' CONSTANT BL $01 _pp!

\ ( -- u )
-3 2 / -2 - 0= CONSTANT floored $01 _pp!
1 CELLS address-unit-bits * CONSTANT cell-bits $01 _pp!
2 CELLS address-unit-bits * 2 + CONSTANT /hold $01 _pp!
1 address-unit-bits LSHIFT 1 - CONSTANT MAX-CHAR $01 _pp!
0 INVERT 1 RSHIFT CONSTANT MAX-N $01 _pp!	\ 0x7fff...ffff
MAX-N INVERT CONSTANT MIN-N $01 _pp!		\ 0x8000...0000
0 INVERT CONSTANT MAX-U	$01 _pp!		\ 0xffff...ffff

_rs CONSTANT return-stack-cells $01 _pp! DROP DROP
_ds CONSTANT stack-cells $01 _pp! DROP DROP

\ (S: nu -- flag )
: 0<> 0= 0= ;

\ (S: x1 x2 -- x2 )
: NIP SWAP DROP ; $21 _pp!

\ (S: x1 x2 -- x1 x2 x1 )
: OVER 1 PICK ; $23 _pp!

\ (S: a b c -- b c a )
: ROT 2 ROLL ; $33 _pp!

\ (C: <spaces>name -- aaddr 0 ) (S: -- size )
: BEGIN-STRUCTURE
	CREATE HERE 0 0 ,			\ C: aaddr 0
	DOES> @						\ S: size
;

\ (C: aaddr size -- )
: END-STRUCTURE SWAP ! ;

\ ... +FIELD name ...
\
\ (C: offset size <spaces>name -- offset' ) \ (S: addr -- addr' )
\
\ Note does not align items.
\
\ Structure name defined last:
\
\	0                       	\ initial total byte count
\	    1 CELLS +FIELD p.x  	\ single cell field named p.x
\	    1 CELLS +FIELD p.y  	\ single cell field named p.y
\	CONSTANT point          	\ save structure size
\
\ Structure name defined first:
\
\	BEGIN-STRUCTURE poin    	\ create the named structure
\	    1 CELLS +FIELD p.x  	\ A single cell filed named p.x
\	    1 CELLS +FIELD p.y  	\ A single cell field named p.y
\	END-STRUCTURE
\
: +FIELD
	CREATE OVER , +				\ C: aaddr offset size -- aaddr offset'
	DOES> @ +					\ S: addr -- addr'
;

\ (C: offset <spaces>name -- offset' ) \ (S: addr -- addr' )
: CFIELD: 1 CHARS +FIELD ;

\ (C: offset <spaces>name -- offset' ) \ (S: addr -- addr' )
: FIELD: ALIGNED 1 CELLS +FIELD ;

BEGIN-STRUCTURE p4_word
	FIELD: w.prev				\ pointer previous word
	FIELD: w.length
	FIELD: w.name
	FIELD: w.bits
	FIELD: w.poppush
	FIELD: w.code				\ pointer
	FIELD: w.ndata				\ data length
	FIELD: w.data				\ pointer to data cells
END-STRUCTURE

%0001 CONSTANT w.bit_imm
%0010 CONSTANT w.bit_created
%0100 CONSTANT w.bit_hidden
%1000 CONSTANT w.bit_compile

\ (S: bit xt -- )
: _word_set w.bits DUP @ ROT OR SWAP ! ; $20 _pp!
: _word_clear w.bits DUP @ ROT INVERT AND SWAP ! ; $20 _pp!
: _word_bit? w.bits @ AND 0<> ; $20 _pp!

\ (S: xt -- )
: hide w.bit_hidden SWAP _word_set ; $10 _pp!
: urgent w.bit_imm SWAP _word_set ; $10 _pp!

\ (S: xt -- bool )
: immediate? w.bit_imm SWAP _word_bit? ; $11 _pp!
: compile-only? w.bit_compile SWAP _word_bit? ; $11 _pp!

 0 CONSTANT w.pp_ds_push
 4 CONSTANT w.pp_ds_pop
 8 CONSTANT w.pp_rs_push
12 CONSTANT w.pp_rs_pop
16 CONSTANT w.pp_fs_push
20 CONSTANT w.pp_fs_pop
24 CONSTANT w.pp_lit

1024 CONSTANT _blk_size

BEGIN-STRUCTURE p4_block
	FIELD: blk.state			\ 0 free, 1 clean, 2 dirty, 3 lock
	FIELD: blk.number			\ 0 < number
	_blk_size +FIELD blk.buffer
END-STRUCTURE

BEGIN-STRUCTURE p4_stack
	FIELD: stk.size
	FIELD: stk.top				\ pointer
	FIELD: stk.base				\ pointer
END-STRUCTURE

BEGIN-STRUCTURE p4_input
	FIELD: in.fp				\ pointer
	FIELD: in.blk
	FIELD: in.length
	FIELD: in.offset
	FIELD: in.buffer			\ pointer
	/pad +FIELD in.data
END-STRUCTURE

BEGIN-STRUCTURE p4_options
	FIELD: opt.argc
	FIELD: opt.argv
	FIELD: opt.trace
	FIELD: opt.ds_size;
	FIELD: opt.rs_size;
	FIELD: opt.fs_size;
	FIELD: opt.mem_size;
	FIELD: opt.hist_size;
	FIELD: opt.core_file;
	FIELD: opt.block_file;
END-STRUCTURE

\ Example
\
\	_ctx		 		\ Post4 machine context pointer
\	ctx.active @ @		\ pointer to most recent word
\	w.name @			\ pointer to word name
\	puts				\ write name
\
BEGIN-STRUCTURE p4_ctx
	FIELD: ctx.end				\ see UNUSED
	FIELD: ctx.here				\ see HERE
	FIELD: ctx.state			\ see STATE
	FIELD: ctx.frame			\ see CATCH and THROW
	FIELD: ctx.trace			\ see _trace
	FIELD: ctx.level			\ see p4
	FIELD: ctx.radix			\ see BASE
	p4_stack +FIELD ctx.ds		\ see _ds
	p4_stack +FIELD ctx.rs		\ see _rs
\ [DEFINED] _fs [IF]
	p4_stack +FIELD ctx.fs		\ see _fs
	FIELD: ctx.precision		\ see PRECISION and SET-PRECISION
\ [THEN]
	FIELD: ctx.unkey			\ KEY and KEY?
	FIELD: ctx.input			\ pointer
	FIELD: ctx.block			\ pointer
	FIELD: ctx.block_fd
	FIELD: ctx.active
	FIELD: ctx.locals			\ must immediate before ctx.lists
	WORDLISTS CELLS +FIELD ctx.lists
	FIELD: ctx.norder
	WORDLISTS CELLS +FIELD ctx.order
	p4_options +FIELD ctx.options
\ [DEFINED] jcall [IF]
	FIELD: ctx.jenv
\ [THEN]
\	0 +FIELD ctx.longjmp		\ size varies by host OS
END-STRUCTURE

\ (S: -- addr )
: _end _ctx ctx.end @ ; $01 _pp!
: STATE _ctx ctx.state ; $01 _pp!
: catch_frame _ctx ctx.frame ; $01 _pp!
: trace _ctx ctx.trace ; $01 _pp!
: BASE _ctx ctx.radix ; $01 _pp!

\ (S: -- argv argc )
: args _ctx ctx.options @ DUP opt.argv @ SWAP opt.argc @ ; $02 _pp!

\ ( -- u )
: UNUSED _end HERE - ; $01 _pp!

\ ( u "<spaces>name" -- addr )
: BUFFER: CREATE ALLOT ; $11 _pp!

\ ( -- )
/PAD CHARS BUFFER: PAD

\ (C: <spaces>name -- ) \ (S: -- aaddr )
: VARIABLE CREATE 0 , ; $01 _pp!

\ (C: <spaces>name -- ) \ (S: -- aaddr )
: 2VARIABLE CREATE 0 , 0 , ; $01 _pp!

\ (S: -- )
: [ FALSE STATE ! ; IMMEDIATE \ allow interpret
: ] TRUE STATE ! ; \ allow interpret

\ (S: aaddr1 -- aaddr2 )
: CELL+ /CELL + ; $11 _pp!
: CELL- /CELL - ; $11 _pp!

\ ( -- x )(R: x -- x)
: R@ R> R> DUP >R SWAP >R ; $1101 _pp!

\ (S" -- u )
: DEPTH _ds DROP NIP ;

\ (S: xn ... x1 n -- )
: dropn CELLS dsp@ SWAP - CELL- dsp! ;

\ ( i*x -- )
: dropall DEPTH dropn ;

\ (S: -- )
: DECIMAL #10 BASE ! ;
: HEX #16 BASE ! ;
: octal #8 BASE ! ;
: binary #2 BASE ! ;

\ (S: x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
: 2ROT 5 ROLL 5 ROLL ; $66 _pp!

\ (S: u -- xu )(R: xu ...x2 x1 -- xu ...x2 x1 )
: rpick _rs DROP 2 - ROT - CELLS + @ ; $11 _pp!

\ @note
\	This assumes that 0< returns a proper flag (all bits 1) for true
\	as opposed simply any non-zero value for true.
\
\ (S: n -- d )
: S>D DUP 0< ; $12 _pp!	\ Sign extend into high word.

\ @note
\	More useful to as way of explaining in code what is being done
\	rather than rely on a zero value being pushed to the stack.
\	Consider ` 123456789 0 67 UM/MOD ` vs ` 123456789 U>D 67 UM/MOD `
\
\ (S: u -- d )
: u>d 0 ; $12 _pp!

\ @note
\	More useful to as way of explaining in code what is being done
\	rather than rely on a zero value being pushed to the stack.
\	Consider ` 123456789 0 67 UM/MOD ` vs ` 123456789 U>D 67 UM/MOD `
\
\ (S: d -- u )
: d>u DROP ; $21 _pp!

\ (S: x1 x2 -- x2 x1 x2 )
: TUCK SWAP OVER ; $23 _pp!

\ (S: a b c -- c b a )
: SPIN SWAP ROT ; $33 _pp!

\ (S: a b c -- b a c )
: RISE	>R SWAP R> ; $33 _pp!

\ (S: a b -- a a b )
: STOW OVER SWAP ; $23 _pp!

\ (S: a b c -- c a b )
\ or : -ROT ROT ROT ;
\ or : -ROT SWAP >R SWAP R> ;
: -ROT SPIN SWAP ; $33 _pp!

\ (S: n addr --	)
: +! DUP @ ROT + SWAP ! ; $20 _pp!

\ (S: caddr u n -- caddr' u' )
: /STRING >R R@ - SWAP R> CHARS + SWAP ;

\ (S: lo hi aaddr -- )
: 2! TUCK ! CELL+ ! ;

\ Fetch from aaddr the two cells, hi lo, and place on stack lo hi.
\
\ (S: aaddr -- lo hi )
: 2@ DUP CELL+ @ SWAP @ ;

\ (S: x1 -- x2 )
: 2* 1 LSHIFT ;

\ ... 2/ ...
\
\ (S: x1 -- x2 )
\
\	int s = -((unsigned) x >> LONG_BIT-1);
\	int sar = (s^x) >> n ^ s;
\
: 2/
	DUP cell-bits 1 -			\ S: x x bits
	RSHIFT NEGATE				\ S: x s
	DUP >R XOR 1 RSHIFT R> XOR	\ S: x'
;

\ x y	2CONSTANT name
\
\ (C: x y <spaces>name -- ) (S: -- x y )
\
: 2CONSTANT CREATE , , DOES> 2@ ;

MAX-U MAX-U 2CONSTANT MAX-UD
MAX-U MAX-N 2CONSTANT MAX-D
0 MIN-N 2CONSTANT MIN-D

\ (S: x1 x2 -- )
: 2DROP DROP DROP ; $20 _pp!

\ (S: x1 x2 -- x1 x2 x1 x2 )
: 2DUP OVER OVER ; $02 _pp!

\ (S: x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
: 2OVER 3 PICK 3 PICK ; $02 _pp!

\ (S: x1 x2 x3 x4 -- x3 x4 x1 x2 )
: 2SWAP 3 ROLL 3 ROLL ;

\ (S: -- ; R: x -- )
: rdrop R> R> DROP >R ; compile-only $1000 _pp!

\ (S: -- ; R: x y -- )
: 2rdrop R> rdrop rdrop >R ; compile-only $2000 _pp!

\ (S: x1 x2 -- )(R: -- x1 x2 )
: 2>R
	R> ROT 					\ S: x2 ip x1	R: --
	>R SWAP					\ S: ip x2	R: x1
	>R >R					\ S: --	R: x1 x2 ip
; compile-only

\ ... 2R> ...
\
\ (S: -- x1 x2 )(R: x1 x2 -- )
\
: 2R>
	R> R> R>				\ S: ip x2 x1	R: --
	ROT						\ S: x2 x1 ip	R: --
	>R SWAP					\ S: x1 x2	R: ip
; compile-only

\ ... 2R@ ...
\
\ (S: -- x1 x2 )(R: x1 x2 -- x1 x2 )
\
: 2R@
	R> 2R>					\ S: ip x1 x2	R: --
	2DUP 2>R				\ S: ip x1 x2	R: x1 x2
	ROT >R					\ S: x1 x2	R: x1 x2 ip
; compile-only

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
	2>R R>						\ S: xl xh yh	R: yl
	-							\ S: x1 dh	R: yl
	SWAP R>						\ S: dh x1 yl	R: --
	-							\ S: dh dl
	D0=							\ S: bool
;

\ (S: <spaces>name.new <spaces>name.old -- )
: SYNONYM >IN @ PARSE-NAME 2DROP ' >IN @ SPIN >IN ! alias >IN ! ;

\ (S: -- )
: CR '\r' EMIT '\n' EMIT ;

\ (S: -- )
: SPACE BL EMIT ;

\ (S: caddr1 -- caddr2 u )
: COUNT DUP CHAR+ SWAP C@ ;

\ (S: <spaces>name -- char )
: CHAR PARSE-NAME DROP C@ ;

\ Compile LIT xt into the current word, which pushes xt when run.
\ (C: <spaces>name -- ) (S: -- xt )
: ['] LIT LIT COMPILE, ' COMPILE, ; IMMEDIATE compile-only

\ (C: x -- ; S:  -- x )
: LIT, ['] LIT COMPILE, , ;

\ ... : name ... [ x ] LITERAL ... ;
\
\	(C: x -- ) (S: -- x )
\
: LITERAL LIT, ; IMMEDIATE compile-only

\ ... test IF ... THEN ...
\ ... test IF ... ELSE ... THEN ...
\
\	(C: -- forw ) (S: flag -- )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: IF ['] _branchz COMPILE, >HERE 0 , ; IMMEDIATE compile-only

\ ... AHEAD ... THEN ...
\ ... test IF ... THEN ...
\ ... test IF ... ELSE ... THEN ...
\
\	(C: forw -- )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: THEN							\	C: forw_off
	>HERE SWAP -				\	C: dist
	DUP							\	C: dist dist
	HERE SWAP -					\	C: dist forw_addr
	!							\	C: --
; IMMEDIATE compile-only

\ ... ?DUP ...
\
\ (S: x -- 0 | x x )
\
: ?DUP DUP IF DUP THEN ;

\ (S: xl xh yl yh -- bool )
: DU<
	ROT 2DUP = IF				\ S: xl yl yh xh
		2DROP U< EXIT			\ S: bool
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
\ https://github.com/ForthHub/discussion/discussions/173#discussioncomment-10668897
\		['] execute-compiling EXIT
\ but the test suite fails.
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
\ Dividend   Divisor   Remainder   Quotient
\    10         7          3           1
\   -10         7          4          -2
\    10        -7         -4          -2
\	-10        -7         -3           1
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
		rdrop
		EXIT
	THEN
	\ No, same sign, SM/REM result same as FM/MOD.
	SM/REM
;

\ ... DEFER name ...
\
\ (S: <spaces>name -- )
\
: DEFER CREATE ['] _nop , DOES> @ EXECUTE ;

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

DEFER fsp@
DEFER fsp!

' TRUE ' fsp@ DEFER!
' DROP ' fsp! DEFER!

\ ... CATCH ...
\
\ ( i*x xt -- j*x 0 | i*x n )
\
: CATCH							\ S: xt		R: ip
	dsp@ >R						\ S: xt		R: ip ds
	fsp@ >R						\ S: xt		R: ip ds fs
	catch_frame @ >R			\ S: xt		R: ip ds fs cf
	rsp@ catch_frame !			\ S: xt		R: ip ds fs cf
	EXECUTE						\ S: --		R: ip ds fs cf
	R> catch_frame !			\ S: --		R: ip ds fs
	R> R> 2DROP 				\ S: --		R: ip
	0							\ S: 0		R: ip
; $11 _pp!

\ ... THROW ...
\
\ ( k*x n -- k*x | i*x n )
\
: THROW							\ S: n		R:
	\ 0 THROW is a no-op.
	?DUP IF						\ S: n		R:
		\ When no catch frame, throw to C.
		catch_frame @ 0= IF		\ S: n		R:
			_longjmp			\ S: --		R: --
		THEN
		\ Restore return stack of CATCH at EXECUTE.
		catch_frame @ rsp!		\ S: n		R: ip ds fs cf
		R> catch_frame !		\ S: n		R: ip ds fs
		R> fsp!					\ S: n		R: ip ds fs
		R> SWAP >R				\ S: ds		R: ip n
		\ Restore data stack at start of CATCH
		dsp!					\ S: xt		R: ip n
		DROP R>					\ S: n		R: ip
	THEN
; $10 _pp!

\ ( i*x -- ⊥ )(F: k*x -- ⊥ )( R: j*x -- ⊥ )
: ABORT -1 THROW ;

\ ( xt -- )
: execute-compiling
	STATE @ IF EXECUTE EXIT THEN
	TRUE STATE ! EXECUTE FALSE STATE !
;

\ (S: <spaces>name -- )
\
\ @note
\	: bar state @ 0<> . ; immediate	: foo postpone bar ;	foo
\	Standard system should print -1.
\
\ @see
\	https://github.com/ForthHub/discussion/discussions/105
\
: POSTPONE
	PARSE-NAME FIND-NAME		\ S: xt | 0
	DUP 0= -13 AND THROW		\ S: xt
	DUP LIT, immediate?			\ S: bool
	IF							\ S: --
		['] execute-compiling
		COMPILE, EXIT
	THEN
	['] COMPILE, COMPILE,
; IMMEDIATE compile-only

\ ( -- xt )
\ Redefine now that we can THROW for an undefined word.
: ' PARSE-NAME FIND-NAME DUP 0= -13 AND THROW ;

\ (C: <spaces>name -- ) (S: -- xt )
\ Redefine now that ' can THROW on undefined word.
: ['] LIT LIT COMPILE, ' COMPILE, ; IMMEDIATE compile-only

\ ...	[CHAR]	...
\
\	(C: <spaces>name -- ) \ (S: -- char )
\
: [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE compile-only

\ ... BEGIN ... AGAIN
\ ... BEGIN ... test UNTIL ...
\ ... BEGIN ... test WHILE ... REPEAT ...
\
\	(C: -- dest )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: BEGIN >HERE ; IMMEDIATE compile-only

\ ... BEGIN ... AGAIN
\
\	(C: dest -- )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: AGAIN POSTPONE _branch >HERE - , ; IMMEDIATE compile-only

\ ... BEGIN ... test UNTIL ...
\
\	(C: dest -- ) \ (S: flag -- )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: UNTIL POSTPONE _branchz >HERE - , ; IMMEDIATE compile-only

\	(C: dest -- ) (S: bool -- )
: whilst POSTPONE _branchnz >HERE - , ; IMMEDIATE compile-only

\ ... AHEAD ... THEN ...
\
\	(C: -- forw )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: AHEAD POSTPONE _branch >HERE 0 , ; IMMEDIATE compile-only

\ ... test IF ... ELSE ... THEN ...
\
\	(C: forw1 -- forw2 )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: ELSE							\ C: forw1
	POSTPONE AHEAD				\ C: forw1 forw2
	1 CS-ROLL					\ C: forw2 forw1
	POSTPONE THEN				\ C: forw2
; IMMEDIATE compile-only

\ ... BEGIN ... test WHILE ... REPEAT ...
\
\	(C: dest -- forw dest ) \ (S: flag -- )
\ ... BEGIN ... test WHILE ... test WHILE ... REPEAT THEN ...
\ ... BEGIN ... test WHILE ... test WHILE ... AGAIN THEN THEN ...
\
\	Multiple WHILE possible to provide short-circuit testing, but each
\	additional WHILE needs a THEN in order to resolve each forward
\	reference remaining on the stack.
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: WHILE							\ C: dest
	POSTPONE IF					\ C: dest forw
	1 CS-ROLL					\ C: forw dest
; IMMEDIATE compile-only

\ ... BEGIN ... test WHILE ... REPEAT ...
\
\	(C: forw dest -- )
\
\ @see
\	A.3.2.3.2 Control-flow stack
\
: REPEAT						\ C: forw dest
	POSTPONE AGAIN				\ C: forw
	POSTPONE THEN				\ C: --
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

\ (S: n1 n2 -- n3 )
: MAX 2DUP < IF SWAP THEN DROP ;
: MIN 2DUP > IF SWAP THEN DROP ;

\ (S: u1 u2 -- u3 )
: umax 2DUP U< IF SWAP THEN DROP ;
: umin 2DUP U> IF SWAP THEN DROP ;

\ (S: d1 d2 -- d3 )
: DMAX 2OVER 2OVER D< IF 2SWAP THEN 2DROP ;
: DMIN 2OVER 2OVER D< INVERT IF 2SWAP THEN 2DROP ;

\ (S: dl dh -- dl' dh' )
\ *c1 = ~*c1 + ((*c0 = -*c0) == 0);
: DNEGATE 						\ S: dl dh
	INVERT SWAP					\ S: dh' dl
	NEGATE SWAP					\ S: dl' dh"
	OVER 0= 1 AND				\ S: dl' dh" c
	+							\ S: dl' dh'
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
	DUP 1 AND					\ S: xl xh lsb
	cell-bits 1 -				\ S: xl xh lsb bits
	LSHIFT SWAP 2/ >R			\ S: xl msb		R: xh'
	SWAP 1 RSHIFT				\ S: msb x1'	R: xh'
	OR R>						\ S: xl' xh'
;

\ (S: xl xh n -- yl yh )
: M+ S>D D+ ;

\ ( ul uh u -- tl tm th )
: ut*
	ROT OVER					\ S: uh u ul u
	UM* 2SWAP					\ S: tl tm uh u
	UM* SWAP					\ S: tl tm th tt
	0 D+						\ S: tl tm' th'
;

\ ( tl tm th u -- ud )
: ut/
	DUP >R						\ S: tl tm th u		R: u
	UM/MOD						\ S: tl r0 q0		R: u
	-ROT R>						\ S: q0 tl r0 u
	UM/MOD						\ S: q0 r1 q1
	NIP SWAP					\ S: q1 q0
;

\ ( d1 n1 n2 -- d2 )
: M*/
	>R 2DUP XOR >R				\ S: dl dh n1		R: n2 sign
	ABS >R						\ S: dl dh			R: n2 sign u1
	DABS R>						\ S: ul uh u1		R: n2 sign
	ut*							\ S: tl tm th 		R: n2 sign
	2R> >R						\ S: tl tm th n2	R: sign
	ut/							\ S: ud				R: sign
	R> 0< IF DNEGATE THEN		\ S: d2
;

\ ... : name ... [ x1 x2 ] 2LITERAL ... ;
\
\	(C: x1 x2 -- ) (S: -- x1 x2 )
\
: 2LITERAL SWAP POSTPONE LITERAL POSTPONE LITERAL ; IMMEDIATE compile-only

\ (S: addr -- addr' x )
: @+ DUP CELL+ SWAP @ ;
: @- DUP cell- SWAP @ ;

\ (S: caddr -- caddr' x )
: C@+ DUP CHAR+ SWAP C@ ;

\ (S: x addr -- addr' )
: !+ DUP CELL+ -rot ! ;

\ (S: x*i i -- )
: n, DUP , BEGIN SWAP , 1- DUP whilst DROP ; $10 _pp!

\ (S: aaddr -- x*i )
: n@
	DUP >R DUP @ CELLS +
	BEGIN DUP R@ > WHILE @- SWAP REPEAT
	R> 2DROP
; $10 _pp!

\ (S: x*i aaddr -- )
: n!
	@+ CELLS OVER + >R
	BEGIN DUP R@ < WHILE !+ REPEAT
	R> 2DROP
; $10 _pp!

\ (S: i*x i <spaces>name -- )
: _value CREATE n, DOES> n@ ; $20 _pp!

\	value VALUE name
\
\	(C: x <spaces>name -- ) (S: -- x )
\
\ @note
\	Similar definition to CONSTANT.	Essentially VALUE when defined does:
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
: VALUE 1 _value ; $10 _pp!

\	lo hi 2VALUE name
\
\	(C: lo hi <spaces>name -- ) (S: -- lo hi )
\
\ @see
\	TO
\
: 2VALUE 2 _value ; $20 _pp!

\ (S: i*x <spaces>name -- )
: TO
	' >BODY
	STATE @ IF
		POSTPONE LITERAL
		POSTPONE n!
	ELSE
		n!
	THEN
; IMMEDIATE $10 _pp!

\ (S: -- )
: >in+ 1 >IN +! ;

\ ( -- caddr u )
: source-remaining SOURCE >IN @ /STRING ; $02 _pp!

\ ( delim escape -- delim escape bool )
\
\ Scan the input buffer character at a time until either the input
\ is exhusted, returning true; or an input character matches delim,
\ returning false.
\
: parse-more
	>R
	BEGIN								\ S: delim
		source-remaining 0= IF			\ S: delim caddr
			DROP R> TRUE EXIT			\ empty input buffer
		THEN
		1 >IN +!						\ S: delim caddr
		DUP C@ R@ = IF					\ escape next char?
			DROP 1 >IN +! 				\ S: delim ch
		ELSE
			C@ OVER = IF				\ S: delim
				R> FALSE EXIT			\ input char matches delim
			THEN
		THEN
	AGAIN
;

\ (S: delim escape xt -- )
: parse-multiline
	>R BEGIN
		R@ EXECUTE						\ find delim in input buffer
	WHILE								\ found delim yet?
		REFILL 							\ read more input
	whilst THEN							\ EOF yet?
	rdrop 2DROP
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
: ( [CHAR] ) -1 ['] parse-more parse-multiline ; IMMEDIATE

: \( [CHAR] ) [CHAR] \ ['] parse-more parse-multiline ; IMMEDIATE

\ ... SPACES ...
\
\ (S: n -- )
\
: SPACES
	BEGIN DUP 0> WHILE			\	S: n
		SPACE 1-				\	S: n'
	REPEAT DROP					\	S: --
;

\ ... CMOVE ...
\
\ (S: src tar u -- )
: CMOVE
	BEGIN ?DUP WHILE			\ S: src tar u
		1- >R					\ S: src tar			R: u'
		2DUP SWAP				\ S: src tar tar src 	R: u'
		C@ SWAP C!				\ S: src tar 			R: u'
		CHAR+ SWAP				\ S: tar' src 			R: u'
		CHAR+ SWAP				\ S: src' tar' 			R: u'
		R>						\ S: src' tar' u'		R --
	REPEAT 2DROP
;

\ ... CMOVE> ...
\
\ (S: src tar u -- )
: CMOVE>
	BEGIN ?DUP WHILE			\ S: src tar u
		1- >R					\ S: src tar 			R: u'
		2DUP SWAP				\ S: src tar tar src 	R: u'
		C@ SWAP C!				\ S: src tar			R: u'
		CHAR- SWAP				\ S: tar' src 			R: u'
		CHAR- SWAP				\ S: src' tar' 			R: u'
		R>						\ S: src' tar' u' 		R --
	REPEAT 2DROP
;

\ (S: addr u -- addr' addr )
: bounds OVER + SWAP ;

\ (S: caddr u char -- )
: FILL
	>R CHARS bounds				\ S: caddr" caddr		R: ch
	BEGIN 2DUP > WHILE			\ S: caddr" caddr		R: ch
		R@ OVER C! CHAR+		\ S: caddr" caddr'		R: ch
	REPEAT
	rdrop 2drop
; $30 _pp!

\ (S: caddr u -- )
: BLANK BL FILL ; $20 _pp!

\ (S: addr u -- )
: ERASE 0 FILL ; $20 _pp!

\ (S: ch -- ch' )
: tolower DUP 'A' [ 'Z' 1+ ] LITERAL WITHIN IF $20 OR THEN ; $11 _pp!
: toupper DUP 'a' [ 'z' 1+ ] LITERAL WITHIN IF [ $20 INVERT ] LITERAL AND THEN ; $11 _pp!

\ (S: char -- bool )
: isblank DUP '\s' = SWAP '\t' = OR ; $11 _pp!
: iscntrl DUP 0 '\s' WITHIN SWAP $7F = OR ; $11 _pp!
: isdigit '0' [ '9' 1+ ] LITERAL WITHIN ; $11 _pp!
: isprint $20 $7F WITHIN ; $11 _pp!
: isgraph $21 $7F WITHIN ; $11 _pp!
: isspace
	FALSE
	OVER '\f' = OR
	OVER '\n' = OR
	OVER '\r' = OR
	OVER '\s' = OR
	OVER '\t' = OR
	OVER '\v' = OR
	SWAP DROP
; $11 _pp!
: isxdigit DUP isdigit toupper 'A' [ 'F' 1+ ] LITERAL WITHIN OR ; $11 _pp!
: isalpha toupper 'A' [ 'Z' 1+ ] LITERAL WITHIN ; $11 _pp!
: isalnum DUP isalpha isdigit OR ; $11 _pp!

\ ... strrev ...
\
\ ( caddr u -- )
\
: strrev
	CHARS OVER +				\ S: x y
	BEGIN
		CHAR- 2DUP <			\ S: x y' bool
	WHILE
		2DUP 2DUP				\ S: x y' x y' x y'
		C@ >R C@				\ S: x y' x y' cx R: cy
		SWAP C! R>				\ S: x y' x cy
		SWAP C!					\ S: x y'
		SWAP CHAR+ SWAP			\ S: x' y'
	REPEAT
	2DROP						\ S: --
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
	DUP isdigit IF						\ S: char
		'0' - EXIT						\ S: value
	ELSE DUP isalpha IF					\ S: char
		tolower 'a' - #10 + EXIT		\ S: value
	THEN THEN							\ S: char
	\ Not found.
	DROP #127							\ S: 127
;

\ ... >NUMBER ...
\
\ ( S: ud1 caddr len -- ud2 caddr' len' )
\
: >NUMBER
	BEGIN
		DUP 0>					\ S: udl udh caddr len
	WHILE
		OVER C@					\ S: udl udh caddr len char
		_digit_value			\ S: udl udh caddr len digit
		DUP BASE @ >= IF		\ S: udl udh caddr len digit
			DROP EXIT			\ S: ud' caddr' len'
		THEN					\ S: udl udh caddr len digit
		ROT CHAR+ ROT 1-		\ S: ud1 udh digit caddr' len'
		2>R						\ S: udl udh digit			R: caddr' len'
		SWAP BASE @				\ S: udl digit udh base		R: caddr' len'
		UM* 					\ S: ud1 digit vl vh		R: caddr' len'
		DROP ROT BASE @			\ S: digit vl udl base		R: caddr' len'
		UM*						\ S: digit vl wl wh			R: caddr' len'
		D+						\ s: udl' udh'				R: caddr' len'
		2R>						\ s: udl' udh'
	REPEAT
;

/HOLD CHARS BUFFER: _pic

VARIABLE _>pic

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
	>R 0 R@						\ S: ud0 ud1 0 n		R: n
	UM/MOD R>					\ S: ud0 r q n
	SWAP >R						\ S: ud0 r n			R: q
	UM/MOD R>					\ S: r' q' q
;

\ ( ud1 -- ud2 )
: #
	BASE @ mu/mod ROT			\ S: q0 q1 rem
	DUP 0 #10 WITHIN			\ S: q0 q1 rem bool
	IF							\ S: q0 q1 rem
		'0' +					\ S: q0 q1 digit
	ELSE
		#10 - 'A' +				\ S: q0 q1 digit
	THEN
	HOLD						\ S: q0 q1
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
		SOURCE >IN @			\ S: char caddr u off
		<= SWAP >IN @ + C@		\ S: char len_ge_off input
		2 PICK <>				\ S: char len_ge_off char_neq
		OR DUP 0= IF			\ S: char bool
			1 >IN +!			\ S: char bool
		THEN
	UNTIL						\ S: char
	DROP						\ S: --
;

\ ... char WORD ...
\
\ (S: char "<chars>ccc<char>" -- caddr )
\
\ See 3.3.3.6 Other transient regions paragraph 2.
\
: WORD							\ S: char
	DUP skip_chars				\ S: char
	PARSE 						\ S: caddr u
	DUP _pic C!					\ S: caddr u
	_pic CHAR+ SWAP				\ S: caddr pic' u
	MOVE _pic					\ S: caddr
;

\ ( caddr -- caddr 0 | xt 1 | xt -1 )
: FIND
	DUP COUNT FIND-NAME			\ S: caddr 0 | caddr xt
	DUP IF
		NIP DUP immediate? IF	\ S: xt
			1					\ S: xt 1
		ELSE
			-1					\ S: xt -1
		THEN
	THEN
;

\ ...	U.	...
\
\ (S: u -- )
\
: U. 0 <# #S #> TYPE SPACE ;

\ ... U.R	...
\
\ (S: u w -- )
\
: U.R >R 0 <# #S #> R> OVER - SPACES TYPE ;

\ ... .R	...
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

\ ( delim escape -- delim escape bool )
\
\ Scan the input buffer character at a time until either the input
\ is exhusted, returning true; or an input character matches delim,
\ returning false.
\
: emit-more
	>R
	BEGIN								\ S: delim
		source-remaining 0= IF			\ S: delim caddr
			DROP R> TRUE EXIT			\ empty input buffer
		THEN
		1 >IN +!						\ S: delim caddr
		DUP C@ R@ = IF					\ escape next char?
			1 >IN +! CHAR+ C@			\ S: delim ch
		ELSE
			C@ 2DUP = IF				\ S: delim ch
				DROP R> FALSE EXIT		\ input char matches delim
			THEN
		THEN
		EMIT							\ S: delim
	AGAIN
;

\
\ .( ccc)
\
\ (S: ccc<paren> -- )
: .( [CHAR] ) -1 ['] emit-more parse-multiline ; IMMEDIATE

: .\( [CHAR] ) [CHAR] \ ['] emit-more parse-multiline ; IMMEDIATE

\ ... ? ...
\
\ (S: aaddr -- )
\
: ?
	BASE @ >R
	DUP HEX [CHAR] $ EMIT . SPACE @
	DUP HEX [CHAR] $ EMIT . SPACE
	DUP DECIMAL [CHAR] # EMIT . SPACE
	CR R> BASE !
;

\ (S: caddr -- )
: C?
	BASE @ >R
	DUP HEX [CHAR] $ EMIT . SPACE C@
	DUP HEX [CHAR] $ EMIT . SPACE
	DUP DECIMAL [CHAR] # EMIT . SPACE
	DUP OCTAL [CHAR] 0 EMIT . SPACE
	DUP BINARY [CHAR] % EMIT . SPACE
	DUP isprint IF [CHAR] ' EMIT EMIT [CHAR] ' EMIT ELSE DROP THEN
	CR R> BASE !
;

\ ... N>R ...
\
\ (S: i*x n -- ) (R: -- i*x n )
\
: N>R							\	S: i*x n 			R: ip
	R> SWAP DUP					\	S: i*x ip n n 		R:
	BEGIN DUP 0> WHILE			\	S: j*x ip n j
		3 ROLL					\	S: j*x ip n j x
		>R 1-					\	S: j*x ip n j' 		R: j*x
	REPEAT
	DROP >R >R					\	S: -- 				R: j*x +n ip (j*x reverse of start i*x)
;

\ ... NR> ...
\
\ (S: –– i*x +n ) (R: i*x +n –– )
\
\	The original stack order of i*x prior to the matching N>R is restored.
\
: NR>							\	S: -- 				R: i*x n ip (i*x reverse of original)
	R> R> DUP					\	S: ip n i			R: i*x
	BEGIN DUP 0> WHILE			\	S: ip j*x n i 		R: i*x
		R> 						\	S: ip j*x n i x' 	R: i*x
		ROT						\	S: ip j*x i x' n 	R: i*x
		ROT						\	S: ip j*x x' n i 	R: i*x
		1-						\	S: ip j*x n i' 		R: i'*x
	REPEAT
	DROP DUP 1+ ROLL >R	\	S: ip j*x n R: ip
; \ allow interpret

\ ... x CASE ... ENDCASE
\
\ (C: -- #of ) (S: x -- x )
\
\	CASE
\		test1 OF ... ENDOF
\		...
\		testN OF ... ENDOF
\		default action
\	ENDCASE
\
0 CONSTANT CASE IMMEDIATE compile-only

\ ... test OF ... ENDOF ...
\
\ (C: i*forw #of -- j*forw #of' )
\
: OF
	1+ >R						\ C: -- R: #of'
	POSTPONE OVER				\ S: x1 x2 x1
	POSTPONE =					\ S: x1 f
	POSTPONE IF					\ S: x1
	POSTPONE DROP				\ S: --
	R>							\ C: #of'
; IMMEDIATE compile-only

\ ... ENDOF ...
\
\ (C: forw1 #of -- forw2 #of )
\
: ENDOF
	>R							\ C: forw1 		R: #of
	POSTPONE ELSE				\ C: forw2 		R: #of
	R>							\ C: forw2 		#of R: --
; IMMEDIATE compile-only

\ ... CASE ... ENDCASE ...
\
\ (C: i*forw i -- )(S: x -- )
\
: ENDCASE
	POSTPONE DROP				\ S: --
	BEGIN ?DUP WHILE			\ C: n*forw n
		1-						\ C: n*forw n'
		SWAP					\ C: n'*forw n' forw
		POSTPONE THEN			\ C: n'*forw n'
	REPEAT
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
		[CHAR] s OF	BL ENDOF	\ S: ascii char
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

\ (S: src dst u -- )
\ Copy and NUL terminate string.
: strncpy 2DUP CHARS + >R MOVE 0 R> C! ;

\ Maximum for octet addressable units.
MAX-CHAR CONSTANT /COUNTED-STRING

\ ( S: -- caddr )
: clit 							\ S: --				R: ip
	\ The IP points to counted string, get its length.
	R> DUP DUP C@				\ S: ip ip u 		R: --
	\ Update IP to point immediate after the counted string.
	1+ CHARS + ALIGNED >R		\ S: caddr 			zR: ip'
; $01000001 _pp!

: _cstring_append
	POSTPONE clit				\ S: src u
	\ Reserve space for the length and string.
	DUP CHAR+ reserve			\ S: src u dst
	2DUP 2>R					\ S: src u dst		R: u dst
	\ Append the input string just after clit in the data space.
	CHAR+ SWAP					\ S: src dst' u		R: u dst
	MOVE						\ S: --				R: u dst
	\ Save the string length.
	2R> C!						\ S: --				R: --
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
4 /PAD CHARS * CONSTANT _str_buf_max

_str_buf_max 1- CONSTANT _str_buf_mask

\ Transient string buffers of size /PAD.
_str_buf_max BUFFER: _str_bufs

\ Last used buffer.
VARIABLE _str_buf_curr

\ ( -- caddr )
: _str_buf_next
	_str_buf_curr @ /PAD CHARS +
	_str_buf_mask AND DUP
	_str_buf_curr !
	_str_bufs +
;

\ ... slit ...
\
\ (S: -- caddr u )
\
\ @note
\	The current IP is used to find and compute the
\	address and length of the string stored within the word.
\	It is then modified to point to just after the string.
\
: slit							\ S: -- 				R: ip
	R@ @ R>						\ S: u ip 				R: --
	CELL+ SWAP 2DUP				\ S: caddr u caddr u 	R: --
	CHAR+						\ Account for terminating NUL byte.
	CHARS + ALIGNED				\ S: caddr u ip' 		R: --
	>R							\ S: caddr u 			R: ip'
; $02000002 _pp!

\ (C: src u -- ) (S: src u -- caddr u )
: SLITERAL
		POSTPONE slit			\ S: src u
		\ Append length.
		DUP ,					\ S: src u
		\ Append string and NUL terminate for C.
		DUP >R reserve R>		\ S: src dst u
		MOVE 0 C, ALIGN			\ S: --
; IMMEDIATE compile-only

\ (S: caddr1 u -- caddr2 u' )
: _string0_store
	STATE @ IF
		POSTPONE SLITERAL
	ELSE
		_str_buf_next SWAP 2DUP 2>R strncpy 2R>
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

\ (S: caddr1 u1 caddr2 u2 xt -- 1 | 0 | -1 )
: _strcmp
	>R RISE 2DUP - >R				\ S: s1 s2 u1 u2		R: xt du
	DROP OVER CHARS + >R			\ S: s1 s2				R: xt du s2"
	BEGIN DUP R@ < WHILE
		C@+ >R SWAP					\ S: s2' s1				R: xt du s2" c2
		C@+ >R SWAP					\ S: s1' s2'			R: xt du s2" c2 c1
		R> 3 rpick EXECUTE			\ S: s1' s2' c1'		R: xt du s2" c2
		R> 2 rpick EXECUTE			\ S: s1' s2' c1' c2'	R: xt du s2"
		- ?DUP IF					\ S: s1' s2' diff		R: xt du s2"
			\ Different stings at character.
			NIP NIP	2rdrop rdrop	\ S: diff
			0< IF -1 ELSE 1 THEN
			EXIT					\ S: -1 | 1
		THEN
	REPEAT
	2drop rdrop						\ S:					R: xt du
	\ Matching leading strings.
	R> rdrop DUP IF					\ S: du					R:
		0< IF -1 ELSE 1 THEN
	THEN
; $51 _pp!

\ Same behaviour as C's strcmp().
\ (S: caddr1 u1 caddr2 u2 -- 1 | 0 | -1 )
: strcmp ['] _nop _strcmp ; $41 _pp!

\ Same behaviour as C's strcasecmp().
\ (S: caddr1 u1 caddr2 u2 -- 1 | 0 | -1 )
: strcasecmp ['] tolower _strcmp ; $41 _pp!

\ ... COMPARE ...
\
\ (S: caddr1 u1 caddr2 u2 -- -1 | 0 | 1 )
\
' strcmp alias COMPARE

\ (S: caddr1 u1 caddr2 u2 -- bool )
: starts-with
	ROT SWAP 2DUP <				\ S: s1 s2 u1 u2
	IF
		\ String too short to start with prefix.
		2DROP 2DROP FALSE
		EXIT					\ S: bool
	THEN
	NIP DUP						\ S: s1 s2 u2 u2
	-ROT						\ S: s1 u2 s2 u2
 	strcmp 0=					\ S: bool
;

\ ... SEARCH ...
\
\ (S: caddr1 u1 caddr2 u2 -- caddr u bool )
\
: SEARCH
	2>R 2DUP					\ S: caddr1 u1 caddr1 u1 		R: caddr2 u2
	BEGIN ?DUP WHILE
		2DUP 2R@ starts-with	\ S: caddr1 u1 caddr1 u1 bool 	R: caddr2 u2
		IF
			R> R> 2DROP			\ S: caddr1 u1 caddr1' u1' 		R: --
			2SWAP 2DROP	TRUE	\ S: caddr1' u1' true
			EXIT
		THEN
		1- SWAP CHAR+ SWAP		\ S: caddr1 u1 caddr' u1' 		R: caddr2 u2
	REPEAT
	DROP R> R> 2DROP			\ S: caddr1 u1 					R: --
	FALSE						\ S: caddr1 u1 false
;

\ (S: bool caddr u -- )
: _abort_msg?
	ROT IF
		catch_frame @ 0= IF TYPE THEN
		-2 THROW
	THEN
	2DROP
;

\ : X ... test ABORT" message" ...
\
\ (C: ccc<quote>" -- ) (S: i*x y --	| i*x ) ( R: j*x --	| j*x )
\
: ABORT" POSTPONE S\" POSTPONE _abort_msg? ; IMMEDIATE compile-only

: [DEFINED] ( <space>name -- bool ) PARSE-NAME FIND-NAME 0<> ; IMMEDIATE
: [UNDEFINED] ( <space>name -- bool ) PARSE-NAME FIND-NAME 0= ; IMMEDIATE

: [ELSE] ( -- )
	1 BEGIN 									\ level
		BEGIN PARSE-NAME DUP WHILE				\ level adr len
			2DUP S" \" COMPARE 0= IF
				\ Ignore remainder of comment line.
				2DROP POSTPONE \
			ELSE 2DUP S" [IF]" strcasecmp 0= IF	\ level adr len
				2DROP 1+						\ level'
			ELSE								\ level adr len
				2DUP S" [ELSE]" strcasecmp 0= IF	\ level adr len
					2DROP 1-					\ level'
					\ Not yet zero, then restore previous level while nested.
					DUP IF 1+ THEN				\ level'
				ELSE 							\ level adr len
					S" [THEN]" strcasecmp 0= IF	\ level
						1-						\ level'
					THEN
				THEN
			THEN THEN
			?DUP 0= IF EXIT THEN				\ level'
		REPEAT 2DROP							\ level
	REFILL 0= UNTIL								\ level
	DROP
; IMMEDIATE

: [IF] ( flag -- )
	0= IF POSTPONE [ELSE] THEN
; IMMEDIATE

: [THEN] ( -- ) ; IMMEDIATE

[DEFINED] WRITE-FILE [IF]
\ ( caddr u fid -- ior )
: WRITE-LINE
	DUP >R WRITE-FILE DROP
	S\" \r\n" R> WRITE-FILE
;
[THEN]

\ (S: ctx -- aaddr )
: ctx.words ctx.active @ ;

\ (S: -- )
: _input_ptr _ctx ctx.input ;

\ (S: -- aaddr )
: BLK _input_ptr @ in.blk ; $01 _pp!

\ (S: -- aaddr )
: _block_ptr _ctx ctx.block ; $01 _pp!
: _blk_state _block_ptr @ blk.state ; $01 _pp!
: _blk_number _block_ptr @ blk.number ; $01 _pp!

\ (S: -- caddr )
: _blk_buffer _block_ptr @ blk.buffer ; $01 _pp!

\ (S: -- )
: _blk_free 0 _blk_state ! 0 _blk_number ! ;
: _blk_clean 1 _blk_state ! ;
: _blk_dirty 2 _blk_state ! ;
' _blk_free alias EMPTY-BUFFERS
' _blk_dirty alias UPDATE

\ (S: -- aaddr )
: _block_fd _ctx ctx.block_fd ; $01 _pp!

\ (S: -- )
: _block_flush _block_fd @ FLUSH-FILE DROP ;

\ (S: u -- ior )
: _block_seek
	_block_flush DUP IF
		1- _blk_size * S>D _block_fd @ REPOSITION-FILE
	THEN
; $11 _pp!

\ (S: u -- | ⊥ )
: _block_read
	DUP _block_seek THROW
	_blk_buffer _blk_size _block_fd @ READ-FILE
	0<> SWAP _blk_size <> AND -33 AND THROW
	_blk_number ! _blk_clean
; $10 _pp!

\ (S: u -- | ⊥ )
: _block_grow
	\ Switch from block # to file offset.
	_blk_size *							\ S: u'
	_block_fd @ FILE-SIZE 0<> -66 AND THROW
	D>S									\ S: u' v
	\ Is the file already large enough?
	2DUP U> IF							\ S: u' v
		_block_flush
		\ Seek end of file.
		DUP S>D _block_fd @ REPOSITION-FILE THROW
		\ Temporary blank buffer, skip allocation.
		HERE DUP >R _blk_size BLANK		\ S: u' v adr	R: adr
		\ Extend the file with blank blocks.
		BEGIN
			2DUP U>						\ S: u'	v		R: adr
		WHILE
			\ Next block.
			_blk_size +					\ S: u'	v'		R: adr
			\ Append a blank block.
			R@ _blk_size _block_fd @ WRITE-FILE THROW
		REPEAT
		rdrop							\ S: u' v'		R:
	THEN
	2DROP								\ S:
; $10 _pp!

\ (S: u -- | ⊥ )
: _block_write
	DUP
	_block_grow _block_seek THROW
	_blk_buffer _blk_size _block_fd @ WRITE-FILE THROW
; $10 _pp!

\ (S: -- )
: SAVE-BUFFERS _blk_number @ _block_write _blk_clean _block_fd @ FLUSH-FILE DROP ;
: FLUSH SAVE-BUFFERS EMPTY-BUFFERS ;

\ (S: u xt -- aaddr )
: _block_or_buffer
	\ Block zero?
	OVER 0= -35 AND THROW
	\ Block aready loaded?
	OVER _blk_number @ <> IF
		\ Block is dirty?
		_blk_state @ 2 = IF
			 _blk_number @ _block_write
		THEN
		\ Read block for BLOCK (not BUFFER).
		OVER SWAP EXECUTE
	ELSE
		DROP
	THEN
 	_blk_number ! _blk_clean _blk_buffer
; $21 _pp!

\ (S: u -- aaddr )
: BLOCK ['] _block_read _block_or_buffer ; $11 _pp!

\ (S: u -- aaddr )
: BUFFER ['] DROP _block_or_buffer ; $11 _pp!

\ (S: -- )
: BLOCK-CLOSE
	_blk_number @ _block_write _block_fd @ CLOSE-FILE DROP
	0 _block_fd ! _blk_free
;

\ (S: caddr u -- ior )
: BLOCK-OPEN
	0 _blk_number !
	2DUP R/W BIN OPEN-FILE IF
		DROP R/W BIN CREATE-FILE ?DUP IF
			NIP EXIT
		THEN
	ELSE
		>R 2DROP R>
	THEN
	_block_fd ! 0
; $21 _pp!

\ (S: -- u )
: BLOCKS _block_fd @ FILE-SIZE DROP D>S _blk_size / ; $01 _pp!

\ (S: -- addr )
: _input_new
	p4_input ALLOCATE THROW
	DUP p4_input ERASE DUP in.data OVER in.buffer ! -1 OVER in.fp !
; $01 _pp!

\ (S: -- ; R: -- addr )
: _input_push R> _input_ptr @ >R >R _input_new _input_ptr ! ; $0100 _pp!

\ (S: -- ; R: addr -- )
: _input_pop _input_ptr @ FREE THROW R> R> _input_ptr ! >R ; $1000 _pp!

\ (S: -- )
: _block_push
	SAVE-BUFFERS
	R> _block_ptr @ >R >R
	p4_block ALLOCATE DROP
	DUP _block_ptr ! 0 OVER blk.number ! 0 SWAP blk.state !
;

\ (S: -- )
: _block_pop
	_block_ptr @ FREE DROP
	R> R> _block_ptr ! >R
;

\ (S: i*x u -- j*x )
: LOAD
	_input_push _block_push
	DUP BLK ! BLOCK _blk_size ['] _evaluate CATCH
	_block_pop _input_pop THROW
; $10 _pp!

\ ... EVALUATE ...
\
\ ( i*x caddr u -- j*x )
\
\ @see
\	https://forth-standard.org/standard/block/EVALUATE
\
: EVALUATE _input_push 0 BLK ! ['] _evaluate CATCH _input_pop THROW ; $20 _pp!

\ (S: start end -- )
: THRU
	>R >R						\ S:		R: end start
	BEGIN
		R@ LOAD					\ S: 		R: end start
		R> 1+ DUP >R			\ S: start'	R: end start'
		1 rpick >				\ S: bool	R: end start'
	UNTIL 2rdrop
;

\ ... \ comment to end of line
\
\ (S: ccc<eol>" -- )
\
: \
	BLK @ IF					( Block input source? )
		>IN @ $3F OR 1+ >IN !	(	 Advance >IN to next line in 16x64 block. )
	ELSE						( Streaming input... )
		'\n' PARSE 2DROP		(	 Skip up to and including newline. )
	THEN
; IMMEDIATE

\ (S: -- aaddr )
VARIABLE SCR

\ (S: u -- )
: LIST
	DUP SCR !					\ S: u
	BLOCK						\ S: caddr
	0 BEGIN						\ S: caddr i
		1+ DUP 2 .R				\ S: caddr i'
		[CHAR] | EMIT
		OVER 64 TYPE			\ S: caddr i'
		[CHAR] | EMIT CR
		SWAP 64 CHARS +			\ S: i' caddr'
		SWAP DUP 16 >=			\ S: caddr' i' bool
	UNTIL 2DROP
;

\ ... LIST+ ...
\
\ (S: -- )
\
: list+ SCR @ 1+ LIST ;

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

\ (S: i*x fd -- j*x )
\ *** An uncaught exception within the include file will leak the file
\ *** handle and some memory.
: INCLUDE-FILE
	_input_push DUP >R ['] _eval_file CATCH
	R> CLOSE-FILE DROP _input_pop THROW
; $10 _pp!

\ (S: i*x caddr u -- j*x )
: INCLUDED R/O OPEN-FILE THROW INCLUDE-FILE ; $20 _pp!

\ (S: i*x caddr u -- j*x )
: included-path S" POST4_PATH" env 2SWAP R/O open-file-path THROW INCLUDE-FILE ; $20 _pp!

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

\ ... NAME>STRING ...
\
\ ( nt -- caddr u )
\
: NAME>STRING
		DUP w.name @			\ S: nt
		SWAP w.length @			\ S: name length
;

\ Find word ignoring hidden bit.
\ (S: <spaces>name -- xt | 0 )
: ''
	PARSE-NAME 2>R				\ S: 			R: c u
	_ctx ctx.words @			\ S: p
	BEGIN
		DUP NAME>STRING 2R@		\ S: p c u d v	R: c u
		COMPARE 0= IF
			2rdrop EXIT
		THEN
		w.prev @ DUP			\ S: p' p'		R: c u
	whilst						\ S: p'
	DROP 2rdrop					\ S: 			R:
	-13 THROW
;

\ (S: wid -- wid' )
\ wid = 0 reserved for locals word list.
: check_wid 1- DUP -1 WORDLISTS WITHIN 0= -257 AND THROW ;

\ (S: wid -- addr )
: head_of_wordlist check_wid CELLS _ctx ctx.lists + ; $11 _pp!

\ (S: wid -- )
: words-in
	0 >R								\ S: --					R: col
	head_of_wordlist					\ S: head				R: col
	BEGIN @ DUP WHILE					\ S: w					R: col
		w.bit_hidden OVER _word_bit? 0= IF
			DUP NAME>STRING				\ S: w word				R: col
			DUP R> + 1+					\ S: w name length col' R: --
			\ Does current column exceed terminal width?
			DUP _window NIP >= IF 		\ S: w name length col' R: --
				CR DROP DUP 1+			\ S: w name length col" R: --
			THEN						\ S: w name length col	R: --
			>R TYPE SPACE				\ S: w					R: col
		THEN
		w.prev							\ S: w'					R: col
	REPEAT
	R> 2DROP CR							\ S: --					R: --
;

\ ( -- )
: WORDS _ctx ctx.order @ words-in ;

\ (S: -- word )
: _pop_word
	\ Pop enclosing defintion being compiled.
	_ctx ctx.words @			\ S: word
	DUP w.prev @				\ S: word prev
	_ctx ctx.words !			\ S: word
;

\ (S: word -- )
: _push_word
	\ Push previous enclosing defintion being compiled.
	DUP w.prev					\ C: word prev
	_ctx ctx.words @			\ C: word prev curr
	SWAP ! 						\ C: word
	_ctx ctx.words !			\ C:
;

\ : newword ... [: nested words ;] ... ;
\
\ (C: -- quotation-sys colon-sys )
\
: [:
	POSTPONE AHEAD				\ C: forw
	STATE @						\ S: forw state
	_pop_word					\ S: forw state word
	\ Start nested definition.
	:NONAME						\ C: forw state word xt
; IMMEDIATE compile-only

\ : newword ... [: nested words ;] ... ;
\
\ (C: forw state curr xt -- ) || (S: -- xt )
\
: ;]
	\ End current nested definition.
	POSTPONE ;					\ C: forw state curr xt
	POSTPONE _nop				\ Used by _see_enter
	>R							\ C: forw state curr	R: xt
	_push_word					\ C: forw state
	STATE !						\ C: forw
	POSTPONE THEN				\ C:					R: xt
	R> POSTPONE LITERAL	\ C:
; IMMEDIATE compile-only

: stack_new ( u <spaces>name -- ) CREATE 1+ CELLS reserve DUP CELL+ SWAP ! ;
: stack_tmp ( u -- stack ) 1+ CELLS ALLOCATE THROW DUP DUP CELL+ SWAP ! ;
: stack_push ( n stack -- ) TUCK @ ! /CELL SWAP +! ;
: stack_pop ( stack -- n ) /CELL NEGATE OVER +! DUP @ TUCK	>= ABORT" tmp. stack underflow" @ ;
: stack_length ( stack -- n ) DUP @ SWAP - /CELL / 1- ;

VARIABLE _do_sys_stk
: _do_sys_new ( -- ) _do_sys_stk @ 8 stack_tmp DUP _do_sys_stk ! stack_push ;
: _do_sys_end ( -- ) _do_sys_stk @ DUP stack_pop _do_sys_stk ! FREE DROP ;

\ ... limit first DO ... LOOP ...
\
\ (C: -- dest ) || (S: limit first -- )(R: -- limit first )
\
: DO
	POSTPONE 2>R				\ S:			R: lm ix
	_do_sys_new
	POSTPONE BEGIN				\ C: dest
; IMMEDIATE compile-only

\ ... limit first ?DO ... LOOP ...
\
\ (C: -- dest ) || (S: limit first -- )(R: -- limit first )
\
: ?DO
	POSTPONE 2>R				\ S:			R: lm ix
	POSTPONE 2R@				\ S: lm ix		R: lm ix
	POSTPONE <>					\ S: bool
	_do_sys_new
	POSTPONE IF					\ C: forw 		R: lm ix
	_do_sys_stk @ stack_push
	POSTPONE BEGIN				\ C: dest
; IMMEDIATE compile-only

\ : X ... limit first DO ... test IF ... UNLOOP EXIT THEN ... LOOP ... ;
\
\ (S: --	) (R: limit index ip -- ip )
: UNLOOP R> 2rdrop >R ; compile-only

\ ... limit first DO ... IF ... LEAVE THEN ... LOOP ...
\
\ (C: -- )
: LEAVE
	POSTPONE AHEAD
	_do_sys_stk @ stack_push
; IMMEDIATE compile-only

\ (S: -- index )(R: limit index ip -- limit index ip )
: I	R> R@ SWAP >R ; compile-only

\ (S: -- index1 )(R: limit1 index1 limit0 index0 ip -- limit1 index1 limit0 index0 ip )
: J 3 rpick ; compile-only

\ ... limit first DO ... LOOP ...
\
\ (S: -- bool ) (R: limit index -- limit index' )
\
\ @note
\	Can count from zero up to the unsigned maximum possible in one cell,
\	therefore 0 0 DO ... LOOP iterates UINT_MAX+1 times.
\
: _loop_inc_test				\ S: 			R: lm ix' r0
	R> 2R> 1+					\ S: r0 lm ix'
	2DUP 2>R					\ S: r0 lm ix'	R: lm ix'
	=							\ S: r0 bool	R: lm ix'
	SWAP >R						\ S: bool		R: lm ix' r0
;

\ (S: n -- bool ) (R: limit index -- limit index' )
: _loop_step_test				\ S: n			R: l x r0
	R> SWAP						\ S: r0 n 		R: l x
	\ Increment index.
	R> DUP ROT + DUP >R			\ S: r0 x x'	R: l x'

	\ (x-l) xor (x'-l) < 0
	1 rpick -					\ S: r0 x d'	R: l x'
	SWAP 1 rpick -				\ S: r0 d' d	R: l x'
	XOR 0<						\ S: r0 bool	R: l x'

	\ Restore return stack.
	SWAP >R						\ S: bool		R: lm ix' r0
;

\ (C: dest -- )
: _loop_control
	POSTPONE UNTIL

	BEGIN
		_do_sys_stk @ stack_length 1 >
	WHILE
		_do_sys_stk @ stack_pop
		POSTPONE THEN
	REPEAT
	_do_sys_end

	POSTPONE UNLOOP
;

\ ... limit first DO ... LOOP ...
\ (C: n*forw n dest -- ) || (S: -- )(R: limit index -- )
: LOOP
	POSTPONE _loop_inc_test
	_loop_control
; IMMEDIATE compile-only

\ ... limit first DO ... +step LOOP ...
\ (C: n*forw n dest -- ) || (S: -- )(R: limit index -- )
: +LOOP
	POSTPONE _loop_step_test
	_loop_control
; IMMEDIATE compile-only

[DEFINED] _fs [IF]
_fs CONSTANT floating-stack DROP DROP
' _fsp@ IS fsp@
' _fsp! IS fsp!

' CELLS alias FLOATS
1 FLOATS CONSTANT /FLOAT
: FLOAT+ /FLOAT + ;

: FALIGN ALIGN ;
: FALIGNED ALIGNED ;
: FFIELD: FIELD: ;

: .fs S\" fs\r\n" TYPE _fs DROP _stack_dump ;

\ (S: -- f )(F: f -- )
\ Move f between stacks _without_ conversion; F>S and S>F convert formats.
: fs>ds fs>rs R> ;
: ds>fs >R rs>fs ;

\ (F: f -- )(S: -- dl dh )
: F>D F>S S>D ;

\ (F: -- f )(S: dl dh -- )
: D>F D>S S>F ;

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
: flit R> DUP FLOAT+ >R F@ ; $01000001 _pp!

\ Similar to LIT,
\ (F: f -- )
: flit, ['] flit COMPILE, F, ;

\ ... : name ... [ x.y ] FLITERAL ... ;
\
\	(C: f -- ) (F: -- f )
\
: FLITERAL flit, ; IMMEDIATE compile-only

\ (C: F:x <spaces>name -- ) (F: -- x )
: FCONSTANT CREATE F, DOES> F@ ;

\ (C: <spaces>name -- ) (S: -- aaddr )
: FVARIABLE VARIABLE ;

: FVALUE CREATE 0 , F, DOES> CELL+ F@ ;

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
	>IN @ ' >BODY @ 			\ S: v in type
	SWAP >IN !					\ S: v type
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

\ IEEE 754-2019 aka IEC 60559
cell-bits 64 = [IF]
' F@ alias DF@
' F! alias DF!
' ALIGN alias DFALIGN
' ALIGNED alias DFALIGNED
' FLOAT+ alias DFLOAT+
' FLOATS alias DFLOATS
$000FFFFFFFFFFFFF CONSTANT _significand_mask
$7FF0000000000000 CONSTANT _exponent_mask
1023 CONSTANT _exponent_bias
MIN-N CONSTANT _sign_mask
52 CONSTANT _significand
11 CONSTANT _exponent
[THEN]

cell-bits 32 = [IF]
' F@ alias SF@
' F! alias SF!
' ALIGN alias SFALIGN
' ALIGNED alias SFALIGNED
' FLOAT+ alias SFLOAT+
' FLOATS alias SFLOATS
$007FFFFF CONSTANT _significand_mask
$7F800000 CONSTANT _exponent_mask
127 CONSTANT _exponent_bias
MIN-N CONSTANT _sign_mask
23 CONSTANT _significand
 8 CONSTANT _exponent
[THEN]

cell-bits 16 = [IF]
$03FF CONSTANT _significand_mask
$7C00 CONSTANT _exponent_mask
15 CONSTANT _exponent_bias
MIN-N CONSTANT _sign_mask
10 CONSTANT _significand
 5 CONSTANT _exponent
[THEN]
[THEN]

[DEFINED] _significand_mask [IF]
: _exp_sig
	fs>ds DUP 							\ S: r r
	_exponent_mask TUCK AND =			\ S: r b1
	SWAP _significand_mask AND			\ S: b1 b2
 ; $100002 _pp!

\ (S: -- bool )(F: r -- )
\ If e(r) == 2^w-1 && m(r) != 0 then NaN.
: nan? _exp_sig 0<> AND ; $100001 _pp!

\ (S: -- -n | 0 | +n )(F: r -- )
\ If e(r) == 2^w-1 && m(r) == 0 then (-1)^s*INF.
: inf? _exp_sig 0= AND ; $100001 _pp!
[THEN]

\ (S: u -- )
: #. BASE @ >R DECIMAL . R> BASE ! ; $10 _pp!
: $. BASE @ >R HEX '$' EMIT U. R> BASE ! ; $10 _pp!

\ (S: x -- )
: $#. DUP -65536 65536 WITHIN IF #. ELSE $. THEN ; $10 _pp!

\ (S: addr u -- )
: .cells
	bounds BEGIN 2DUP > WHILE
		DUP @ $#. CELL+
	REPEAT 2DROP
; $20 _pp!

\ (S: end beg -- )
: _dump_chars
	BEGIN 2DUP U> WHILE
		DUP C@ DUP isprint 0= IF
			DROP '.'
		THEN EMIT
		1 CHARS +
	REPEAT 2DROP
; $20 _pp!

\ (S: end beg -- )
: _dump_pad
	16 CHARS + SWAP
	BEGIN 2DUP U> WHILE
		S\" \s\s\s" TYPE
		1 CHARS +
	REPEAT 2DROP
; $20 _pp!

\ (S: end beg -- )
: _dump_row
	DUP >R 16 CHARS +					\ S: e b"		R: b
	2DUP U> IF NIP ELSE DROP THEN		\ S: e'			R: b
	R> 2DUP 2>R							\ S: e' b		R: e' b
	DUP $. SPACE
	BEGIN 2DUP U> WHILE					\ S: e' b		R: e' b
		DUP C@ S>D						\ S: e' b c 0	R: e' b
		<# BL HOLD # # #> TYPE 			\ S: e' b		R: e' b
		1 CHARS +						\ S: e' b'		R: e' b
	REPEAT 2DROP
	2R@ _dump_pad 2R> SPACE _dump_chars CR
; $20 _pp!

\ (S: addr u -- )
: DUMP
	CHARS bounds
	BASE @ >R HEX
	BEGIN 2DUP U> WHILE
		2DUP _dump_row
		16 CHARS +
	REPEAT
	2DROP R> BASE !
; $20 _pp!

\ (S: xt wid -- bool )
: xt_in?
	head_of_wordlist
	BEGIN @ DUP WHILE
		2DUP = IF 2DROP TRUE EXIT THEN
		w.prev
	REPEAT
	2DROP FALSE
; $21 _pp!

\ (S: xt -- bool )
: xt?
	WORDLISTS 1 DO
		DUP I xt_in? IF DROP TRUE UNLOOP EXIT THEN
	LOOP
	DROP FALSE
; $11 _pp!

\ ( c -- c' )
: _literal_backspace
	CASE								\ S: char
		$0A OF [CHAR] n ENDOF			\ S: ascii char
		$0D OF [CHAR] r ENDOF			\ S: ascii char
		$09 OF [CHAR] t ENDOF			\ S: ascii char
		$1B OF [CHAR] e ENDOF			\ S: ascii char
		\ Less frequent
		$07 OF [CHAR] a ENDOF			\ S: ascii char
		$08 OF [CHAR] b ENDOF			\ S: ascii char
		$0C OF [CHAR] f ENDOF			\ S: ascii char
		$0B OF [CHAR] v ENDOF			\ S: ascii char
		$00 OF [CHAR] z ENDOF			\ S: ascii char
		$7F OF [CHAR] ? ENDOF			\ S: ascii char
		0 SWAP							\ S: ascii char
	ENDCASE								\ S: ascii
; $11 _pp!

\ (S: caddr u -- )
: \type
	CHARS bounds								\ S: b a
	BEGIN 2DUP > WHILE					\ S: b a
		C@+ DUP _literal_backspace		\ S: b a' c e
		?DUP IF							\ S: b a' c
			NIP '\' EMIT				\ S: b a' e
		THEN
		EMIT 							\ S: b a'
	REPEAT 2DROP
; $20 _pp!

\ (S: ip -- ip' )
\ Test: SEE ['] SEE LIT, SEE AT-XY
: _see_lit
	CELL+ DUP @							\ S: ip' x
	DUP xt? DUP IF 						\ S: ip' x b1
		DROP DUP NAME>STRING NIP 0<> 	\ S: ip' x b2
	THEN IF								\ S: ip' x
		S" [ ' " TYPE NAME>STRING TYPE S\" \s] LITERAL " TYPE
	ELSE
		DUP BL = IF
			#. S" ( '\s' ) " TYPE
		ELSE
			DUP isgraph IF
				DUP #. S" ( '" TYPE EMIT S" ' ) " TYPE
			ELSE
				$#.
			THEN
		THEN
	THEN								\ S: ip'
; $11 _pp!

\ (S: ip -- ip' )
: _see_clit
	S\" C\\\" " TYPE					\ S: ip
	CELL+ DUP C@						\ S: ip1 u
	SWAP CHAR+ SWAP 2DUP				\ S: ip2 u a u
	\type								\ S: ip2 u
	S\" \" " TYPE						\ S: ip2 u
	CHARS + ALIGNED	CELL-				\ S: ip3
; $11 _pp!

\ (S: ip -- ip' )
\ Test: SEE PAGE SEE AT-XY
: _see_slit
	S\" S\\\" " TYPE					\ S: ip
	CELL+ DUP @							\ S: ip1 u
	SWAP CELL+ SWAP 2DUP				\ S: ip2 u ip2 u
	\type								\ S: ip2 u
	S\" \" " TYPE						\ S: ip2 u
	CHAR+ CHARS + ALIGNED CELL-			\ S: ip3
; $11 _pp!

[DEFINED] _fs [IF]
\ (S: ip -- ip' )
: _see_flit
	FLOAT+ DUP F@ F.
;
[THEN]

\ (S: ip -- ip' )
: _see_common
	DUP @ NAME>STRING TYPE SPACE
; $11 _pp!

\ (S: ip -- ip' )
\ Test: SEE THROW SEE ABS SEE FIND
: _see_bra
	_see_common
	CELL+ DUP @ /CELL /
	S" [ " TYPE #. S" CELLS , ] " TYPE
; $11 _pp!

\ (S: xt -- )
\ Test most words, eg. SEE IF SEE ['] SEE \ SEE LIT,
: _see_enter
	DUP NAME>STRING ?DUP IF
		S" : " TYPE TYPE SPACE
	ELSE
		DROP S" :NONAME " TYPE
	THEN
	DUP w.data @ BEGIN					\ S: xt ip
		DUP @ ['] _; <>					\ S: xt ip b1
		OVER CELL+ @ ['] _nop =			\ S: xt ip b2
	OR WHILE							\ S: xt ip
		DUP @ CASE						\ S: xt ip wp
			['] LIT	OF _see_lit ENDOF
			['] slit OF _see_slit ENDOF
			['] clit OF _see_clit ENDOF
[DEFINED] flit [IF]
			['] flit OF _see_flit ENDOF
[THEN]
			['] _branch OF _see_bra ENDOF
			['] _branchz OF _see_bra ENDOF
			['] _branchnz OF _see_bra ENDOF
			['] _call OF _see_bra ENDOF
			DROP DUP _see_common
		ENDCASE
		CELL+							\ S: xt ip"
	REPEAT
	DROP S" ; " TYPE DUP immediate? IF S" IMMEDIATE " TYPE THEN
	compile-only? IF S" compile-only" TYPE THEN CR
; $10 _pp!

\ (S: xt -- )
\ Test: SEE TRUE 123 VALUE x SEE x
: _see_dodoes
	\ Dump words' data.
	DUP w.data @	 					\ S: xt a
	OVER w.ndata @						\ S: xt a u
	\ data[0] = pointer to DOES>, data[n-1] = xt of defining word,
	\ see _does.  data[1..n-1] is the actual data.
	2DUP + cell- @						\ S: xt a u xt'
    DUP >R CASE							\ S: xt a u' xt'	R: xt'
		['] VALUE OF DROP CELL+ CELL+ /cell .cells ENDOF
		['] 2VALUE OF DROP CELL+ CELL+ DUP CELL+ @ $#. @ $#. ENDOF
[DEFINED] FVALUE [IF]
		['] FVALUE OF DROP CELL+ CELL+ F@ F. ENDOF
[THEN]
		-rot 2 CELLS - SWAP CELL+ SWAP .cells
	ENDCASE
	R> NAME>STRING TYPE SPACE			\ S: xt				R:
	NAME>STRING TYPE CR					\ S:
; $10 _pp!

\ (S: xt -- )
\ Test: CREATE y 1 , 2 , 3 , SEE y
: _see_data
	S" CREATE " TYPE DUP NAME>STRING TYPE
	S\" \s( size " TYPE DUP w.ndata @ CELL- DUP >R #. S\" )" TYPE CR
	w.data @ CELL+ R> DUMP
; $10 _pp!

\ (S: xt -- )
\ Test: SEE LIT SEE CREATE
: _see_internal
	S" : " TYPE DUP NAME>STRING TYPE
	S\" \s( code " TYPE w.code @ $. S" ) ;" TYPE CR
; $10 _pp!

\ Used to extract the default code field for a CREATEd word.
CREATE _nada

\ (S: xt -- )
: _seext
	DUP w.code @ CASE
		[ ' #. w.code @ ] LITERAL OF _see_enter ENDOF
		[ ' TRUE w.code @ ] LITERAL OF _see_dodoes ENDOF
		[ ' _nada w.code @ ] LITERAL OF _see_data ENDOF
		SWAP _see_internal
	ENDCASE
; $10 _pp!

\ ' _nada hide
\ ' _see_data hide
\ ' _see_enter hide
\ ' _see_dodoes hide
\ ' _see_internal hide
\ ' _see_lit hide
\ ' _see_clit hide
\ ' _see_flit hides
\ ' _see_slit hide
\ ' _see_bra hide
\ ' _see_common hide

\ (S: <spaces>name -- )
: SEE ' _seext ;

\ Using locals
\ : EMITS {: n char -- :} n 0 ?do char emit loop ;
: EMITS ( n char -- )	SWAP 0 ?DO DUP EMIT LOOP DROP ; $20 _pp!

1 CONSTANT FORTH-WORDLIST

\ (S: -- wid )
: GET-CURRENT _ctx ctx.words _ctx ctx.lists - /CELL / 1+ ; $01 _pp!

\ (S: wid -- )
: SET-CURRENT head_of_wordlist _ctx ctx.active ! ; $10 _pp!

FORTH-WORDLIST SET-CURRENT

\ (S: -- wid )
: WORDLIST
	_ctx ctx.lists DUP WORDLISTS CELLS + SWAP
	BEGIN
		DUP @ 0= IF
			NIP _ctx ctx.lists - /CELL / 1+
			EXIT
		THEN
		CELL+ 2DUP U>
	WHILST
	2DROP -257 THROW
; $01 _pp!

\ (S: -- )
: FORTH FORTH-WORDLIST _ctx ctx.order ! ;

\ (S: -- widn ... wid1 n )
: GET-ORDER
	_ctx ctx.order DUP					\ S: p p
	_ctx ctx.norder @ CELLS +			\ S: p p"
	2>R									\ S: 		R: p p'
	BEGIN
		2R@ U<
	WHILE
		R> CELL- DUP >R @				\ S: ... w	R: p p"
	REPEAT
	2rdrop _ctx ctx.norder @			\ S: wn..w1 n
;

\ (S: wid1 ... widn n -- )
: SET-ORDER
	DUP 0< IF
		\ System default word lists.
		DROP FORTH-WORDLIST 1
	THEN
	\ Too many word lists?
	DUP 0 WORDLISTS 1+ WITHIN 0= -49 AND THROW
	DUP _ctx ctx.norder !				\ S: wn..w1 n
	CELLS _ctx ctx.order TUCK + 		\ S: wn..w1 p p"
	>R >R								\ S: wn..w1 	R: p" p
	BEGIN
		2R@ U>							\ S: wn.. p b	R: p" p
	WHILE
		R> DUP CELL+ >R !				\ S: wn.. 		R: p" p'
	REPEAT
	2rdrop
; $10 _pp!

\ (S: i*x xt wid -- j*x )
: TRAVERSE-WORDLIST
	SWAP >R head_of_wordlist			\ S: w			R: xt
	BEGIN @ DUP WHILE					\ S: w			R: xt
		DUP w.length @ IF				\ S: w			R: xt
			R@ OVER >R EXECUTE 0= IF	\ S: w xt		R: xt w
				2rdrop EXIT
			THEN
		THEN
		R> w.prev						\ S: w'			R: xt
	REPEAT
	R> 2DROP
; $20 _pp!

\ (S: caddr u wid -- 0 | xt -1 | xt 1 )
: SEARCH-WORDLIST
	FIND-NAME-IN DUP IF
		DUP immediate? IF				\ S: xt
			1							\ S: xt 1
		ELSE
			-1							\ S: xt -1
		THEN
	THEN
; $30 _pp!

\ (S: -- )
: ONLY -1 SET-ORDER ;
: PREVIOUS GET-ORDER NIP 1- SET-ORDER ;
: ALSO GET-ORDER OVER SWAP 1+ SET-ORDER ;
: DEFINITIONS GET-ORDER OVER SET-CURRENT SET-ORDER ;

: show_wid ( wid -- ) S\" \e[36m[ " TYPE #. S\" ]\e[0m\r\n" TYPE ; $10 _pp!

: ORDER ( -- )
	." Search: " GET-ORDER 0 DO . SPACE LOOP CR
	." Define: " GET-CURRENT . CR
;

WORDLIST CONSTANT required-wordlist

\ (S: <spaces>filename" -- )
: _parse_string0 PARSE-NAME _string0_store ;

\ (S: caddr u -- caddr u )
: _save_included
	\ Avoid adding duplicate files to the word-list.
	2DUP required-wordlist FIND-NAME-IN 0= IF
		GET-CURRENT >R required-wordlist SET-CURRENT
		2DUP _created R> SET-CURRENT
	THEN
;

\ (S: caddr u -- )
: INCLUDED _save_included INCLUDED ;
: included-path _save_included included-path ;

\ (S: i*x <spaces>filename" -- j*x )
: INCLUDE _parse_string0 INCLUDED ;
: include-path _parse_string0 included-path ;

\ (S: caddr u -- )
: _already_included 2DUP required-wordlist FIND-NAME-IN ;

\ (S: i*x caddr u -- j*x )
: REQUIRED _already_included IF 2DROP EXIT THEN INCLUDED ;
: required-path _already_included IF 2DROP EXIT THEN included-path ;

\ (S: i*x <spaces>filename" -- j*x )
: REQUIRE _parse_string0 REQUIRED ;
: require-path _parse_string0 required-path ;

: _free_word ( w -- )
	?DUP IF
		DUP w.name @ FREE DROP
		FREE DROP
	THEN
;

\ Free words from head of the word list down-to stop.
: _free_words ( stop wid -- )
	head_of_wordlist 2DUP 				\ S: stop ptr stop ptr
	\ Set new list head.
	@ >R SWAP ! R>						\ S: stop word
	\ Delete words from old list head to stop exclusive.
	BEGIN 2DUP <> WHILE					\ S: stop word
		DUP w.prev @ SWAP _free_word	\ S: stop word'
	REPEAT 2DROP
;

: MARKER ( <spaces>name -- )
	\ Collect head of all word lists BEFORE creating the marker.
	_ctx ctx.lists WORDLISTS 0 DO @+ SWAP LOOP DROP
	\ Save HERE and all the list pointers with the marker.
	GET-CURRENT HERE CREATE WORDLISTS -2 DO , LOOP
	\ Save search order.
	GET-ORDER DUP -1 DO , LOOP
	DOES>
	\ Restore HERE.
	@+ _ctx ctx.here !
	\ Restore compilation list.
	@+ SET-CURRENT
	\ Delete words from each list to restore earlier state.
	1 WORDLISTS DO @+ I _free_words -1 +LOOP
	\ Restore search order.
	DUP @ CELLS OVER + DO I @ /CELL NEGATE +LOOP SET-ORDER
;

\ GH-76
: set-source ( sd -- ) _ctx ctx.input @ TUCK in.length ! in.buffer ! ;
: execute-parsing ( any sd xt -- any ) _input_push -rot set-source CATCH _input_pop THROW ;

MARKER rm_user_words
