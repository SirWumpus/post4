\
\ http://forth.org/svfig/Len/softstak.htm
\

\ lifo ( n -- ) ( -- adr)
\	During compilation, create a LIFO with n cells.  On execution,
\	return the address of the stack.
\
\ : lifo CREATE HERE CELL+ , CELLS ALLOT DOES> ;

: lifo CREATE 1+ CELLS reserve DUP CELL+ SWAP ! ;

\ push ( n lifo -- )
\	Push number onto LIFO.
\
: push SWAP OVER @ ! /CELL SWAP +! ;

\ pop ( lifo -- n )
\	Pop number from LIFO.
\
: pop
     /CELL NEGATE OVER +! DUP @ SWAP OVER >=
     ABORT" [pseudostack underflow] " @
;

\ pclear ( lifo -- )
\	Clear LIFO.
\
: pclear DUP CELL+ SWAP ! ;

\ pbounds ( lifo -- addr1 addr2 )
\	Create parameters for a ?DO loop that will scan every item
\	currently in LIFO. The intended use is:
\
\	pbounds ?DO ... CELL +LOOP
\
: pbounds DUP @ SWAP CELL+ ;
