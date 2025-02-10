MARKER rm_factorial

\ 20 factorial is 2432902008176640000.
\ https://en.wikipedia.org/wiki/Factorial
: FACTORIAL ( +n1 -- +n2 )
 DUP 2 < IF DROP 1 EXIT THEN
 DUP
 BEGIN DUP 2 > WHILE
	1- SWAP OVER
	*
	SWAP
 REPEAT DROP
;
