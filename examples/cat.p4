\ Trivial example of how to do a cat(1) like pipe filter without
\ any convoluted constructs as proposed in Gforth.
: cat
	\ Save this file's input.
	SAVE-INPUT
	\ Switch to standard input.
	_stdin
	BEGIN
	  REFILL
	WHILE
	  SOURCE TYPE
	REPEAT
	\ Restore this file's input for proper EOF.
	RESTORE-INPUT
;
cat
