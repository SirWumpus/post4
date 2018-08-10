\ Trivial example of how to do a cat(1) like pipe filter without
\ any convoluted constructs as proposed in Gforth.
: cat
	BEGIN
	 REFILL 0= IF ." Wave!\n" TYPE BYE THEN
	 SOURCE TYPE
	AGAIN
;
cat

