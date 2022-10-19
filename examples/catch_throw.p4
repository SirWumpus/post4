\ A.9.6.1.2275 THROW demo.

MARKER rm_catch_throw_demo

: could-fail ( -- char )
	." Type a key or ESC to trigger an exception." CR
	KEY DUP $1B = IF 1 THROW THEN
;

: do-it ( a b -- c ) 2DROP could-fail ;

: try-it ( -- )
	1 2 ['] do-it CATCH
	IF
		( x1 x2 ) 2DROP
		." There was an exception" CR
	ELSE
		." The character was " EMIT CR
	THEN
;

: retry-it ( -- )
	BEGIN
		1 2 ['] do-it CATCH
	WHILE
		( x1 x2) 2DROP
		." Exception, keep trying" CR
	REPEAT ( char )
	." The character was " EMIT CR
;

.( Enter TRY-IT or RETRY-IT words to test CATCH / THROW.
)
