: fizzbuzz ( n -- )
	1+ 1 DO
	  I 3 MOD 0= I 5 MOD 0= AND IF
	    ." Fizz Buzz " CR
	  ELSE I 3 MOD 0= IF
	    ." Fizz " CR
	  ELSE I 5 MOD 0= IF
	    ." Buzz " CR
	  ELSE
	    I U. CR
	  THEN THEN THEN
	LOOP
;
