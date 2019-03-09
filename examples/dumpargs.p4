\
\ Command line arguments example.
\

: dumpargs
	args 			\ S: argv argc
	0 ?DO			\ S: argv
	  DUP @			\ S: argv cstr
	  DUP strlen . puts CR	\ S: argv
	  CELL+			\ S: argv'
	LOOP DROP
;

dumpargs
