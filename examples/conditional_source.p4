\
\ [IF] ... [ELSE] ... [THEN] example
\

.( check for WORDS... ) CR

[DEFINED] WORDS [IF]
	.( has WORDS ) CR
	FALSE [IF]
		.( this should not appear ) CR
	[ELSE]
		.( Boo! ) CR
	[THEN]
[ELSE]
	.( 1 this should not appear ) CR
[THEN]

.( check for BOGUS... ) CR

[DEFINED] BOGUS [IF]
	.( 2 this should not appear ) CR
[ELSE]
	.( no BOGUS ) CR
[THEN]

.( done ) CR
