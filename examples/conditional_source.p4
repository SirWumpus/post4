\
\ [IF] ... [ELSE] ... [THEN] example
\

.( check for WORDS... ) CR

[DEFINED] WORDS [IF]
	.( has WORDS ) CR
	FALSE [if]
		.( this should not appear ) CR
	[else]
		.( Boo! ) CR
	[THEN]
[ELSE]
	.( 1 this should not appear ) CR
[then]

.( check for BOGUS... ) CR

[DEFINED] BOGUS [IF]
	.( 2 this should not appear ) CR
[ELSE]
	.( no BOGUS ) CR
[THEN]

.( done ) CR
