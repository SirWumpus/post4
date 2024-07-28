\ One Dimensional Cellular Automata
\
\ https://en.wikipedia.org/wiki/Cellular_automaton
\
\ Scientific America, Computer Recreations, May 1985, pg. 18-30
\ Glider Gun Guidelines, Stephen Wolfram, March 1985
\

MARKER rm_life1d

0 VALUE life1d_size
#110 VALUE life1d_rule

CHAR # CONSTANT on
CHAR . CONSTANT off

0 VALUE life1d_curr
0 VALUE life1d_next

: life1d_swap ( -- )
	life1d_curr
	life1d_next TO life1d_curr
	TO life1d_next
;

: life1d_alloc ( n -- aaddr )
	ALLOCATE 0<> -59 AND THROW
;

: life1d_get_curr ( index -- bit )
	CHARS life1d_curr + C@
	on = negate
;

: life1d_set_next ( bool index -- )
	SWAP IF on ELSE off THEN SWAP
	CHARS life1d_next + C!
;

: life1d_curr_state ( index -- value )
	DUP 1- life1d_get_curr 2 LSHIFT SWAP
	DUP    life1d_get_curr 1 LSHIFT SWAP
	    1+ life1d_get_curr
	OR OR
;

: life1d_next_state ( value index -- )
	life1d_rule ROT RSHIFT 1 AND
	SWAP life1d_set_next
;

: life1d_transition ( -- )
	life1d_size 1- 0 DO
		I life1d_curr_state		\ value
		I life1d_next_state		\ --
	LOOP
;

\ Define initial life state; dot (.) for empty cell, hash (#) live cell.
: >life1d ( -- )
	\ Read line depicting state 0.
	REFILL 0= IF EXIT THEN
	BL PARSE				\ input u
	DUP TO life1d_size			\ input u

	\ Allocate life lines based on size of state 0.
	DUP life1d_alloc TO life1d_curr		\ input u
	DUP life1d_alloc TO life1d_next		\ input u

	\ Copy life state to the current line.
	life1d_curr SWAP 			\ input line u
	MOVE					\ --
;

\ Write N generations.
: life1d ( n -- )
	." Rule " life1d_rule U. CR
	0 4 U.R BL EMIT
	life1d_curr life1d_size TYPE CR
	1+ 1 DO
		I 4 U.R BL EMIT
		life1d_curr life1d_next life1d_size MOVE
		life1d_transition life1d_swap
		life1d_curr life1d_size TYPE CR
		KEY? IF KEY DROP CR LEAVE THEN
	LOOP
	life1d_curr FREE DROP
	life1d_next FREE DROP
	CR
;

>life1d
.###.##.#.#.#.#..#..
20 life1d

#30 TO life1d_rule
>life1d
.###.##.#.#.#.#..#..
20 life1d

#90 TO life1d_rule
>life1d
.###.##.#.#.#.#..#..
20 life1d

#184 TO life1d_rule
>life1d
.###.##.#.#.#.#..#..
10 life1d

#104 TO life1d_rule
>life1d
.###.##.#.#.#.#..#..
10 life1d
( See https://rosettacode.org/wiki/One-dimensional_cellular_automata#Forth
Rule 104 expected pattern.
.###.##.#.#.#.#..#..
.#.#####.#.#.#......
..##...##.#.#.......
..##...###.#........
..##...#.##.........
..##....###.........
..##....#.#.........
..##.....#..........
..##................
..##................
..##................
)
