\
\ Wumpus
\
\ Based on the original BASIC version, see:
\ https://www.atariarchives.org/morebasicgames/showpage.php?page=178
\
\ See also https://en.wikipedia.org/wiki/Hunt_the_Wumpus
\

MARKER rm_wumpus

PAGE
.( HUNT THE WUMPUS

THE WUMPUS LIVES IN A CAVE OF 20 ROOMS.	EACH ROOM HAS 3
TUNNELS LEADING TO OTHER ROOMS.

WUMPUS
		THE WUMPUS IS NOT BOTHERED BY THE HAZARDS, HE HAS
		SUCKER FEET AND IS TOO BIG FOR A BAT TO LIFT.	USUALLY
		HE IS ASLEEP.	TWO THINGS WAKE HIM UP: YOUR ENTERING
		HIS ROOM OR YOUR SHOOTING AN ARROW.

		IF THE WUMPUS WAKES, HE MOVES ONE ROOM OR STAYS STILL.
		AFTER THAT, IF HE IS WHERE YOU ARE, HE EATS YOU UP AND
		YOU DIE.

BOTTOMLESS PITS
		TWO ROOMS HAVE BOTTOMLESS PITS IN THEM IF YOU GO THERE,
		YOU FALL INTO THE PIT AND ROT TO DEATH.

SUPER BATS
		TWO OTHER ROOMS HAVE SUPER BATS.	IF YOU GO THERE, A BAT
		GRABS YOU AND TAKES YOU TO SOME OTHER ROOM AT RANDOM.

HUNTER
		EACH TURN YOU MAY MOVE OR SHOOT A CROOKED ARROW.

		YOU CAN GO ONE ROOM OR SHOOT ONE OF 5 ARROWS.	YOU DIE
		WHEN YOU RUN OUT.	EACH ARROW CAN GO FROM 1 TO 5 ROOMS.
		YOU AIM BY TELLING THE COMPUTER THE ROOM'S YOU WANT THE
		ARROW TO GO TO.	IF THE ARROW CAN'T GO THAT WAY IT MOVES
		AT RAMDOM TO THE NEXT ROOM.	IF THE ARROW HITS THE WUMPUS,
		YOU LIVE.	IF THE ARROW HITS YOU, YOU DIE.

		AS YOU MOVE ABOUT YOU MIGHT SMELL A WUMPUS, HEAR BATS
		NEARBY, OR FEEL A DRAFT.	BE CAREFUL.


Type PLAY to start.
)

DECIMAL

VARIABLE debug
VARIABLE srand

: rand ( -- u )
	srand @ 1103515245 * 12345 + DUP srand !
	0 65536 UM/MOD NIP 0 32768 UM/MOD DROP
;

\ Original cave, vertices of a dodecahedron.
\ 1-based index, rooms 1..20, room 0 is unused.
CREATE cave-layout
	 0 ,.0 , 0 ,
	 2 ,.5 , 8 ,
	 1 ,.3 , 10 ,
	 2 ,.4 , 12 ,
	 3 ,.5 , 14 ,
	 1 ,.4 , 6 ,
	 5 ,.7 , 15 ,
	 6 ,.8 , 17 ,
	 1 ,.7 , 9 ,
	 8 , 10 , 18 ,
	 2 , 9 , 11 ,
	10 , 12 , 19 ,
	 3 , 11 , 13 ,
	12 , 14 , 20 ,
	 4 , 13 , 15 ,
	 6 , 14 , 16 ,
	15 , 17 , 20 ,
	 7 , 16 , 18 ,
	 9 , 17 , 19 ,
	11 , 18 , 20 ,
	13 , 16 , 19 ,

3 CONSTANT max_doors

: cave-room-row ( room -- aaddr ) CELLS max_doors * cave-layout + ;
: cave-room-door ( room door -- room ) CELLS SWAP cave-room-row + ;

\ Does dst-rooom follow from src-room?
: cave-src-dst? ( dst src -- bool )
	cave-room-row						\ S: dst aaddr
	max_doors 0 DO						\ S: dst aaddr
		DUP @ 							\ S: dst aaddr room
		2 PICK = IF						\ S: dst aaddr
			2DROP TRUE UNLOOP EXIT		\ S: bool
		THEN
		CELL+							\ S: dst aaddr'
	LOOP								\ S: dst aaddr'
	2DROP FALSE							\ S: bool
;

VARIABLE arrows
5 CONSTANT max_arrows

0 CONSTANT empty
1 CONSTANT hunter
2 CONSTANT wumpus
3 CONSTANT pit1
4 CONSTANT pit2
5 CONSTANT bat1
6 CONSTANT bat2

6 1+ CONSTANT max_objects

\ 0-empty 1-hunter, 2-wumpus, 3&4-pits, 5&6-bats
CREATE objects max_objects CELLS ALLOT

: location ( index -- aaddr ) CELLS objects + ;

20 1+ CONSTANT max_rooms

: pick-room ( -- room ) rand max_rooms 1- MOD 1+ ;	\ 1-based index [1, max_rooms)

: objects-overlap? ( -- bool)
	max_objects 1 DO
		max_objects 1 DO
			\ Ignore same object.
			J I <> IF
				\ Do two objects overlap?
				J location @ I location @ = IF
					UNLOOP UNLOOP FALSE EXIT
				THEN
			THEN
		LOOP
	LOOP
	\ No overlaps.
	TRUE
;

: place-objects ( -- )
	0 empty location !
	BEGIN
		max_objects 1 DO
			pick-room
			\ Assign object I to a room.
			I location !
		LOOP
		objects-overlap?
		\ Repeat until no object's loctation overlaps another.
	UNTIL
;

\ For debugging.
: list-objects ( -- )
	max_objects 1 DO
		I ." object " DUP . ." room " location @ . CR
	LOOP
;

: where-are-you ( -- )
	hunter location @
	\ What are in the nearby rooms?
	max_doors 0 DO
		DUP I cave-room-door @
		CASE
			wumpus location @ OF ." I SMELL A WUMPUS!" CR ENDOF
			pit1 location @ OF ." I FEEL A DRAFT" CR ENDOF
			pit2 location @ OF ." I FEEL A DRAFT" CR ENDOF
			bat1 location @ OF ." BATS NEARBY" CR ENDOF
			bat2 location @ OF ." BATS NEARBY" CR ENDOF
		ENDCASE
	LOOP
	\ List the nearby rooms.
	." YOU ARE IN ROOM " DUP . CR
	." TUNNELS LEAD TO "
	max_doors 0 DO
		DUP I cave-room-door @ 4 .R
	LOOP
	CR
	DROP
;

: move-valid-room? ( dst-room -- dst-room | 0 )
	hunter location @					\ S: dst hunter
	max_doors 0 DO
		DUP I cave-room-door @			\ S: dst hunter to
		2 PICK = IF						\ S: dst hunter
			UNLOOP DROP EXIT			\ S: dst
		THEN
	LOOP
	." NOT POSSIBLE" CR
	2DROP 0								\ S: 0
;

: move-wumpus ( -- )
	wumpus location @					\ S: room
	rand 4 MOD							\ S: room 0..3
	DUP 3 < IF							\ S: room 0..3
		\ Near by room.
		cave-room-door @				\ S: room'
	ELSE
		\ No room change.
		DROP							\ S: room
	THEN
	\ Update location.
	wumpus location !					\ S: --
;

: dead ( -- )
	." HA HA HA - YOU LOSE!" CR
	dropall QUIT
;

: wumpus-smoosh? ( room -- )
	wumpus location @ = IF
		." TSK TSK TSK- WUMPUS GOT YOU!" CR
		dead
	THEN
;

: road-to-hell ( -- )
	." YYYIIIIEEEE . . . FELL IN PIT" CR
	dead
;

: fell-in-pit? ( room -- )
	DUP pit1 location @ =				\ S: room bool1
	SWAP pit2 location @ =				\ S: bool1 bool2
	OR IF								\ S: --
		road-to-hell
	THEN
;

: super-bats? ( room -- room' bool )
	DUP bat1 location @ =				\ S: room bool1
	OVER bat2 location @ =				\ S: room bool1 bool2
	OR IF								\ S: room
		." ZAP--SUPER BAT SNATCH! ELSEWHEREVILLE FOR YOU!" CR
		DROP pick-room FALSE			\ S: room' FALSE
	ELSE
		\ No bats, room unchanged.
		TRUE							\ S: room TRUE
	THEN
;

: move-check-hazards ( room -- )
	BEGIN
		DUP hunter location !			\ S: room
		DUP wumpus location @ = IF
			." ...OOPS! BUMPED A WUMPUS!" CR
			move-wumpus
					THEN
		DUP wumpus-smoosh?				\ S: room
		DUP fell-in-pit?				\ S: room
		super-bats?						\ S: room' bool
	UNTIL
	DROP								\ S: --
;

: where-to ( -- )
	BEGIN
		BEGIN
			." WHERE TO? " PAD /PAD ACCEPT			\ S: length
			DROP 0 S>D PAD /PAD >NUMBER 2DROP D>S 	\ S: room
			DUP 1 max_rooms WITHIN				\ S: room bool
		UNTIL									\ S: room
		move-valid-room?						\ S: room | 0
		?DUP									\ S: room room | 0
	UNTIL										\ S: room
	move-check-hazards							\ S: --
;

CREATE shoot-path 5 CELLS ALLOT

: arrow-path ( index -- room ) CELLS shoot-path + ;

\ For debugging.
: list-arrow-path ( -- )
	5 0 DO
		I ." index " DUP . ." room " arrow-path @ . CR
	LOOP
;

: shoot-distance ( -- dist )
	0 BEGIN ( dist-wrong ) DROP
		." NO. OF ROOMS(1-5) " PAD /PAD ACCEPT 	\ S: len
		DROP 0 S>D PAD /PAD >NUMBER 2DROP D>S	\ S: dist
		DUP 1 6 WITHIN							\ S: dist bool
	UNTIL										\ S: dist
;

: shoot-room ( -- room )
	0 BEGIN ( room-wrong ) DROP
		." ROOM# " PAD /PAD ACCEPT				\ S: len
		DROP 0 S>D PAD /PAD >NUMBER 2DROP D>S	\ S: room
		DUP 1 max_rooms WITHIN					\ S: room bool
	UNTIL										\ S: room
;

: shoot-valid-path? ( room index -- bool )
	DUP 1 > IF									\ S: room index
		2 - arrow-path @ = IF					\ S: --
			." ARROWS AREN'T THAT CROOKED - TRY ANOTHER ROOM" CR
			FALSE EXIT
		THEN
		TRUE EXIT
	THEN
	2DROP TRUE
;

: hit-wumpus ( room -- )
	wumpus location @ = IF
		." AHA! YOU GOT THE WUMPUS!" CR
		debug @ IF list-arrow-path THEN
		dropall QUIT
	THEN
;

: hit-hunter ( room -- )
	hunter location @ = IF
		." OUCH! ARROW GOT YOU!" CR
		debug @ IF list-arrow-path THEN
		dead
	THEN
;

: shoot-check-path ( dist -- )
	hunter location @ SWAP				\ S: src dist
	0 DO								\ S: src
		I arrow-path @ OVER				\ S: src dst src
		cave-src-dst? 0= IF				\ S: src
			\ No tunnel for arrow, revise path.
			rand 3 MOD					\ S: src door
			cave-room-door @			\ S: src'
			I arrow-path !				\ S: --
		THEN
		\ Next room along path.
		I arrow-path @					\ S: src'
	LOOP
	DROP								\ S: --
;

: arrow-check ( -- )
	arrows @ 1- 						\ S: remain
	DUP arrows !						\ S: remain
	0= IF								\ S: --
		." NO MORE ARROWS. YOUR DOOM APPROACHES." CR
		dead
	THEN
;

: shoot-arrow ( dist -- )
	DUP shoot-check-path				\ S: dist
	\ Follow the arrow's path.
	0 DO
		I arrow-path @					\ S: room
		DUP hit-wumpus					\ S: room
		hit-hunter						\ S: --
	LOOP
	." MISSED" CR
	move-wumpus
	arrow-check
;

: shoot ( -- )
	shoot-path 5 CELLS ERASE
	shoot-distance DUP					\ S: dist dist
	0 DO
		BEGIN
			shoot-room					\ S: dist room
			DUP I arrow-path !			\ S: dist room
			I shoot-valid-path?			\ S: dist bool
		UNTIL							\ S: dist
	LOOP
	shoot-arrow							\ S: --
;

: shoot-or-move ( -- )
	BEGIN
		." (S)HOOT OR (M)OVE? " PAD /PAD ACCEPT \ S: len
		DROP PAD C@						\ S: ch
		CASE
			'M' OF where-to EXIT ENDOF
			'm' OF where-to EXIT ENDOF
			'S' OF shoot EXIT ENDOF
			's' OF shoot EXIT ENDOF
		ENDCASE
	AGAIN
;

: play ( -- )
	epoch-seconds srand !
	max_arrows arrows !
	place-objects
	BEGIN
		CR
		debug @ IF list-objects THEN
		where-are-you
		shoot-or-move
	AGAIN
;
