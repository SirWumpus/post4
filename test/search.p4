INCLUDE ../test/assert.p4

[UNDEFINED] WORDLISTS [IF]

.( Search-Order support disabled. ) CR

[ELSE]

VARIABLE wid1
VARIABLE wid2

.( FORTH-WORDLIST ) test_group
T{ FORTH-WORDLIST wid1 ! -> }T
test_group_end

.( GET-CURRENT SET-CURRENT WORDLIST ) test_group
T{ GET-CURRENT -> wid1 @ }T
T{ WORDLIST wid2 ! -> }T
T{ wid2 @ SET-CURRENT -> }T
T{ GET-CURRENT -> wid2 @ }T
T{ wid1 @ SET-CURRENT -> }T
test_group_end

.( GET-ORDER ) CR
\ The sequence of these words and the next test is specific and
\ sensitive to re-ordering.  Do not enclose in a test_group to
\ avoid rewinding HERE.
: save-orderlist ( widn ... wid1 n -- ) DUP , 0 ?DO , LOOP ;
CREATE orderlist
T{ GET-ORDER save-orderlist -> }T
CR \ orderlist 32 DUMP

: get-orderlist ( -- widn ... wid1 n )
	orderlist DUP @ CELLS		\ S: ad n
	OVER +				\ S: ad ad’
	?DO I @ -1 CELLS +LOOP
;

.( GET-ORDER SET-ORDER ) test_group
t{ orderlist @ WORDLISTS < -> TRUE }t
T{ GET-ORDER OVER -> GET-ORDER wid1 @ }T
T{ GET-ORDER SET-ORDER -> }T
T{ GET-ORDER -> get-orderlist }T
T{ get-orderlist DROP get-orderList 2* SET-ORDER -> }T
T{ GET-ORDER -> get-orderlist DROP get-orderList 2* }T
T{ get-orderlist SET-ORDER GET-ORDER -> get-orderlist }T

: so2a GET-ORDER get-orderlist SET-ORDER ;
: so2 0 SET-ORDER so2a ;
T{ so2 -> 0 }T		\ 0 SET-ORDER leaves an empty search order
: so3 -1 SET-ORDER so2a ;
: so4 ONLY so2a ;
T{ so3 -> so4 }T	\ -1 SET-ORDER is the same as ONLY
test_group_end

.( FIND-NAME-IN ) test_group
: bounds ( addr len -- addr+len addr ) OVER + SWAP ;
: >lower ( c1 -- c2 ) DUP 'A' 'Z' 1+ WITHIN BL AND OR ;
: istr= ( addr1 u1 addr2 u2 -- flag )
    ROT OVER <> IF 2DROP DROP FALSE EXIT THEN
    bounds ?DO
      DUP C@ >lower I C@ >lower <> IF
        DROP FALSE UNLOOP EXIT
      THEN
      1+
    LOOP
    DROP TRUE
;

\ Create wordlist with test words.
t{ WORDLIST CONSTANT fntwl -> }t
t{ GET-CURRENT -> FORTH-WORDLIST }t
t{ fntwl SET-CURRENT -> }t
: fnt1 25 ;
: fnt2 34 ; IMMEDIATE
\ Restore default word list.
t{ FORTH-WORDLIST SET-CURRENT -> }t

T{ S" fnt1" fntwl FIND-NAME-IN NAME>INTERPRET EXECUTE -> 25 }T
T{ : fnt3 [ S" fnt1" fntwl FIND-NAME-IN NAME>COMPILE EXECUTE ] ; fnt3 -> 25 }T
T{ S" fnt1" fntwl FIND-NAME-IN NAME>STRING S" fnt1" istr= -> TRUE }T
T{ S" fnt2" fntwl FIND-NAME-IN NAME>INTERPRET EXECUTE -> 34 }T
T{ S" fnt2" fntwl FIND-NAME-IN NAME>COMPILE EXECUTE -> 34 }T

: fnt4 fntwl FIND-NAME-IN NAME>COMPILE EXECUTE ; IMMEDIATE
T{ S" fnt2" ] fnt4 [ -> 34 }T
T{ S" fnt0" fntwl FIND-NAME-IN -> 0 }T
test_group_end

.( ONLY ) test_group
T{ ONLY FORTH GET-ORDER -> get-orderlist }T

: so1 SET-ORDER ; \ In case it is unavailable in the forth wordlist
T{ ONLY FORTH-WORDLIST 1 SET-ORDER get-orderlist so1 -> }T
T{ GET-ORDER -> get-orderlist }T
test_group_end

: alsowid2 ALSO GET-ORDER wid2 @ ROT DROP SWAP SET-ORDER ;

.( ONLY FORTH DEFINITIONS SEARCH-WORDLIST ALSO PREVIOUS ) test_group
T{ ONLY FORTH DEFINITIONS -> }T
T{ GET-CURRENT -> FORTH-WORDLIST }T

T{ GET-ORDER wid2 @ SWAP 1+ SET-ORDER DEFINITIONS GET-CURRENT -> wid2 @ }T
T{ GET-ORDER -> get-orderlist wid2 @ SWAP 1+ }T
T{ PREVIOUS GET-ORDER -> get-orderlist }T
T{ DEFINITIONS GET-CURRENT -> FORTH-WORDLIST }T

alsowid2
: w1 1234 ;
DEFINITIONS : w1 -9876 ; IMMEDIATE

ONLY FORTH
T{ w1 -> 1234 }T
DEFINITIONS
T{ w1 -> 1234 }T
alsowid2
T{ w1 -> -9876 }T
DEFINITIONS T{ w1 -> -9876 }T

ONLY FORTH DEFINITIONS
: so5 DUP IF SWAP EXECUTE THEN ;

T{ S" w1" wid1 @ SEARCH-WORDLIST so5 -> -1  1234 }T
T{ S" w1" wid2 @ SEARCH-WORDLIST so5 ->  1 -9876 }T

: c"w1" C" w1" ;
T{ alsowid2 c"w1" FIND so5 ->  1 -9876 }T
T{ PREVIOUS c"w1" FIND so5 -> -1  1234 }T
test_group_end

.( ORDER ) test_group
CR .( An unnamed wordlist at head of search order: ) CR
T{ alsowid2 DEFINITIONS ORDER -> }T
CR .( ONLY FORTH DEFINITIONS search order and compilation list: ) CR
T{ ONLY FORTH DEFINITIONS ORDER -> }T
test_group_end

.( MARKER with word lists ) test_group
VARIABLE tv_wid
VARIABLE here_before

\ Assert current state.
t{ GET-ORDER GET-CURRENT -> FORTH-WORDLIST 1 FORTH-WORDLIST }t

\ Add words and marker to current definitions.
t{ : tw_x 123 ; -> }t
t{ HERE here_before ! -> }t
t{ MARKER tw_mark -> }t
t{ : tw_y 456 ; -> }t

\ New word list and switch.  Add word.
t{ WORDLIST tv_wid ! -> }t
t{ tv_wid @ SET-CURRENT -> }t
t{ tv_wid @ FORTH-WORDLIST 2 SET-ORDER -> }t

\ Assert new state.
t{ GET-ORDER GET-CURRENT -> tv_wid @ FORTH-WORDLIST 2 tv_wid @ }t
t{ : tw_z 789 ; -> }t

\ Restore previous dictionary state.
t{ tw_mark -> }t

\ Previous state restored.
t{ GET-ORDER GET-CURRENT -> FORTH-WORDLIST 1 FORTH-WORDLIST }t
t{ HERE -> here_before @ }t

\ Words removed.
t{ ' ' CATCH tw_z -> -13 }t
t{ ' ' CATCH tw_y -> -13 }t

\ Last word remains.
t{ ' ' CATCH tw_x NIP -> 0 }t

test_group_end

[THEN]
