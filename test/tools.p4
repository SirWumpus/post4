INCLUDE ../test/assert.p4

.( AHEAD THEN ) test_group
T{ : tw_pt1 AHEAD 1111 2222 THEN 3333 ; -> }T
T{ tw_pt1 -> 3333 }T
test_group_end

.( CS-PICK ) test_group
VARIABLE tv_pt4
: tw_repeat? 0 CS-PICK POSTPONE UNTIL ; IMMEDIATE
: <= > 0= ;
T{  : tw_pt5 ( n1 -- )
    tv_pt4 !
    BEGIN
        -1 tv_pt4 +!
        tv_pt4 @ 4 <= tw_repeat?    \ Back to BEGIN if false
        111
        tv_pt4 @ 3 <= tw_repeat?
        222
        tv_pt4 @ 2 <= tw_repeat?
        333
        tv_pt4 @ 1 =
    UNTIL
    ; ->
}T
T{ 6 tw_pt5 -> 111 111 222 111 222 333 111 222 333 }T
test_group_end

.( CS-ROLL ) test_group
T{  : tw_done? ( dest -- orig dest )		\ Same as WHILE
        POSTPONE IF 1 CS-ROLL
    ; IMMEDIATE ->
}T
T{ : tw_pt6
    >R
    BEGIN
        R@
        tw_done?
        R@
        R> 1- >R
    REPEAT
    R> DROP
   ; ->
}T
T{ 5 tw_pt6 -> 5 4 3 2 1 }T
: tw_mix_up 2 CS-ROLL ; IMMEDIATE \ cs-rot
: tw_pt7 ( f3 f2 f1 -- ? )
    IF 1111 ROT ROT                 \ C: o1         S: 1111 f3 f2
        IF 2222 SWAP                \ C: o1 o2      S: 1111 2222 f3
            IF                      \ C: o1 o2 o3
                3333 tw_mix_up      \ C: o2 o3 o1   S: 1111 2222 3333
            THEN                    \ C: o2 o3
            4444    \ Hence failure of first IF comes here and falls through
        THEN                        \ C: o2
        5555        \ Failure of 3rd IF comes here
    THEN                            \ C: --
    6666            \ Failure of 2nd IF comes here
;
T{ -1 -1 -1 tw_pt7 -> 1111 2222 3333 4444 5555 6666 }T
T{ 0 -1 -1 tw_pt7 -> 1111 2222 5555 6666 }T
T{ 0 0 -1 tw_pt7 -> 1111 0 6666 }T
T{ 0 0 0 tw_pt7 -> 0 0 4444 5555 6666 }T
: tw_1_csroll 1 CS-ROLL ; IMMEDIATE
T{  : tw_pt8
        >R
        AHEAD 111
        BEGIN 222
            tw_1_csroll
            THEN
            333
            R> 1- >R
            R@ 0<
        UNTIL
        R> DROP
    ; -> }T
T{ 1 tw_pt8 -> 333 222 333 }T
test_group_end

.( [: ;] ) test_group
T{ : tw_q1 [: 1 ;] ; tw_q1 EXECUTE -> 1 }T
T{ : tw_q2 [: [: 2 ;] ;] ; tw_q2 EXECUTE EXECUTE -> 2 }T

[DEFINED] {: [IF]
T{ : tw_q3 {: a :} [: {: a b :} b a ;] ; 1 2 3 tw_q3 EXECUTE -> 2 1 }T
T{ : tw_q4 [: DUP IF DUP 1- RECURSE THEN ;] ; 3 tw_q4 EXECUTE -> 3 2 1 0 }T
T{ : tw_q5 [: DOES> DROP 4 ;] 5 SWAP ; CREATE tw_x tw_q5 EXECUTE tw_x -> 5 4 }T

T{ : tw_q6 {: a :} [: {: a b :} b a ;] a 1+ ; 1 2 tw_q6 SWAP EXECUTE -> 3 1 }T
T{ 1 2 tw_q6 tw_q6 SWAP EXECUTE EXECUTE -> 4 1 }T
T{ 1 2 3 tw_q3 SWAP tw_q6 SWAP EXECUTE EXECUTE -> 3 1 }T
[THEN]
test_group_end

.( N>R NR> ) test_group
: tw_nr_1 N>R SWAP NR> ;
: tw_nr_2 N>R N>R SWAP NR> NR> ;
T{ 1 2 10 20 30 3 tw_nr_1 -> 2 1 10 20 30 3 }T
T{ 1 2 10 20 30 3 40 50 2 tw_nr_2 -> 2 1 10 20 30 3 40 50 2 }T
test_group_end

.( [IF] [ELSE] [THEN] ) test_group
T{ TRUE [IF] 111 [ELSE] 222 [THEN] -> 111 }T
T{ FALSE [IF] 111 [ELSE] 222 [THEN] -> 222 }T

\ Check words are immediate
: tw_tfind BL WORD FIND ;
T{ tw_tfind [IF] NIP -> 1 }T
T{ tw_tfind [ELSE] NIP -> 1 }T
T{ tw_tfind [THEN] NIP -> 1 }T
T{ : tw_pt2 [ 0 ] [IF] 1111 [ELSE] 2222 [THEN] ; tw_pt2 -> 2222 }T
T{ : tw_pt3 [ -1 ] [IF] 3333 [ELSE] 4444 [THEN] ; tw_pt3 -> 3333 }T

\ Code spread over more than 1 line
T{ TRUE [IF] 1
		2
	[ELSE]
		3
		4
	[THEN] -> 1 2 }T

T{ FALSE [IF]
		1 2
	[ELSE]
		3 4
	[THEN] -> 3 4 }T

\ Nested
: <T> TRUE ;
: <F> FALSE ;
T{ <T> [IF] 1 <T> [IF] 2 [ELSE] 3 [THEN] [ELSE] 4 [THEN] -> 1 2 }T
T{ <F> [IF] 1 <T> [IF] 2 [ELSE] 3 [THEN] [ELSE] 4 [THEN] -> 4 }T
T{ <T> [IF] 1 <F> [IF] 2 [ELSE] 3 [THEN] [ELSE] 4 [THEN] -> 1 3 }T
T{ <F> [IF] 1 <F> [IF] 2 [ELSE] 3 [THEN] [ELSE] 4 [THEN] -> 4 }T

\ Multiline with single line comment of [IF] [ELSE] [THEN] words, ie.
\ developer is futzing around with code and temorarily comments words.
T{ TRUE [IF]
    \ TRUE [IF]
        1
    \ [ELSE]
        2
    [ELSE]
        3
    \ [THEN]
        4
    [THEN] -> 1 2 }T

T{ FALSE [IF]
        1
    \ [ELSE]
        2
    [ELSE]
    \ TRUE [IF}
        3
    \ [THEN]
        4
    [THEN] -> 3 4 }T

test_group_end

.( SYNONYM ) test_group
1 VALUE tv_val
T{ SYNONYM tv_synval tv_val -> }T
T{ tv_val tv_synval -> 1 1 }T

2 TO tv_val
T{ tv_synval -> 2 }T
T{ 3 TO tv_synval -> }T
T{ tv_val tv_synval -> 3 3 }T

\ Similarly for 2VALUE and FVALUE
DEFER tw_def :NONAME 4 ; IS tw_def
T{ SYNONYM tw_syndef tw_def -> }T
T{ tw_def tw_syndef -> 4 4 }T

:NONAME 5 ; IS tw_syndef
T{ tw_def tw_syndef -> 5 5 }T

:NONAME 6 ; CONSTANT tv_defxt
T{ tv_defxt ' tw_syndef DEFER! -> }T
T{ tw_def tw_syndef -> 6 6 }T
T{ ' tw_syndef DEFER@ ' tw_def DEFER@ -> tv_defxt DUP }T
T{ ACTION-OF tw_syndef -> tv_defxt }T

CREATE tw_cre
T{ SYNONYM tw_syncre tw_cre -> }T
T{ ' tw_cre >BODY ' tw_syncre >BODY = -> TRUE }T

\ In Post4 xt of aliases and synonyms not the same.
\ CREATE tw_foo
\ SYNONYM tw_bar tw_foo
\ t{ ' tw_bar -> ' tw_foo }t
\ SYNONYM tw_baz tw_bar
\ t{ ' tw_baz -> ' tw_foo }t

CREATE tw_n>s1
SYNONYM tw_syn-n>s1 tw_n>s1
T{ S" tw_n>s1" 2DUP FIND-NAME NAME>STRING strcasecmp -> 0 }T
T{ S" tw_syn-n>s1" 2DUP FIND-NAME NAME>STRING strcasecmp -> 0 }T

\ Can't test DOES> on a synonym as there is no portable way to test ambiguous conditions`
test_group_end
