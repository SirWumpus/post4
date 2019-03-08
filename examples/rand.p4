\
\ Simple pseudo random number generator based example from
\ ISO C11 draft April 12, 2011.
\

MARKER rm_rand

VARIABLE srand 1 srand !

( RAND_MAX = 32767
	Iter.	srand           rand
	-------------------------------
	0	1
	1	1103527590	16838
	2	2524885223	5758
	3	662824084	10113
	4	3295386429	17515
	5	4182499122	31051
)
: rand ( -- n )
	srand @ 1103515245 * 12345 + DUP srand !
	65536 UM/MOD NIP 32768 MOD
;

\
\ Pseudo-Random Sequence Generator for 32-Bit CPUs
\ https://www.schneier.com/academic/archives/1994/09/pseudo-random_sequen.html#0065_0006
\

VARIABLE randA 1 randA !
VARIABLE randB 0 randB !
VARIABLE randC 0 randC !

: randA_shift ( -- bit )
	randA @ >R
	R@ 31 RSHIFT
	R@ 6 RSHIFT XOR
	R@ 4 RSHIFT XOR
	R@ 2 RSHIFT XOR
	R@ 1 RSHIFT XOR
	R@          XOR
	1 AND 31 LSHIFT
	R> 1 RSHIFT OR
	DUP randA !
	1 AND
;

: randB_shift ( -- bit )
	randB @ >R
	R@ 30 RSHIFT
	R@  2 RSHIFT XOR
	1 AND 30 LSHIFT
	R> 1 RSHIFT OR
	DUP randB !
	1 AND
;

: randC_shift ( -- bit )
	randC @ >R
	R@ 28 RSHIFT
	R@  1 RSHIFT XOR
	1 AND 28 LSHIFT
	R> 1 RSHIFT OR
	DUP randC !
	1 AND
;

: random_xor ( -- bit )
	randA_shift randB_shift XOR randC_shift XOR 1 AND
;

: random_maj ( -- bit )
	randA_shift randB_shift + randC_shift + 1 RSHIFT 1 AND
;

( randA=1 randB=0 randC=0

	Iter	randA		15 randomXor
	-----------------------------------
	0	1
	1	4294836224	0
	2	4026531836	0
	3	2863259647	16383
	4	1448432979	32763
	5	859090090 	19114
	6	1899390569	21813
	7	1573970540	5734
	8	3149183905	13895
	9	2659809129	1501
	10	1027030290	5870
)
: randomXor ( bits -- u )
	0 SWAP 0 DO
	  1 LSHIFT
	  random_xor OR
	LOOP
;

: randomMaj ( bits -- u )
	0 SWAP 0 DO
	  1 LSHIFT
	  random_maj OR
	LOOP
;

