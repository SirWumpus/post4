
INCLUDE ../test/assert.p4

.( UM* ) test_group
T{ 0 0 UM* -> 0 0 }T
T{ 0 1 UM* -> 0 0 }T
T{ 1 0 UM* -> 0 0 }T
T{ 1 2 UM* -> 2 0 }T
T{ 2 1 UM* -> 2 0 }T
T{ 3 3 UM* -> 9 0 }T
T{ MAX-N 1 + 1 RSHIFT 2 UM* -> MAX-N 1 + 0 }T
T{ MAX-N 1 + 2 UM* -> 0 1 }T
T{ MAX-N 1 + 4 UM* -> 0 2 }T
T{ 0 INVERT 2  UM* -> 0 INVERT 1 LSHIFT 1 }T
T{ MAX-N MAX-N UM* -> 1 1 INVERT 2 RSHIFT }T
T{ MAX-U MAX-U UM* -> 1 1 INVERT }T

cell-bits 64 = [IF]
T{ $deadbeefdeadbeef $beefdeadbeefdead UM*
-> $3a522ca1ca1e4983 $a615999d16497cbb }T
[THEN]
test_group_end

.( M* ) test_group
cell-bits 64 = [IF]
\ +ve * +ve = +ve
t{ $7fffffffffffffff $7fffffffffffffff M*
-> $0000000000000001 $3fffffffffffffff }t

\ -ve * -ve = +ve
t{ $8000000000000000 $8000000000000000 M*
-> $0000000000000000 $4000000000000000 }t

\ -ve * -ve = +ve
t{ $ffffffffffffffff $ffffffffffffffff M*
-> $0000000000000001 $0000000000000000 }t

\ -ve * -ve = +ve
t{ $deadbeefdeadbeef $beefdeadbeefdead M*
-> $3a522ca1ca1e4983 $0877fbff78abdf1f }t

\ -ve * -ve = +ve
t{ $deadbeefcafebabe $babecafebeefdead M*
-> $6ea0c1026f76f666 $0903a85214a96506 }t

\ -ve * +ve = -ve
t{ $deadbeefdeadbeef $7fffffffffffffff M*
-> $a152411021524111 $ef56df77ef56df77 }t

\ -ve * +ve = -ve
t{ $deadbeefcafebabe $7fffffffffffffff M*
-> $2152411035014542 $ef56df77e57f5d5f }t

\ -ve * +ve = -ve
t{ $deadbeefcafebabe $7ee3cafebeefdead M*
-> $e416c1026f76f666 $ef7bdd9e44bcc2d0 }t
[THEN]

T{  0  0 M* ->  0 S>D }T
T{  0  1 M* ->  0 S>D }T
T{  1  0 M* ->  0 S>D }T
T{  1  2 M* ->  2 S>D }T
T{  2  1 M* ->  2 S>D }T
T{  3  3 M* ->  9 S>D }T
T{ -3  3 M* -> -9 S>D }T
T{  3 -3 M* -> -9 S>D }T
T{ -3 -3 M* ->  9 S>D }T
T{ 0 MIN-INT M* -> 0 S>D }T
T{ 1 MIN-INT M* -> MIN-INT S>D }T
T{ 2 MIN-INT M* -> 0 1S }T
T{ 0 MAX-INT M* -> 0 S>D }T
T{ 1 MAX-INT M* -> MAX-INT S>D }T
T{ 2 MAX-INT M* -> MAX-INT 1 LSHIFT 0 }T
T{ MIN-INT MIN-INT M* -> 0 MSB 1 RSHIFT }T
T{ MAX-INT MIN-INT M* -> MSB MSB 2/ }T
T{ MAX-INT MAX-INT M* -> 1 MSB 2/ INVERT }T
test_group_end

.( UM/MOD ) test_group
t{ 0 0 1 UM/MOD -> 0 0 }t
t{ 1 0 1 UM/MOD -> 0 1 }t
t{ 1 0 2 UM/MOD -> 1 0 }t
t{ 3 0 2 UM/MOD -> 1 1 }t
t{ MAX-U 2 UM* 2 UM/MOD -> 0 MAX-U }t
t{ MAX-U 2 UM* MAX-U UM/MOD -> 0 2 }t
t{ MAX-U MAX-U UM* MAX-U UM/MOD -> 0 MAX-U }t
test_group_end

.( SM/REM ) test_group
T{  15 S>D  5 SM/REM ->  0  3 }T
T{  10 S>D  7 SM/REM ->  3  1 }T
T{ -10 S>D  7 SM/REM -> -3 -1 }T
T{  10 S>D -7 SM/REM ->  3 -1 }T
T{ -10 S>D -7 SM/REM -> -3  1 }T
T{  0 S>D  1 SM/REM ->  0  0 }T
T{  1 S>D  1 SM/REM ->  0  1 }T
T{  2 S>D  1 SM/REM ->  0  2 }T
T{ -1 S>D  1 SM/REM ->  0 -1 }T
T{ -2 S>D  1 SM/REM ->  0 -2 }T
T{  0 S>D -1 SM/REM ->  0  0 }T
T{  1 S>D -1 SM/REM ->  0 -1 }T
T{  2 S>D -1 SM/REM ->  0 -2 }T
T{ -1 S>D -1 SM/REM ->  0  1 }T
T{ -2 S>D -1 SM/REM ->  0  2 }T
T{  2 S>D  2 SM/REM ->  0  1 }T
T{ -1 S>D -1 SM/REM ->  0  1 }T
T{ -2 S>D -2 SM/REM ->  0  1 }T
T{  7 S>D  3 SM/REM ->  1  2 }T
T{  7 S>D -3 SM/REM ->  1 -2 }T
T{ -7 S>D  3 SM/REM -> -1 -2 }T		\ hmm
T{ -7 S>D -3 SM/REM -> -1  2 }T
T{ MAX-INT S>D 1 SM/REM -> 0 MAX-INT }T
T{ MIN-INT S>D 1 SM/REM -> 0 MIN-INT }T
T{ MAX-INT S>D MAX-INT SM/REM -> 0 1 }T
T{ MIN-INT S>D MIN-INT SM/REM -> 0 1 }T
T{ 1S 1 4 SM/REM -> 3 MAX-INT }T
T{ 2 MIN-INT M* 2 SM/REM -> 0 MIN-INT }T
T{ 2 MIN-INT M* MIN-INT SM/REM -> 0 2 }T
T{ 2 MAX-INT M* 2 SM/REM -> 0 MAX-INT }T
T{ 2 MAX-INT M* MAX-INT SM/REM -> 0 2 }T
T{ MIN-INT MIN-INT M* MIN-INT SM/REM -> 0 MIN-INT }T
T{ MIN-INT MAX-INT M* MIN-INT SM/REM -> 0 MAX-INT }T
T{ MIN-INT MAX-INT M* MAX-INT SM/REM -> 0 MIN-INT }T
T{ MAX-INT MAX-INT M* MAX-INT SM/REM -> 0 MAX-INT }T
test_group_end

.( FM/MOD ) test_group
T{  15 S>D  5 FM/MOD ->  0  3 }T
T{  10 S>D  7 FM/MOD ->  3  1 }T
T{ -10 S>D  7 FM/MOD ->  4 -2 }T
T{  10 S>D -7 FM/MOD -> -4 -2 }T
T{ -10 S>D -7 FM/MOD -> -3  1 }T
T{  0 S>D  1 FM/MOD ->  0  0 }T
T{  1 S>D  1 FM/MOD ->  0  1 }T
T{  2 S>D  1 FM/MOD ->  0  2 }T
T{ -1 S>D  1 FM/MOD ->  0 -1 }T
T{ -2 S>D  1 FM/MOD ->  0 -2 }T
T{  0 S>D -1 FM/MOD ->  0  0 }T
T{  1 S>D -1 FM/MOD ->  0 -1 }T
T{  2 S>D -1 FM/MOD ->  0 -2 }T
T{ -1 S>D -1 FM/MOD ->  0  1 }T
T{ -2 S>D -1 FM/MOD ->  0  2 }T
T{  2 S>D  2 FM/MOD ->  0  1 }T
T{ -1 S>D -1 FM/MOD ->  0  1 }T
T{ -2 S>D -2 FM/MOD ->  0  1 }T
T{  7 S>D  3 FM/MOD ->  1  2 }T
T{  7 S>D -3 FM/MOD -> -2 -3 }T
T{ -7 S>D  3 FM/MOD ->  2 -3 }T
T{ -7 S>D -3 FM/MOD -> -1  2 }T
T{ MAX-INT S>D             1 FM/MOD -> 0 MAX-INT }T
T{ MIN-INT S>D             1 FM/MOD -> 0 MIN-INT }T
T{ MAX-INT S>D        MAX-INT FM/MOD -> 0 1 }T
T{ MIN-INT S>D        MIN-INT FM/MOD -> 0 1 }T
T{    1S 1                  4 FM/MOD -> 3 MAX-INT }T
T{       1 MIN-INT M*       1 FM/MOD -> 0 MIN-INT }T
T{       1 MIN-INT M* MIN-INT FM/MOD -> 0 1 }T
T{       2 MIN-INT M*       2 FM/MOD -> 0 MIN-INT }T
T{       2 MIN-INT M* MIN-INT FM/MOD -> 0 2 }T
T{       1 MAX-INT M*       1 FM/MOD -> 0 MAX-INT }T
T{       1 MAX-INT M* MAX-INT FM/MOD -> 0 1 }T
T{       2 MAX-INT M*       2 FM/MOD -> 0 MAX-INT }T
T{       2 MAX-INT M* MAX-INT FM/MOD -> 0 2 }T
T{ MIN-INT MIN-INT M* MIN-INT FM/MOD -> 0 MIN-INT }T
T{ MIN-INT MAX-INT M* MIN-INT FM/MOD -> 0 MAX-INT }T
T{ MIN-INT MAX-INT M* MAX-INT FM/MOD -> 0 MIN-INT }T
T{ MAX-INT MAX-INT M* MAX-INT FM/MOD -> 0 MAX-INT }T
test_group_end

test_group
: test/mod >R S>D R> floored IF FM/MOD EXIT THEN SM/REM ;

	.( /MOD ) test_group
t{  0  1 /MOD ->  0  1 test/mod }t
t{  1  1 /MOD ->  1  1 test/mod }t
t{  2  1 /MOD ->  2  1 test/mod }t
t{ -1  1 /MOD -> -1  1 test/mod }t
t{ -2  1 /MOD -> -2  1 test/mod }t
t{  0 -1 /MOD ->  0 -1 test/mod }t
t{  1 -1 /MOD ->  1 -1 test/mod }t
t{  2 -1 /MOD ->  2 -1 test/mod }t
t{ -1 -1 /MOD -> -1 -1 test/mod }t
t{ -2 -1 /MOD -> -2 -1 test/mod }t
t{  2  2 /MOD ->  2  2 test/mod }t
t{ -1 -1 /MOD -> -1 -1 test/mod }t
t{ -2 -2 /MOD -> -2 -2 test/mod }t
test_group_end

: test/ test/mod SWAP DROP ;

	.( / ) test_group
t{  0  1 / ->  0  1 test/ }t
t{  1  1 / ->  1  1 test/ }t
t{  2  1 / ->  2  1 test/ }t
t{ -1  1 / -> -1  1 test/ }t
t{ -2  1 / -> -2  1 test/ }t
t{  0 -1 / ->  0 -1 test/ }t
t{  1 -1 / ->  1 -1 test/ }t
t{  2 -1 / ->  2 -1 test/ }t
t{ -1 -1 / -> -1 -1 test/ }t
t{ -2 -1 / -> -2 -1 test/ }t
t{  2  2 / ->  2  2 test/ }t
t{ -1 -1 / -> -1 -1 test/ }t
t{ -2 -2 / -> -2 -2 test/ }t
t{  7  3 / ->  7  3 test/ }t
t{  7 -3 / ->  7 -3 test/ }t
t{ -7  3 / -> -7  3 test/ }t
t{ -7 -3 / -> -7 -3 test/ }t
t{ MAX-N 1 / -> MAX-N 1 test/ }t
t{ MIN-N 1 / -> MIN-N 1 test/ }t
t{ MAX-N MAX-N / -> MAX-N MAX-N test/ }t
t{ MIN-N MIN-N / -> MIN-N MIN-N test/ }t
	test_group_end

: testmod test/mod DROP ;

	.( MOD ) test_group
t{  0  1 MOD ->  0  1 testmod }t
t{  1  1 MOD ->  1  1 testmod }t
t{  2  1 MOD ->  2  1 testmod }t
t{ -1  1 MOD -> -1  1 testmod }t
t{ -2  1 MOD -> -2  1 testmod }t
t{  0 -1 MOD ->  0 -1 testmod }t
t{  1 -1 MOD ->  1 -1 testmod }t
t{  2 -1 MOD ->  2 -1 testmod }t
t{ -1 -1 MOD -> -1 -1 testmod }t
t{ -2 -1 MOD -> -2 -1 testmod }t
t{  2  2 MOD ->  2  2 testmod }t
t{ -1 -1 MOD -> -1 -1 testmod }t
t{ -2 -2 MOD -> -2 -2 testmod }t
t{  7  3 MOD ->  7  3 testmod }t
t{  7 -3 MOD ->  7 -3 testmod }t
t{ -7  3 MOD -> -7  3 testmod }t
t{ -7 -3 MOD -> -7 -3 testmod }t
t{ MAX-N 1 MOD -> MAX-N 1 testmod }t
t{ MIN-N 1 MOD -> MIN-N 1 testmod }t
t{ MAX-N MAX-N MOD -> MAX-N MAX-N testmod }t
t{ MIN-N MIN-N MOD -> MIN-N MIN-N testmod }t
	test_group_end

: test*/mod >R M* R> floored IF FM/MOD EXIT THEN SM/REM ;

	.( */MOD ) test_group
t{  0  2  1 */MOD ->  0  2  1 test*/mod }t
t{  1  2  1 */MOD ->  1  2  1 test*/mod }t
t{  2  2  1 */MOD ->  2  2  1 test*/mod }t
t{ -1  2  1 */MOD -> -1  2  1 test*/mod }t
t{ -2  2  1 */MOD -> -2  2  1 test*/mod }t
t{  0  2 -1 */MOD ->  0  2 -1 test*/mod }t
t{  1  2 -1 */MOD ->  1  2 -1 test*/mod }t
t{  2  2 -1 */MOD ->  2  2 -1 test*/mod }t
t{ -1  2 -1 */MOD -> -1  2 -1 test*/mod }t
t{ -2  2 -1 */MOD -> -2  2 -1 test*/mod }t
t{  2  2  2 */MOD ->  2  2  2 test*/mod }t
t{ -1  2 -1 */MOD -> -1  2 -1 test*/mod }t
t{ -2  2 -2 */MOD -> -2  2 -2 test*/mod }t
t{  7  2  3 */MOD ->  7  2  3 test*/mod }t
t{  7  2 -3 */MOD ->  7  2 -3 test*/mod }t
t{ -7  2  3 */MOD -> -7  2  3 test*/mod }t
t{ -7  2 -3 */MOD -> -7  2 -3 test*/mod }t
t{ MAX-N 2 MAX-N */MOD -> MAX-N 2 MAX-N test*/mod }t
t{ MIN-N 2 MIN-N */MOD -> MIN-N 2 MIN-N test*/mod }t
	test_group_end

: test*/ test*/mod SWAP DROP ;

	.( */ ) test_group
t{  0  2  1 */ ->  0  2  1 test*/ }t
t{  1  2  1 */ ->  1  2  1 test*/ }t
t{  2  2  1 */ ->  2  2  1 test*/ }t
t{ -1  2  1 */ -> -1  2  1 test*/ }t
t{ -2  2  1 */ -> -2  2  1 test*/ }t
t{  0  2 -1 */ ->  0  2 -1 test*/ }t
t{  1  2 -1 */ ->  1  2 -1 test*/ }t
t{  2  2 -1 */ ->  2  2 -1 test*/ }t
t{ -1  2 -1 */ -> -1  2 -1 test*/ }t
t{ -2  2 -1 */ -> -2  2 -1 test*/ }t
t{  2  2  2 */ ->  2  2  2 test*/ }t
t{ -1  2 -1 */ -> -1  2 -1 test*/ }t
t{ -2  2 -2 */ -> -2  2 -2 test*/ }t
t{  7  2  3 */ ->  7  2  3 test*/ }t
t{  7  2 -3 */ ->  7  2 -3 test*/ }t
t{ -7  2  3 */ -> -7  2  3 test*/ }t
t{ -7  2 -3 */ -> -7  2 -3 test*/ }t
t{ MAX-N 2 MAX-N */ -> MAX-N 2 MAX-N test*/ }t
t{ MIN-N 2 MIN-N */ -> MIN-N 2 MIN-N test*/ }t
	test_group_end
test_group_end


.( M*/ ) test_group
\ To correct the result if the division is floored, only used when
\ necessary, i.e., negative quotient and remainder != 0.
: tw_is_floored ( d -- d' )
	[ -3 2 / -2 = ] LITERAL IF 1 S>D D- THEN
;
T{    5 S>D  7 11 M*/ ->  3 S>D }T
T{    5 S>D -7 11 M*/ -> -3 S>D tw_is_floored }T
T{   -5 S>D  7 11 M*/ -> -3 S>D tw_is_floored }T
T{   -5 S>D -7 11 M*/ ->  3 S>D }T
T{ MAX-2INT  8 16 M*/ -> HI-2INT }T
T{ MAX-2INT -8 16 M*/ -> HI-2INT DNEGATE tw_is_floored }T
T{ MIN-2INT  8 16 M*/ -> LO-2INT }T
T{ MIN-2INT -8 16 M*/ -> LO-2INT DNEGATE }T
T{ MAX-2INT MAX-INT MAX-INT M*/ -> MAX-2INT }T
T{ MAX-2INT MAX-INT 2/ MAX-INT M*/ -> MAX-INT 1- HI-2INT NIP }T
T{ MIN-2INT LO-2INT NIP DUP NEGATE M*/ -> MIN-2INT }T
T{ MIN-2INT LO-2INT NIP 1- MAX-INT M*/ -> MIN-INT 3 + HI-2INT NIP 2 + }T
T{ MAX-2INT LO-2INT NIP DUP NEGATE M*/ -> MAX-2INT DNEGATE }T
T{ MIN-2INT MAX-INT DUP M*/ -> MIN-2INT }T

\ comp.lang.forth 2024-07-10 Overflow Test for M*/
\ krishna.myneni@ccreweb.org
cell-bits 64 = [IF]
: ipow ( n u -- d ) 1 S>D ROT 0 ?DO 2 PICK 1 M*/ LOOP ROT DROP ;
\ Result ok, output 117809547936177440979200839996337890625
t{ 65 21 ipow CR 2DUP d. -> $ec883376b5d52541 $58a14b2a0eca886e }t
\ Result overflow, output -1.
t{ 65 22 ipow CR 2DUP d. -> -1 -1 }t
[THEN]
test_group_end
