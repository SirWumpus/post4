
INCLUDE ../test/assert.p4

.( UM* ) test_group
0 0 UM* 0 0 D= assert
0 1 UM* 0 0 D= assert
1 0 UM* 0 0 D= assert
1 2 UM* 2 0 D= assert
2 1 UM* 2 0 D= assert
3 3 UM* 9 0 D= assert
MAX-N 1 + 1 RSHIFT 2 UM* MAX-N 1 + 0 D= assert
MAX-N 1 + 2 UM* 0 1 D= assert
MAX-N 1 + 4 UM* 0 2 D= assert
0 INVERT 2  UM* 0 INVERT 1 LSHIFT 1 D= assert
MAX-N MAX-N UM* 1 1 INVERT 2 RSHIFT D= assert
MAX-U MAX-U UM* 1 1 INVERT D= assert

0xdeadbeefdeadbeef 0xbeefdeadbeefdead UM*
0x3a522ca1ca1e4983 0xa615999d16497cbb D= assert
test_group_end

.( M* ) test_group
\ +ve * +ve = +ve
0x7fffffffffffffff 0x7fffffffffffffff M*
0x0000000000000001 0x3fffffffffffffff D= assert

\ -ve * -ve = +ve
0x8000000000000000 0x8000000000000000 M*
0x0000000000000000 0x4000000000000000 D= assert

\ -ve * -ve = +ve
0xffffffffffffffff 0xffffffffffffffff M*
0x0000000000000001 0x0000000000000000 D= assert

\ -ve * -ve = +ve
0xdeadbeefdeadbeef 0xbeefdeadbeefdead M*
0x3a522ca1ca1e4983 0x0877fbff78abdf1f D= assert

\ -ve * -ve = +ve
0xdeadbeefcafebabe 0xbabecafebeefdead M*
0x6ea0c1026f76f666 0x0903a85214a96506 D= assert

\ -ve * +ve = -ve
0xdeadbeefdeadbeef 0x7fffffffffffffff M*
0xa152411021524111 0xef56df77ef56df77 D= assert

\ -ve * +ve = -ve
0xdeadbeefcafebabe 0x7fffffffffffffff M*
0x2152411035014542 0xef56df77e57f5d5f D= assert

\ -ve * +ve = -ve
0xdeadbeefcafebabe 0x7ee3cafebeefdead M*
0xe416c1026f76f666 0xef7bdd9e44bcc2d0 D= assert
test_group_end

.( UM/MOD ) test_group
0 0 1 UM/MOD 0 0 D= assert
1 0 1 UM/MOD 0 1 D= assert
1 0 2 UM/MOD 1 0 D= assert
3 0 2 UM/MOD 1 1 D= assert
MAX-U 2 UM* 2 UM/MOD 0 MAX-U D= assert
MAX-U 2 UM* MAX-U UM/MOD 0 2 D= assert
MAX-U MAX-U UM* MAX-U UM/MOD 0 MAX-U D= assert
test_group_end

.( SM/REM ) test_group
 15 S>D  5 SM/REM  0  3 D= assert
 10 S>D  7 SM/REM  3  1 D= assert
-10 S>D  7 SM/REM -3 -1 D= assert
 10 S>D -7 SM/REM  3 -1 D= assert
-10 S>D -7 SM/REM -3  1 D= assert
 0 S>D  1 SM/REM  0  0 D= assert
 1 S>D  1 SM/REM  0  1 D= assert
 2 S>D  1 SM/REM  0  2 D= assert
-1 S>D  1 SM/REM  0 -1 D= assert
-2 S>D  1 SM/REM  0 -2 D= assert
 0 S>D -1 SM/REM  0  0 D= assert
 1 S>D -1 SM/REM  0 -1 D= assert
 2 S>D -1 SM/REM  0 -2 D= assert
-1 S>D -1 SM/REM  0  1 D= assert
-2 S>D -1 SM/REM  0  2 D= assert
 2 S>D  2 SM/REM  0  1 D= assert
-1 S>D -1 SM/REM  0  1 D= assert
-2 S>D -2 SM/REM  0  1 D= assert
 7 S>D  3 SM/REM  1  2 D= assert
 7 S>D -3 SM/REM  1 -2 D= assert
-7 S>D  3 SM/REM -1 -2 D= assert		\ hmm
-7 S>D -3 SM/REM -1  2 D= assert
MAX-N S>D 1 SM/REM 0 MAX-N D= assert
\ MIN-N~ S>D 1 SM/REM .s 0 MIN-N~ D= assert	\ grr
MIN-N S>D 1 SM/REM 0 MIN-N D= assert
MAX-N S>D MAX-N SM/REM 0 1 D= assert
\ MIN-N~ S>D MIN-N~ SM/REM .s 0 1 D= assert	\ grr
MIN-N S>D MIN-N SM/REM 0 1 D= assert
MAX-U 1 4 SM/REM 3 MAX-N D= assert
2 MIN-N~ M* 2 SM/REM 0 MIN-N~ D= assert
2 MIN-N~ M* MIN-N~ SM/REM 0 2 D= assert
2 MAX-N M* 2 SM/REM 0 MAX-N D= assert
2 MAX-N M* MAX-N SM/REM 0 2 D= assert
MIN-N~ MIN-N~ M* MIN-N~ SM/REM 0 MIN-N~ D= assert
MIN-N~ MAX-N M* MIN-N~ SM/REM 0 MAX-N D= assert
MIN-N~ MAX-N M* MAX-N SM/REM 0 MIN-N~ D= assert
MAX-N MAX-N M* MAX-N SM/REM 0 MAX-N D= assert
test_group_end

.( FM/MOD ) test_group
 15 S>D  5 FM/MOD  0  3 D= assert
 10 S>D  7 FM/MOD  3  1 D= assert
-10 S>D  7 FM/MOD  4 -2 D= assert
 10 S>D -7 FM/MOD -4 -2 D= assert
-10 S>D -7 FM/MOD -3  1 D= assert
 0 S>D  1 FM/MOD  0  0 D= assert
 1 S>D  1 FM/MOD  0  1 D= assert
 2 S>D  1 FM/MOD  0  2 D= assert
-1 S>D  1 FM/MOD  0 -1 D= assert
-2 S>D  1 FM/MOD  0 -2 D= assert
 0 S>D -1 FM/MOD  0  0 D= assert
 1 S>D -1 FM/MOD  0 -1 D= assert
 2 S>D -1 FM/MOD  0 -2 D= assert
-1 S>D -1 FM/MOD  0  1 D= assert
-2 S>D -1 FM/MOD  0  2 D= assert
 2 S>D  2 FM/MOD  0  1 D= assert
-1 S>D -1 FM/MOD  0  1 D= assert
-2 S>D -2 FM/MOD  0  1 D= assert
 7 S>D  3 FM/MOD  1  2 D= assert
 7 S>D -3 FM/MOD -2 -3 D= assert
-7 S>D  3 FM/MOD  2 -3 D= assert
-7 S>D -3 FM/MOD -1  2 D= assert
MAX-N S>D 1 FM/MOD 0 MAX-N D= assert
\ MIN-N~ S>D 1 FM/MOD 0 MIN-N~ D= assert		\ grr
MIN-N S>D 1 FM/MOD 0 MIN-N D= assert
MAX-N S>D MAX-N FM/MOD 0 1 D= assert
\ MIN-N~ S>D MIN-N~ FM/MOD 0 1 D= assert		\ grr
MIN-N S>D MIN-N FM/MOD 0 1 D= assert
MAX-U 1 4 FM/MOD 3 MAX-N D= assert
1 MIN-N~ M* 1 FM/MOD 0 MIN-N~ D= assert
\ 1 MIN-N~ M* MIN-N~ FM/MOD 0 1 D= assert		\ grr
1 MIN-N M* MIN-N FM/MOD 0 1 D= assert
2 MIN-N~ M* 2 FM/MOD 0 MIN-N~ D= assert
2 MIN-N~ M* MIN-N~ FM/MOD 0 2 D= assert
1 MAX-N M* 1 FM/MOD 0 MAX-N D= assert
1 MAX-N M* MAX-N FM/MOD 0 1 D= assert
2 MAX-N M* 2 FM/MOD 0 MAX-N D= assert
2 MAX-N M* MAX-N FM/MOD 0 2 D= assert
MIN-N~ MIN-N~ M* MIN-N~ FM/MOD 0 MIN-N~ D= assert
MIN-N~ MAX-N M* MIN-N~ FM/MOD 0 MAX-N D= assert
MIN-N~ MAX-N M* MAX-N FM/MOD 0 MIN-N~ D= assert
MAX-N MAX-N M* MAX-N FM/MOD 0 MAX-N D= assert

test_group_end

: test/mod >R S>D R> floored IF FM/MOD EXIT THEN SM/REM ;

.( /MOD ) test_group
 0  1 /MOD  0  1 test/mod D= assert
 1  1 /MOD  1  1 test/mod D= assert
 2  1 /MOD  2  1 test/mod D= assert
-1  1 /MOD -1  1 test/mod D= assert
-2  1 /MOD -2  1 test/mod D= assert
 0 -1 /MOD  0 -1 test/mod D= assert
 1 -1 /MOD  1 -1 test/mod D= assert
 2 -1 /MOD  2 -1 test/mod D= assert
-1 -1 /MOD -1 -1 test/mod D= assert
-2 -1 /MOD -2 -1 test/mod D= assert
 2  2 /MOD  2  2 test/mod D= assert
-1 -1 /MOD -1 -1 test/mod D= assert
-2 -2 /MOD -2 -2 test/mod D= assert
test_group_end

: test/ test/mod SWAP DROP ;

.( / ) test_group
 0  1 /  0  1 test/ = assert
 1  1 /  1  1 test/ = assert
 2  1 /  2  1 test/ = assert
-1  1 / -1  1 test/ = assert
-2  1 / -2  1 test/ = assert
 0 -1 /  0 -1 test/ = assert
 1 -1 /  1 -1 test/ = assert
 2 -1 /  2 -1 test/ = assert
-1 -1 / -1 -1 test/ = assert
-2 -1 / -2 -1 test/ = assert
 2  2 /  2  2 test/ = assert
-1 -1 / -1 -1 test/ = assert
-2 -2 / -2 -2 test/ = assert
 7  3 /  7  3 test/ = assert
 7 -3 /  7 -3 test/ = assert
-7  3 / -7  3 test/ = assert
-7 -3 / -7 -3 test/ = assert
MAX-N 1 / MAX-N 1 test/ = assert
MIN-N 1 / MIN-N 1 test/ = assert
MAX-N MAX-N / MAX-N MAX-N test/ = assert
MIN-N MIN-N / MIN-N MIN-N test/ = assert
test_group_end

: testmod test/mod DROP ;

.( MOD ) test_group
 0  1 MOD  0  1 testmod = assert
 1  1 MOD  1  1 testmod = assert
 2  1 MOD  2  1 testmod = assert
-1  1 MOD -1  1 testmod = assert
-2  1 MOD -2  1 testmod = assert
 0 -1 MOD  0 -1 testmod = assert
 1 -1 MOD  1 -1 testmod = assert
 2 -1 MOD  2 -1 testmod = assert
-1 -1 MOD -1 -1 testmod = assert
-2 -1 MOD -2 -1 testmod = assert
 2  2 MOD  2  2 testmod = assert
-1 -1 MOD -1 -1 testmod = assert
-2 -2 MOD -2 -2 testmod = assert
 7  3 MOD  7  3 testmod = assert
 7 -3 MOD  7 -3 testmod = assert
-7  3 MOD -7  3 testmod = assert
-7 -3 MOD -7 -3 testmod = assert
MAX-N 1 MOD MAX-N 1 testmod = assert
MIN-N 1 MOD MIN-N 1 testmod = assert
MAX-N MAX-N MOD MAX-N MAX-N testmod = assert
MIN-N MIN-N MOD MIN-N MIN-N testmod = assert
test_group_end

: test*/mod >R M* R> floored IF FM/MOD EXIT THEN SM/REM ;

.( */MOD ) test_group
 0  2  1 */MOD  0  2  1 test*/mod D= assert
 1  2  1 */MOD  1  2  1 test*/mod D= assert
 2  2  1 */MOD  2  2  1 test*/mod D= assert
-1  2  1 */MOD -1  2  1 test*/mod D= assert
-2  2  1 */MOD -2  2  1 test*/mod D= assert
 0  2 -1 */MOD  0  2 -1 test*/mod D= assert
 1  2 -1 */MOD  1  2 -1 test*/mod D= assert
 2  2 -1 */MOD  2  2 -1 test*/mod D= assert
-1  2 -1 */MOD -1  2 -1 test*/mod D= assert
-2  2 -1 */MOD -2  2 -1 test*/mod D= assert
 2  2  2 */MOD  2  2  2 test*/mod D= assert
-1  2 -1 */MOD -1  2 -1 test*/mod D= assert
-2  2 -2 */MOD -2  2 -2 test*/mod D= assert
 7  2  3 */MOD  7  2  3 test*/mod D= assert
 7  2 -3 */MOD  7  2 -3 test*/mod D= assert
-7  2  3 */MOD -7  2  3 test*/mod D= assert
-7  2 -3 */MOD -7  2 -3 test*/mod D= assert
MAX-N 2 MAX-N */MOD MAX-N 2 MAX-N test*/mod D= assert
MIN-N 2 MIN-N */MOD MIN-N 2 MIN-N test*/mod D= assert
test_group_end

: test*/ test*/mod SWAP DROP ;

.( */ ) test_group
 0  2  1 */  0  2  1 test*/ = assert
 1  2  1 */  1  2  1 test*/ = assert
 2  2  1 */  2  2  1 test*/ = assert
-1  2  1 */ -1  2  1 test*/ = assert
-2  2  1 */ -2  2  1 test*/ = assert
 0  2 -1 */  0  2 -1 test*/ = assert
 1  2 -1 */  1  2 -1 test*/ = assert
 2  2 -1 */  2  2 -1 test*/ = assert
-1  2 -1 */ -1  2 -1 test*/ = assert
-2  2 -1 */ -2  2 -1 test*/ = assert
 2  2  2 */  2  2  2 test*/ = assert
-1  2 -1 */ -1  2 -1 test*/ = assert
-2  2 -2 */ -2  2 -2 test*/ = assert
 7  2  3 */  7  2  3 test*/ = assert
 7  2 -3 */  7  2 -3 test*/ = assert
-7  2  3 */ -7  2  3 test*/ = assert
-7  2 -3 */ -7  2 -3 test*/ = assert
MAX-N 2 MAX-N */ MAX-N 2 MAX-N test*/ = assert
MIN-N 2 MIN-N */ MIN-N 2 MIN-N test*/ = assert
test_group_end
