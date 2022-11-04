
INCLUDE ../test/assert.p4

.( UM* ) test_group
0x7fffffffffffffff 0x7fffffffffffffff UM*
0x0000000000000001 0x3fffffffffffffff D= assert

0xffffffffffffffff 0xffffffffffffffff UM*
0x0000000000000001 0xfffffffffffffffe D= assert

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
