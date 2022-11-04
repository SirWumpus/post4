INCLUDE ../test/assert.p4

.( 2* ) test_group
1 2* 2 = assert
0x12345 2* 0x2468a = assert
0x3fffffffffffffff 2* 0x7ffffffffffffffe = assert
0xffffffffffffedcc 2* 0xffffffffffffdb98 = assert
test_group_end

.( 2/ ) test_group
0x1234 2/ 0x091a = assert
MAX-N 2/ 0x3fffffffffffffff = assert
0xffffffffffffedcc 2/ 0xfffffffffffff6e6 = assert
0xffffffffffffedcc 2/ 2/ 0xfffffffffffffb73 = assert
test_group_end

.( D2* ) test_group
0x8000000000000000 0x91a2 D2* 0 0x12345 D= assert
0x8000000000000000 0xffffffffffff76e6 D2* 0 0xfffffffffffeedcd D= assert
test_group_end

.( D2/ ) test_group
0 0x12345 D2/ 0x8000000000000000 0x91a2 D= assert
0 0xffffffffffffedcd D2/ 0x8000000000000000 0xfffffffffffff6e6 D= assert
0 0xffffffffffffedcd D2/ D2/ 0x4000000000000000 0xfffffffffffffb73 D= assert
test_group_end
