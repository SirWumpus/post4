INCLUDE ../test/assert.p4

.( 2* ) test_group
T{ 0    2* -> 0 }T
T{ 1    2* -> 2 }T
T{ 4000 2* -> 8000 }T
T{ 1S   2* 1 XOR -> 1S }T
T{ MSB  2* -> 0 }T
test_group_end

.( 2/ ) test_group
T{ 0        2/ -> 0 }T
T{ 1        2/ -> 0 }T
T{ 4000     2/ -> 2000 }T
T{ 1S       2/ -> 1S }T
T{ 1S 1 XOR 2/ -> 1S }T
T{ MSB      2/ MSB AND -> MSB }T
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
