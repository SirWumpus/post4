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
T{ 0 0 D2* -> 0 S>D D2* }T
T{ MIN-INT 0 D2* -> 0 1 }T
T{ HI-2INT D2* -> MAX-2INT 1 S>D D- }T
T{ LO-2INT D2* -> MIN-2INT }T

cell-bits 64 = [IF]
t{ 0x8000000000000000 0x91a2 D2* -> 0 0x12345 }t
t{ 0x8000000000000000 0xffffffffffff76e6 D2* -> 0 0xfffffffffffeedcd }t
[THEN]
test_group_end

.( D2/ ) test_group
T{ 0 0 D2/ -> 0 0 }T
T{ 1 0 D2/ -> 0 0 }T
T{ 0 1 D2/ -> MIN-INT 0 }T
T{ MAX-2INT D2/ -> HI-2INT }T
T{ -1 S>D D2/ -> -1 S>D }T
T{ MIN-2INT D2/ -> LO-2INT }T

cell-bits 64 = [IF]
t{ 0 0x12345 D2/ -> 0x8000000000000000 0x91a2 }t
t{ 0 0xffffffffffffedcd D2/ -> 0x8000000000000000 0xfffffffffffff6e6 }t
t{ 0 0xffffffffffffedcd D2/ D2/ -> 0x4000000000000000 0xfffffffffffffb73 }t
[THEN]
test_group_end
