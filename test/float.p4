INCLUDE ../test/assert.p4

.( F0= ) test_group
 0.0 F0= assert
 1.0 F0= assert_not
-1.0 F0= assert_not
test_group_end

.( F0< ) test_group
 0.0 F0< assert_not
 1.0 F0< assert_not
-1.0 F0< assert
test_group_end

.( F= ) test_group
 0.0  0.0 F= assert
 1.0  1.0 F= assert
-1.0 -1.0 F= assert
 1.0 -1.0 F= assert_not
-1.0  1.0 F= assert_not
test_group_end

.( F< ) test_group
 0.0  0.0 F< assert_not
 1.0  2.0 F< assert
-1.0  2.0 F< assert
 2.0  1.0 F< assert_not
 2.0 -1.0 F< assert_not
-2.0 -1.0 F< assert
test_group_end

.( FDROP ) test_group
1.0 FDROP FDEPTH 0 = assert
999.0 1.0 2.0 FDEPTH 3 = assert FDROP FDROP FDEPTH 1 = assert 999.0 F= assert
test_group_end

.( FDUP ) test_group
0.0 FDUP F- F0= assert
123.45 FDUP F- F0= assert
test_group_end

.( FSWAP ) test_group
0.0 1.0 FSWAP  F0= assert 1.0 F= assert
4567.89 3.14159 FSWAP F- -4564.74841 F= assert
test_group_end

.( FOVER ) test_group
1.0 2.0 FOVER 1.0 F= assert 2.0 F= assert 1.0 F= assert
test_group_end

.( FROT ) test_group
999.0 1.0 2.0 3.0 FROT 1.0 F= assert 3.0 F= assert 2.0 F= assert 999.0 F= assert
test_group_end

.( FNEGATE ) test_group
 0.0 FNEGATE  0.0 F= assert
 1.0 FNEGATE -1.0 F= assert
-1.0 FNEGATE  1.0 F= assert
 2.0 FNEGATE -2.0 F= assert
-2.0 FNEGATE  2.0 F= assert
test_group_end

.( FNEGATE ) test_group
 0.0 FABS 0.0 F= assert
 1.0 FABS 1.0 F= assert
-1.0 FABS 1.0 F= assert
test_group_end

.( FDEPTH ) test_group
FDEPTH 0 = assert
1.0 FDEPTH 1 = assert FDROP
1.0 2.0 FDEPTH 2 = assert FDROP FDROP
test_group_end

.( FVALUE TO ) test_group
T{ 123e0 FVALUE tv_fval -> }T
tv_fval 123.0 F= assert
T{ 234e0 TO tv_fval -> }T
tv_fval 234e0 F= assert
T{ : tw_set_fval tv_fval FSWAP TO tv_fval ; -> }T
345e0 tw_set_fval tv_fval 345e0 F= assert 234e0 F= assert
T{ 5e0 TO tv_fval -> }T
T{ : [execute] EXECUTE ; IMMEDIATE -> }T
' tv_fval ] [execute] [ 5e0 F= assert
test_group_end
