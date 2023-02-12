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

.( FTRUNC ) test_group
T{ -0E0         FTRUNC F0= -> TRUE }T
T{ -1E-9        FTRUNC F0= -> TRUE }T
T{ -0.9E0       FTRUNC F0= -> TRUE }T
T{ -1E0 1E-5 F+ FTRUNC F0= -> TRUE }T
   0E0          FTRUNC 0E0 F= assert
   1E-9         FTRUNC 0E0 F= assert
  -1E0 -1E-5 F+ FTRUNC -1E0 F= assert
   3.14E0       FTRUNC 3E0 F= assert
   3.99E0       FTRUNC 3E0 F= assert
   4E0          FTRUNC 4E0 F= assert
  -4E0          FTRUNC -4E0 F= assert
  -4.1E0        FTRUNC -4E0 F= assert
test_group_end

.( FATAN2 ) test_group
.fs
21 SET-PRECISION

3.141592653589793238463 FCONSTANT pi
pi FNEGATE FCONSTANT -pi

 pi   4e0 F/ FCONSTANT   pi/4
-pi   4e0 F/ FCONSTANT  -pi/4
 pi   2e0 F/ FCONSTANT   pi/2
-pi   2e0 F/ FCONSTANT  -pi/2
 pi/2 3e0 F* FCONSTANT  3pi/2
 pi/4 3e0 F* FCONSTANT  3pi/4
-pi/4 3e0 F* FCONSTANT -3pi/4

\ y    x           rad            deg
 0e0  1e0 FATAN2   0e0  F= assert \ 0
 1e0  1e0 FATAN2   pi/4 F= assert \ 45
 1e0  0e0 FATAN2   pi/2 F= assert \ 90
-1e0 -1e0 FATAN2 -3pi/4 F= assert \ 135
 0e0 -1e0 FATAN2   pi   F= assert \ 180
-1e0  1e0 FATAN2  -pi/4 F= assert \ 225
-1e0  0e0 FATAN2  -pi/2 F= assert \ 270
-1e0  1e0 FATAN2  -pi/4 F= assert \ 315

\ If y is +/-0 and x is < 0, +/-pi shall be returned.
 0e0 -1e0 FATAN2   pi   F= assert
-0e0 -1e0 FATAN2  -pi   F= assert

\ If y is +/-0 and x is > 0, +/-0 shall be0 returned.
 0e0  1e0 FATAN2   0e0  F= assert
-0e0  1e0 FATAN2  -0e0  F= assert

\ If y is < 0 and x is +/-0, -pi/2 shall be0 returned.
-1e0  0e0 FATAN2  -pi/2 F= assert
-1e0 -0e0 FATAN2  -pi/2 F= assert

\ If y is > 0 and x is +/-0, pi/2 shall be0 returned.
 1e0  0e0 FATAN2   pi/2 F= assert
 1e0 -0e0 FATAN2   pi/2 F= assert

\ Optional ISO C / single UNIX specs:
\ If either x or y is NaN, a NaN shall be returned.
 0e0  0e0 F/ FCONSTANT  NaN
 1e0  0e0 F/ FCONSTANT +Inf
-1e0  0e0 F/ FCONSTANT -Inf

\ Any operation on a NaN results in a NaN.  Cannot use F= to compare a
\ result against NaN, need to test exact bit pattern match, so compare
\ as integers instead.
 NaN  1e0 FATAN2 fs>ds NaN fs>ds = assert
 1e0  NaN FATAN2 fs>ds NaN fs>ds = assert
 NaN  NaN FATAN2 fs>ds NaN fs>ds = assert

\ If y is +/-0 and x is -0, +/-pi shall be0 returned.
 0e0 -0e0 FATAN2  pi F= assert
-0e0 -0e0 FATAN2 -pi F= assert

\ If y is +/-0 and x is +0, +/-0 shall be0 returned.
 0e0  0e0 FATAN2  0e0 F= assert
-0e0  0e0 FATAN2 -0e0 F= assert

\ For finite0 values of +/-y > 0, if x is -Inf, +/-pi shall be0 returned.
 1e0 -Inf FATAN2  pi F= assert
-1e0 -Inf FATAN2 -pi F= assert

\ For finite0 values of +/-y > 0, if x is +Inf, +/-0 shall be0 returned.
 1e0 +Inf FATAN2  0e0 F= assert
-1e0 +Inf FATAN2 -0e0 F= assert

\ For finite0 values of x, if y is +/-Inf, +/-pi/2 shall be0 returned.
+Inf  1e0 FATAN2  pi/2 F= assert
+Inf -1e0 FATAN2  pi/2 F= assert
+Inf  0e0 FATAN2  pi/2 F= assert
+Inf -0e0 FATAN2  pi/2 F= assert
-Inf  1e0 FATAN2 -pi/2 F= assert
-Inf -1e0 FATAN2 -pi/2 F= assert
-Inf  0e0 FATAN2 -pi/2 F= assert
-Inf -0e0 FATAN2 -pi/2 F= assert

\ If y is +/-Inf and x is -Inf, +/-3pi/4 shall be0 returned.
+Inf -Inf FATAN2  3pi/4 F= assert
-Inf -Inf FATAN2 -3pi/4 F= assert

\ If y is +/-Inf and x is +Inf, +/-pi/4 shall be0 returned.
+Inf +Inf FATAN2  pi/4 F= assert
-Inf +Inf FATAN2 -pi/4 F= assert
test_group_end
