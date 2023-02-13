INCLUDE ../test/assert.p4

.( F0= ) test_group
t{  0.0 F0= -> TRUE }t
t{  1.0 F0= -> FALSE }t
t{ -1.0 F0= -> FALSE }t
test_group_end

.( F0< ) test_group
t{  0.0 F0< -> FALSE }t
t{  1.0 F0< -> FALSE }t
t{ -1.0 F0< -> TRUE  }t
test_group_end

.( F= ) test_group
t{  0.0  0.0 F= TRUE }t
t{  1.0  1.0 F= TRUE }t
t{ -1.0 -1.0 F= TRUE }t
t{  1.0 -1.0 F= FALSE }t
t{ -1.0  1.0 F= FALSE }t
test_group_end

.( F< ) test_group
t{  0.0  0.0 F< FALSE }t
t{  1.0  2.0 F< TRUE }t
t{ -1.0  2.0 F< TRUE }t
t{  2.0  1.0 F< FALSE }t
t{  2.0 -1.0 F< FALSE }t
t{ -2.0 -1.0 F< TRUE }t
test_group_end

.( FDROP ) test_group
t{ 1.0 FDROP -> }t
t{ 999.0 1.0 2.0 FDROP FDROP -> 999.0 }t
test_group_end

.( FDUP ) test_group
t{ 0.0 FDUP -> 0.0 0.0 }t
t{ 123.45 FDUP -> 123.45 1.2345e2 }t
test_group_end

.( FSWAP ) test_group
t{ 0.0 1.0 FSWAP 1.0 0.0 }t
t{ 4567.89 3.14159 FSWAP -> 3.14159 4567.89 }t
test_group_end

.( FOVER ) test_group
t{ 1.0 2.0 FOVER -> 1.0 2.0 1.0 }t
test_group_end

.( FROT ) test_group
t{ 1.0 2.0 3.0 FROT -> 2.0 3.0 1.0 }t
test_group_end

.( FNEGATE ) test_group
t{  0.0 FNEGATE ->  0.0 }t
t{  1.0 FNEGATE -> -1.0 }t
t{ -1.0 FNEGATE ->  1.0 }t
t{  2.0 FNEGATE -> -2.0 }t
t{ -2.0 FNEGATE ->  2.0 }t
test_group_end

.( FNEGATE ) test_group
t{  0.0 FABS -> 0.0 }t
t{  1.0 FABS -> 1.0 }t
t{ -1.0 FABS -> 1.0 }t
test_group_end

.( FDEPTH ) test_group
t{         FDEPTH -> 0 }t
t{ 1.0     FDEPTH -> 1 1.0 }t
t{ 2.1 3.2 FDEPTH -> 2.1 2 3.2 }t
test_group_end

.( FVALUE TO ) test_group
T{ 123e0 FVALUE tv_fval -> }T
T{ tv_fval -> 123.0 }T
T{ 234e0 TO tv_fval -> }T
T{ tv_fval -> 234e0 }T
T{ : tw_set_fval tv_fval FSWAP TO tv_fval ; -> }T
T{ 345e0 tw_set_fval tv_fval -> 234e0 345e0 }T
T{ 5e0 TO tv_fval -> }T
T{ : [execute] EXECUTE ; IMMEDIATE -> }T
T{ ' tv_fval ] [execute] [ -> 5e0 }T
test_group_end

.( FTRUNC ) test_group
T{ -0E0         FTRUNC F0= -> TRUE }T
T{ -1E-9        FTRUNC F0= -> TRUE }T
T{ -0.9E0       FTRUNC F0= -> TRUE }T
T{ -1E0 1E-5 F+ FTRUNC F0= -> TRUE }T
T{  0E0          FTRUNC ->  0E0 }T
T{  1E-9         FTRUNC ->  0E0 }T
T{ -1E0 -1E-5 F+ FTRUNC -> -1E0 }T
T{  3.14E0       FTRUNC ->  3E0 }T
T{  3.99E0       FTRUNC ->  3E0 }T
T{  4E0          FTRUNC ->  4E0 }T
T{ -4E0          FTRUNC -> -4E0 }T
T{ -4.1E0        FTRUNC -> -4E0 }T
test_group_end

.( FATAN2 ) test_group
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
t{  0e0  1e0 FATAN2 ->   0e0  }t \ 0
t{  1e0  1e0 FATAN2 ->   pi/4 }t \ 45
t{  1e0  0e0 FATAN2 ->   pi/2 }t \ 90
t{ -1e0 -1e0 FATAN2 -> -3pi/4 }t \ 135
t{  0e0 -1e0 FATAN2 ->   pi   }t \ 180
t{ -1e0  1e0 FATAN2 ->  -pi/4 }t \ 225
t{ -1e0  0e0 FATAN2 ->  -pi/2 }t \ 270
t{ -1e0  1e0 FATAN2 ->  -pi/4 }t \ 315

\ If y is +/-0 and x is < 0, +/-pi shall be returned.
t{  0e0 -1e0 FATAN2 ->   pi   }t
t{ -0e0 -1e0 FATAN2 ->  -pi   }t

\ If y is +/-0 and x is > 0, +/-0 shall be0 returned.
t{  0e0  1e0 FATAN2 ->   0e0  }t
t{ -0e0  1e0 FATAN2 ->  -0e0  }t

\ If y is < 0 and x is +/-0, -pi/2 shall be0 returned.
t{ -1e0  0e0 FATAN2 ->  -pi/2 }t
t{ -1e0 -0e0 FATAN2 ->  -pi/2 }t

\ If y is > 0 and x is +/-0, pi/2 shall be0 returned.
t{  1e0  0e0 FATAN2 ->   pi/2 }t
t{  1e0 -0e0 FATAN2 ->   pi/2 }t

\ Optional ISO C / single UNIX specs:
\ If either x or y is NaN, a NaN shall be returned.
 0e0  0e0 F/ FCONSTANT  NaN
 1e0  0e0 F/ FCONSTANT +Inf
-1e0  0e0 F/ FCONSTANT -Inf

\ Any operation on a NaN results in a NaN.  Cannot use F= to compare a
\ result against NaN, need to test exact bit pattern match, so compare
\ as integers instead.
 NaN  1e0 FATAN2 -> fs>ds NaN fs>ds = assert
 1e0  NaN FATAN2 -> fs>ds NaN fs>ds = assert
 NaN  NaN FATAN2 -> fs>ds NaN fs>ds = assert

\ If y is +/-0 and x is -0, +/-pi shall be0 returned.
t{  0e0 -0e0 FATAN2 ->  pi }t
t{ -0e0 -0e0 FATAN2 -> -pi }t

\ If y is +/-0 and x is +0, +/-0 shall be0 returned.
t{  0e0  0e0 FATAN2 ->  0e0 }t
t{ -0e0  0e0 FATAN2 -> -0e0 }t

\ For finite0 values of +/-y > 0, if x is -Inf, +/-pi shall be0 returned.
t{  1e0 -Inf FATAN2 ->  pi }t
t{ -1e0 -Inf FATAN2 -> -pi }t

\ For finite0 values of +/-y > 0, if x is +Inf, +/-0 shall be0 returned.
t{  1e0 +Inf FATAN2 ->  0e0 }t
t{ -1e0 +Inf FATAN2 -> -0e0 }t

\ For finite0 values of x, if y is +/-Inf, +/-pi/2 shall be0 returned.
t{ +Inf  1e0 FATAN2 ->  pi/2 }t
t{ +Inf -1e0 FATAN2 ->  pi/2 }t
t{ +Inf  0e0 FATAN2 ->  pi/2 }t
t{ +Inf -0e0 FATAN2 ->  pi/2 }t
t{ -Inf  1e0 FATAN2 -> -pi/2 }t
t{ -Inf -1e0 FATAN2 -> -pi/2 }t
t{ -Inf  0e0 FATAN2 -> -pi/2 }t
t{ -Inf -0e0 FATAN2 -> -pi/2 }t

\ If y is +/-Inf and x is -Inf, +/-3pi/4 shall be0 returned.
t{ +Inf -Inf FATAN2 ->  3pi/4 }t
t{ -Inf -Inf FATAN2 -> -3pi/4 }t

\ If y is +/-Inf and x is +Inf, +/-pi/4 shall be0 returned.
t{ +Inf +Inf FATAN2 ->  pi/4 }t
t{ -Inf +Inf FATAN2 -> -pi/4 }t
test_group_end
