INCLUDE-PATH post4/assert.p4

[UNDEFINED] F@ [IF]

.( Float support disabled. ) CR

[ELSE]

.( GH-68 e is not a float ) test_group
t{ s" 0 1 efoobar-undefined-word" ' evaluate catch nip nip -> -13 }t
test_group_end

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

.( FABS ) test_group
t{  0.0 FABS -> 0.0 }t
t{  1.0 FABS -> 1.0 }t
t{ -1.0 FABS -> 1.0 }t
test_group_end

.( FDEPTH ) test_group
t{         FDEPTH -> 0 }t
t{ 1.0     FDEPTH -> 1 1.0 }t
t{ 2.1 3.2 FDEPTH -> 2.1 2 3.2 }t
test_group_end

.( >FLOAT ) test_group
t{ S" 2.718281" >FLOAT -> 2.718281 TRUE }t 	\ e = 2.718281828459 ...
t{ S" 3.141592" >FLOAT -> 3.141592 TRUE }t	\ PI = 3.141592653589 ...
t{ S" 6.62607015e-34" >FLOAT -> 6.62607015e-34 TRUE }t	\ Planck's
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
 NaN  1e0 FATAN2 f> NaN f> = assert
 1e0  NaN FATAN2 f> NaN f> = assert
 NaN  NaN FATAN2 f> NaN f> = assert

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

.( FFIELD: FLOATS ) test_group
t{ 0 FFIELD: tv_field_0 CONSTANT tv_struct_0 -> }t
t{ tv_struct_0 -> 1 FLOATS }t
t{ 0 tv_field_0 -> 0 }t
t{ 0 FFIELD: tv_field_a FFIELD: tv_field_b FFIELD: tv_field_c CONSTANT tv_struct_1 -> }t
t{ tv_struct_1 0 tv_field_a 0 tv_field_b 0 tv_field_c -> 3 FLOATS 0 1 FLOATS 2 FLOATS }t
t{ 0 FFIELD: tv_field_d FIELD: tv_field_e FFIELD: tv_field_f CONSTANT tv_struct_2 -> }t
t{ tv_struct_2 0 tv_field_d 0 tv_field_e 0 tv_field_f -> 2 FLOATS CELL+ 0 1 FLOATS 1 FLOATS CELL+ }t
test_group_end

\ Construct some floating point numbers based on IEEE 754.
_exponent_mask	                  >f FCONSTANT +inf
_exponent_mask _sign_mask OR      >f FCONSTANT -inf
_exponent_mask	             1 OR >f FCONSTANT +nan
_exponent_mask _sign_mask OR 1 OR >f FCONSTANT -nan

2.718281828459 FCONSTANT e
3.141592653589 FCONSTANT pi
6.62607015e-34 FCONSTANT planck

.( inf? ) test_group
t{  123.45 inf? -> FALSE }t
t{ -123.45 inf? -> FALSE }t
t{ +nan    inf? -> FALSE }t
t{ -nan    inf? -> FALSE }t
t{ +inf    inf? -> TRUE }t
t{ -inf    inf? -> TRUE }t
test_group_end

.( nan? ) test_group
t{  123.45 nan? -> FALSE }t
t{ -123.45 nan? -> FALSE }t
t{ +inf    nan? -> FALSE }t
t{ -inf    nan? -> FALSE }t
t{ +nan    nan? -> TRUE }t
t{ -nan    nan? -> TRUE }t
test_group_end

.( REPRESENT ) test_group
6 SET-PRECISION
: dumppad pad PRECISION dump ;

t{ 0.1e0 PAD PRECISION REPRESENT S" 100000" PAD PRECISION COMPARE -> 0 FALSE TRUE 0 }t
t{ 0.4e0 PAD PRECISION REPRESENT S" 400000" PAD PRECISION COMPARE -> 0 FALSE TRUE 0 }t
t{ 0.6e0 PAD PRECISION REPRESENT S" 600000" PAD PRECISION COMPARE -> 0 FALSE TRUE 0 }t

t{    0.0   PAD PRECISION REPRESENT S" 000000" PAD PRECISION COMPARE -> 0 FALSE TRUE 0 }t
t{  123.45  PAD PRECISION REPRESENT S" 123450" PAD PRECISION COMPARE -> 3 FALSE TRUE 0 }t
t{ -123.45  PAD PRECISION REPRESENT S" 123450" PAD PRECISION COMPARE -> 3  TRUE TRUE 0 }t
t{  0.12345 PAD PRECISION REPRESENT S" 123450" PAD PRECISION COMPARE -> 0 FALSE TRUE 0 }t
t{ -0.12345 PAD PRECISION REPRESENT S" 123450" PAD PRECISION COMPARE -> 0  TRUE TRUE 0 }t

t{ e        PAD PRECISION REPRESENT S" 271828" PAD PRECISION COMPARE -> 1  FALSE TRUE 0 }t
t{ pi       PAD PRECISION REPRESENT S" 314159" PAD PRECISION COMPARE -> 1  FALSE TRUE 0 }t
t{ planck   PAD PRECISION REPRESENT S" 662607" PAD PRECISION COMPARE -> -33 FALSE TRUE 0 }t

\ The exp and sign are undefined when the number is undefined.
t{ +nan     PAD PRECISION REPRESENT -ROT 2DROP -> FALSE }t
t{ -nan     PAD PRECISION REPRESENT -ROT 2DROP -> FALSE }t
t{ +inf     PAD PRECISION REPRESENT -ROT 2DROP -> FALSE }t
t{ -inf     PAD PRECISION REPRESENT -ROT 2DROP -> FALSE }t
test_group_end
[THEN]

[THEN]
