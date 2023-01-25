Post4 (Post-Forth)
==================

Copyright 2007, 2023 Anthony Howe.  All rights reserved.


Building
---------

Post4 is written in ISO C11 using only one extension, `Labels As Values`, which is supported by `gcc` and `clang` compilers.  The bulk of the floating point support is provided by ISO C11 `libm`, which is enabled by default.  It can be disabled.

        $ ./configure --disable-math


### Floating Point Words

#### >FLOAT
( `caddr` `u` -- `F:f` `bool` )  
Convert the numeric floating point string `caddr` `u` into an internal floating point value `f` on the float stack.  Return true if `caddr` `u` is valid string, otherwise false. 

- - -
#### F\!
( `F:f` `faddr` -- )  
Store `f` at `faddr`.

- - -
#### F*
(F: `f1` `f2` -- `f3` )  
Multiply the top two cells on the float stack.

- - -
#### F**
(F: `f1` `f2` -- `f3` )  
Raise `f1` to the power `f2`, giving the product `f3`.

- - -
#### F+
(F: `f1` `f2` -- `f3` )  
Add the top two cells on the float stack.

- - -
#### F-
(F: `f1` `f2` -- `f3` )  
Subtract `f2` from `f1` placing the result `f3` on the float stack.

- - -
#### F/
(F: `f1` `f2` -- `f3` )  
Divide the top two cells on the float stack.

- - -
#### F.
(F: `f` -- )  
Display, with a trailing space, the top number on the float stack using fixed-point notation:

        [-] 〈digits〉.〈digits0〉

- - -
#### F0\<
(F: `f1` `f2` -- `f3` )  
`bool` is true if and only if `f` is less than zero (0).

- - -
#### F0=
(F: `f` -- ) ( -- `bool` )  
`bool` is true if and only if `f` equals  zero (0).

- - -
#### F\<
(F: `f1` `f2` --  ) ( -- `bool`)  
`bool` is true if and only if `f1` is less than `f2`.

- - -
#### F@
( `faddr` -- ) (F: -- `f` )  
Fetch from `faddr` the value `f` stored there.

- - -
#### FABS
(F: `f1` -- `f2` )  
`f2` is the absolute value of `f1`.

- - -
#### FALIGN
( -- )  
If the data-space pointer is not float aligned, `reserve` enough space to align it.

- - -
#### FALIGNED
( `addr` -- `faddr` )  
`faddr` is the first float aligned address greater than or equal to `addr`.

- - -
#### FCONSTANT
(F: `f` -- ) ( `<spaces>name` -- )  
Define the word `name` to represent the float constant value `f`.  When `name` is executed, the value `f` is pushed on to the float stack.

- - -
#### FCOS
(F: `f1` -- `f2` )  
`f2` is the cosine of the radian angle `f1`.

- - -
#### FDEPTH
( -- `u` )  
Number of cells on the float stack before `f` was placed there.

- - -
#### FDROP
(F: `f` -- )  
Remove the top of the float stack.

- - -
#### FDUP
(F: `f` -- `f` `f` )  
Duplicate `f` on the float stack.

- - -
#### FEXP
(F: `f1` -- `f2` )  
Raise `e` to the power `f1`, giving `f2`.

- - -
#### FFIELD:
( `n1` `<spaces>name` -- `n2` )  
Skip leading space delimiters.  Parse name delimited by a space.  Offset is the first float aligned value greater than or equal to `n1`.  `n2 = offset + 1` float.

- - -
#### FLITERAL
(F: `f` -- ) immediate  
Compile `f` into the definition so that it is later pushed onto the float stack during execution of the definition.

- - -
#### FLN
(F: `f1` -- `f2` )  
`f2` is the natural logarithm of `f1`.

- - -
#### FLOAT+
( `faddr1` -- `faddr2` )  
Add the size in address units of a float to `faddr1` giving `faddr2`.

- - -
#### FLOATS
( `n1` -- `n2` )  
`n2` is the size in address units of `n1` floating point numbers.

- - -
#### FLOG
(F: `f1` -- `f2` )  
`f2` is the base-10 logarithm of `f1`.

- - -
#### FLOOR
(F: `f1` -- `f2` )  
Round `f1` to an integral value using the "round toward negative infinity" rule, giving `f2`.

- - -
#### FMAX
(F: `f1` `f2` -- `f3` )  
Float `f3` is the greater of `f1` and `f2`.

- - -
#### FMIN
(F: `f1` `f2` -- `f3` )  
Float `f3` is the lesser of `f1` and `f2`.

- - -
#### FNEGATE
(F: `f1` -- `f2` )  
Negate the float `f1`, giving its arithmetic inverse `f2`.

- - -
#### FOVER
(F: `f1` `f2` -- `f1` `f2` `f1` )  
Copy the second value `f1` below the float stack to the top.

- - -
#### FROT
(F: `f1` `f2` `f3` -- `f2` `f3` `f1` )  
Rotate the top three float stack entries.

- - -
#### FROUND
(F: `f1` -- `f2` )  
Round `f1` to an integral value using the "round to nearest" rule, giving `f2`.

- - -
#### FS.
(F: `f` -- )  
Display, with a trailing space, the top number on the float stack using scientific notation:

        [-] digits[.digits] E digits

- - -
#### FSIN
(F: `f1` -- `f2` )  
`f2` is the sine of the radian angle `f1`.

- - -
#### FSWAP
(F: `f1` `f2` -- `f2` `f1` )  
Exchange the top two float stack items.

- - -
#### FSQRT
(F: `f1` -- `f2` )  
`f2` is the square root `f1`.

- - -
#### FTAN
(F: `f1` -- `f2` )  
`f2` is the tangent of the radian angle `f1`.

- - -
#### FVARIABLE
( `<spaces>name` -- )  
Create `name` with one float of data.  When `name` is executed push the `faddr` of the float.

- - -
#### PRECISION
( -- `u` )  
Return the number of significant digits currently used by `F.`, `FE.`, or `FS.` as `u`.

- - -
#### SET-PRECISION
( u -- )  
Set the number of significant digits currently used by `F.`, `FE.`, or `FS.` to `u`.

- - -

### Post4 Specific Words

#### floating-stack
(  -- `u` ) constant  
Size of the float stack.  Zero (0) if the float stack is combined with the data stack.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### f>r
(F: `f` -- ) (R: -- `f` )  
Move top of the float stack to the return stack.

- - -
#### r>f
(R: `f` -- ) (F: -- `f` )  
Move top of the return stack to the float stack.

- - -
