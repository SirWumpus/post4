Post4 (Post-Forth)
==================

Copyright 2007, 2024 Anthony Howe.  All rights reserved.


### Double-Cell Words

#### 2CONSTANT
( `x` `y` `<spaces>name` -- )  
Define the word `name` to represent the constant values `x` and `y`.  When `name` is executed, the values `x` and `y` are pushed on to the stack.

        $dead $beef 2CONSTANT steak

- - -
#### 2LITERAL
( `x` `y`  -- ) immediate  
Compile `x` and `y` into the definition so that they are later pushed onto the stack during execution of the definition.

         : SOMEWORD ... [ x y ] 2LITERAL ... ;

- - -
#### 2ROT
( `x1` `x2` `x3` `x4` `x5` `x6` -- `x3` `x4` `x4` `x5` `x1` `x2` )  
Rotate the top three cell pairs on the stack bringing cell pair `x1` `x2` to the top of the stack.

- - -
#### 2VALUE
( `lo` `hi` `<spaces>name` -- )  
Create `name` with two cells of data assigned `hi` and `lo`.  When `name` is executed, the values `lo` `hi` are pushed to the stack.  See [TO](./standard.md).

- - -
#### 2VARIABLE
( `<spaces>name` -- )  
Create `name` with two cells of data.  When `name` is executed push the `aaddr` of the data cells.

        2VARIABLE name          \ define name
        123 987 name 2!         \ store values to variable
        name 2@                 \ fetch value from variable

-- - -
#### D+
( `d1|ud1` `d2|ud2` -- `d3|ud3` )  
Add the double-cells `d2|ud2` to `d1|ud1`, giving the sum `d3|ud3`.

#### D-
( `d1|ud1` `d2|ud2` -- `d3|ud3` )  
Subtract the double-cells `d2|ud2` from `d1|ud1`, giving the difference `d3|ud3`.

- - -
#### D.
( `d` -- )  
Display double-cell `d` in free field format.

- - -
#### D.R
( `d` `n` -- )  
Display double-cell `d` right aligned in a field `n` characters wide.  If the number of characters required to display `d` is
greater than `n`, all digits are displayed with no leading spaces in a field as wide as necessary.

- - -
#### D0<
( `d` -- `bool` )  
`bool` is true if and only if the double-cell `d` (`lo` `hi`) is less than to zero (0).

- - -
#### D0=
( `d` -- `bool` )  
`bool` is true if and only if the double-cell `d` (`lo` `hi`) is equal to zero (0).

- - -
#### D<
( `d1` `d2` -- `bool` )  
`bool` is true if and only if the double-cell `d1` is less then `d2`.

- - -
#### D=
( `d1` `d2` -- `bool` )  
`bool` is true if and only if the double-cell `d1` is equal to `d2`.

- - -
#### D>S
( `d` -- `n` )  
Convert the double-cell number `d` (`lo` `hi`) to the single-cell number `n` with the same numerical value.

- - -
#### D2*
( `xd` -- `yd` )  
The double-cell `yd` is the result of shifting `xd` one bit toward the most-significant bit, filling the vacated least-significant bit with zero.

- - -
#### D2/
( `xd` -- `yd` )  
The double-cell `yd` is the result of shifting `xd` one bit toward the least-significant bit leaving the most-significant bit unchanged, .ie arithmetic right shift.

- - -
#### DABS
( `d` -- `ud` )  
Double-cell `ud` is the absolute value of `d`.

- - -
#### DMAX
(S: `d1` `d2` -- `d3` )  
Double-cell `d3` is the greater of `d1` and `d2`.

- - -
#### DMIN
(S: `d1` `d2` -- `d3` )  
Double-cell `d3` is the lesser of `d1` and `d2`.

- - -
#### DNEGATE
( `d1` -- `d2` )  
Negate the double-cell `d1`, giving its arithmetic inverse `d2`.

- - -
#### DU<
( `ud1` `ud2` -- `bool` )  
`bool` is true if and only if the unsigned double-cell `ud1` is less then `ud2`.

- - -
#### M*/
( `d1` `n1` `+n2` -- `d2` )  
Multiply `d1` by `n`1 producing the triple-cell intermediate result `t`.  Divide `t` by `n2` giving the double-cell quotient `d2`. 

- - -
#### M+
( `d1|ud1` `n` -- `d2|ud2` )  
Add single-cell `n` to the double-cell `d1|ud1` (`lo` `hi`) giving the sum `d2|ud2`.

- - -

### Post4 Specific Words

#### max-d
( -- `d` ) constant  
Largest usable double-cell signed integer.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### max-ud
( -- `ud` ) constant  
Largest usable double-cell unsigned integer.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### min-d
( -- `d` ) constant  
Smallest usable double-cell signed integer.

- - -
