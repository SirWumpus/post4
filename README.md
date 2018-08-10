Post4 (Post-Forth)
==================

Copyright 2018 Anthony Howe.  All rights reserved.


Overview
--------

Post4 (`p4`) is an indirect threaded Forth dialect written in C, based on the ["Forth 200x Draft 16.1, 2016-08-30"](http://www.forth200x.org/documents/forth16-1.pdf).

```
usage: p4 [-V][-b file][-c file][-d size][-r size] [script [args ...]]

-b file         block file; default p4.blk
-c file         word definition file; default p4.p4
-d size         data stack size in cells; default 32
-r size         return stack size in cells; default 32
-V              build and version information
```

Standard Words
--------------

### ! 
( `x` `aaddr` -- )  
Store `x` at `aaddr`.

- - -
### \# 
( `ud1` -- `ud2` )  
Divide `ud1` by the number in `BASE` giving the quotient `ud2` and the remainder `n`.  (`n` is the least significant digit of `ud1`.)  Convert `n` to external form and append the resulting character to the pictured numeric output string. 

- - -
### #> 
( `xd` -- `caddr` `u` )  
Drop `xd`.  Make the pictured numeric output string available as a character string.  `caddr` and `u` specify the resulting character string.  A program may replace characters within the string.

- - -
### #S 
( `ud1` -- `0` )  
Convert one digit of `ud1` according to the rule for `#`.  Continue conversion until the quotient is zero.

- - -
### ' name
( `"<spaces>name"` -- `xt` )  
Find `name` and place its execution token on the stack.  Throw undefined word (-13) if not found.

- - -
### ( ccc)
( `"ccc )"` -- ) immediate  
Parse and ignore characters upto the closing right parenthesis.

- - -

### \*
( `n1|u1` `n2|u2` -- `n3|n3` )  
Multiply the top two stack values.

- - -

### +
( `n1|u1` `n2|u2` -- `n3|n3` )  
Add the top two stack values.

- - -

### +!
( `n|u` `aaddr` -- )  
Add `n|u` to the cell at `aadr`.

- - -

### ,
( `x` -- )  
//Align// and reserve one cell of data-space and store `x` there.

- - -

### -
( `n1|u1` `n2|u2` -- `n3|n3` )  
Subtract the top two stack values.

- - -
### .
( `n` -- )  
Display `n` in free field format.

- - -
### ." ccc"
( `"ccc \""` -- )  
Display `ccc`.

- - -

### /
( `n1|u1` `n2|u2` -- `n3|n3` )  
Divide the top two stack values.

- - -

### /MOD
( `n1` `n2` -- `rem` `quot` )  

Divide the top two stack values and place the remainder and quotient back on the stack.

- - -
### 0<
( `n` -- `bool` )  
`bool` is true if and only if `n` is less than zero.

- - -
### 0=
( `x` -- `bool` )  
`bool` is true if and only if `x` is equal to zero.

- - -
### 1+

( `n1|u1` -- `n2|u2` )  
Add one (1) to `n1|u1` giving the sum `n2|u2`.

- - -
### 1-

( `n1|u1` -- `n2|u2` )  
Subtract one (1) from `n1|u1` giving the difference `n2|u2`.

- - -
### 2!
( `lo` `hi` `aaddr` -- )  
Store the cell pair `lo hi` at `aaddr` as `hi lo`.

- - -
### 2*
( `x1` -- `x2` )  
`x2` is the result of shifting `x1` one bit toward the most-significant bit, filling the vacated least-significant bit with zero.

- - -
### 2/
( `x1` -- `x2` )  
`x2` is the result of shifting `x1` one bit toward the least-significant bit, leaving the most-significant bit unchanged.

- - -
### 2@
( `aaddr` -- `lo` `hi` )  
Fetch the cell pair `hi lo` stored at `aaddr` and place on the stack as `lo hi`.lo hi.

- - -
### 2DROP
( `x1` `x2` -- )  
Drop cell pair `x1 x2` from the stack.

- - -
### 2DUP
( `x1` `x2` -- `x1` `x2` `x1` `x2` )  
Duplicate cell pair `x1 x2`.

- - -
### 2OVER
( `x1` `x2` x3 x4 -- `x1` `x2` x3 x4 `x1` `x2` )  
Copy cell pair `x1 x2` to the top of the stack.

- - -
### 2SWAP
( `x1` `x2` x3 x4 -- x3 x4 `x1` `x2` )  
Exchange the top two cell pairs.

- - -
### : name ...
Start definition of word `name`.

- - -
### ;
End definition of word.

- - -
### <
( `n1` `n2` -- `bool` )  
`bool` is true if and only if `n1` is less than `n2`.

- - -
### <#
( -- )  
Initialize the pictured numeric output conversion process.

- - -
### =
( `n1` `n2` -- `bool` )  
`bool` is true if and only if `x1` is bit-for-bit the same as `x2`.

- - -
### >
( `n1` `n2` -- `bool` )  
`bool` is true if and only if `n1` is greater than `n2`.

- - -
### >BODY
( `xt` -- `aaddr` )

`aaddr` is the data-field address corresponding to `xt`.  Will throw `-31` if `xt` is not a word defined by `CREATE`.

- - -
### >IN
( -- `aaddr` )
`aaddr` is the address of a cell containing the offset in characters from the start of the
input buffer to the start of the parse area.

### >R
( `x` -- )(R: -- `x` )  
Move cell `x` from the data stack to the return stack.

- - -
### ?DUP
( `x` -- `x` `x` )
Duplicate `x` if it is non-zero.

- - -
### @
( `aaddr` -- `x` )  
Fetch from `aaddr` the value `x` stored there.

- - -
### ABORT
( `i*x` -- ) ( R: `j*x` -- )  
Throw -1.  Clear the data stack and perform a `QUIT`, which clears the return stack too.

- - -
### ABORT" ccc"
( `i*x` `x1` -- | `i*x` ) ( R: `j*x` -- | `j*x` )  
If `x1` is not equal to zero, display the message that follows upto the closing double-quote and perform an `ABORT`.

- - -
### ABS
( `n` -- `u` )  
Return the absolute value of `n`.

- - -
### ACCEPT
( `caddr` `+n1` -- `+n2` )  
Accept upto `+n1` (between 1 and +32767) characters into the buffer given by `caddr`; the number of characters actually received, `+n2`, is pushed.

- - -
### ALIGN
( -- )  
If the data-space pointer is not aligned, reserve enough space to align it.

- - -
### ALIGNED
( `addr` -- `aaddr` )  
`aaddr` is the first cell aligned address greater than or equal to `addr`.

- - -
### ALLOT
( `n` -- )  
If `n` is greater than zero, reserve `n` address units of data space. If `n` is less than zero, release `|n|` address units of
data space. If `n` is zero, leave the data-space pointer unchanged.

- - -
### AND
( `x1` `x2` -- `x3` )
Bit-wise and of the top two stack values.

- - -
### BASE
( -- `aaddr` )  
`aaddr`is the address of a cell containing the current number conversion radix, between 2..36.

- - -
### BEGIN
( -- )  
Mark the start of `BEGIN...AGAIN` or `BEGIN...WHILE...REPEAT` loop.

- - -
### BL
( -- `char` )  
`char` is the character value for a space, since Forth words are delimited by space.  A space can also be specified with `'\s'`.

- - -
### C!
( `char` `caddr` -- )  
Store `char` at `caddr`.

- - -
### C,
( `char` -- )  


- - -
### C@
( `caddr` -- `char` )  
Fetch from `caddr` the character `char` stored there.

- - -
### CELL+
( `aaddr1` -- `aaddr2` )  
Add the size in address units of a cell to `aaddr1` giving `aaddr2`.

- - -
### CELLS
( `n1` -- `n2` )  
`n2` is the size in address units of `n1` cells.

- - -
### CHAR text
( "<spaces>text" -- `char` )  
Parse `text` placing the first character of text on the stack.

- - -
### CONSTANT name
( `x` "<spaces>name" -- )  
Define the word `name` to represent the constant value `x`.  When `name` is executed, the value `x` is pushed on to the stack.

- - -
### CR
( -- )  
Write a newline to standard output.

- - -
### CREATE
( "<spaces>name" -- )  
Create a new word definition.  When `name` is executed, the `aaddr` of its data space is pushed onto the stack.  `CREATE` can be used to define new data structure, eg.

```
\ Define how to make an array.
: array ( n "<spaces>name" -- ) CREATE CELLS ALLOT ;

\ Define a new array of 3 cells.
3 array fred

\ Store 123 into fred[1].
123 fred 1 CELLS + !
```

Or when combined with `DOES>` make new "defining" words, eg. 

```
\ General structure:
: word1 CREATE ( build data space of word2 ) DOES> ( actions applied to data of word2 ) ;

\ Make a new defining word CONSTANT.
: CONSTANT CREATE , DOES> @ ;

\ Use CONSTANT to define a word representing a value.
377 CONSTANT monaco

\ Use the new word.
monaco ( -- 377 )
```

- - -
### DECIMAL
( -- )  
Set the numeric conversion radix to ten (decimal).

- - -
### DEPTH
( -- `u` )  
Number of cells on the data stack before `u` was placed there.

- - -
### DO
Mark the start of `DO ... +LOOP` or `DO ... LOOP`.

- - -
### DOES>
Define the execution semantics for the most recently defined word by `CREATE`.  Throws -31 if `DOES>` is applied to a word not defined by `CREATE`.

```
\ General structure:
: word1 CREATE ( build data space of word2 ) DOES> ( actions applied to data of word2 ) ;

\ Make a new defining word CONSTANT.
: CONSTANT CREATE , DOES> @ ;

\ Use CONSTANT to define a word representing a value.
377 CONSTANT monaco

\ Use the new word.
monaco ( -- 377 )
```

- - -
### DROP
( `x` -- )  
Remove the top of the stack.

- - -
### DUP
( `x` -- `x` `x` )  
Duplicate `x`.

- - -
### ELSE


- - -
### EMIT
( `char` -- )  
Write the character octet to standard output.

- - -
### EVALUATE
( `i*x` `caddr` `u` -- `j*x` )  

- - -
### EXECUTE
( `i*x` `xt` -- `j*x` )  
Remove the execution token `xt` from the stack and perform the semantics identified by it. 

- - -
### EXIT
( -- )(R: ip -- )  
Return control from the currently executing word to its caller.  Before executing `EXIT` within a do-loop, a program shall discard the loop-control parameters by executing `UNLOOP`.

- - -
### FILL
( `caddr` `u` `char` -- )  
Fill memory at address `caddr` with `u` consecutive characters `char`.

- - -
### HERE
( -- `addr` )
`addr` is the data-space pointer that will next be assigned by `,`, `ALIGN`, `ALLOT`, `C,`, `COMPILE,`.

- - -
### HOLD
( `char` -- )
Append `char` to the picture numeric output string.

- - -

Post4 Specific Words
--------------------

### >here
( -- `u` )  
Offset into the current data-space for the word being compiled.  Similar to the word `HERE`, except expressed as an offset from the start of the data-space.  During the compiliation of a word in C based implementations, the data-space region may be relocated when they are enlarged by `,`, `ALIGN`, `ALLOT`, `C,`, `COMPILE,` thus invalidating previous values of `HERE`.  Providing an offset into the current data-region allows for computing relative locations.

- - -
### _bp
( -- )  

- - -
### _branch
( -- )  
Branch relative.  The integer that immediately follows is the relative distance in address units from the integer's address.  Used in the definition of flow control words, like `AGAIN` and `AHEAD`, `ELSE`, `REPEAT`.

- - -
### _branchz
( `bool` -- )  
Branch relative if zero.  The integer that immediately follows is the relative distance in address units from the integer's address.  Used in the definition of flow control words, like `IF`, `WHILE`, `UNTIL`.

- - -
### _ds
( -- `aaddr` `u` )  
Push the base data stack address and size (depth).

- - -
### reserve

( `u1` -- `addr` )  
Similar to `ALLOT`, reserve `u1` address-units of data-space and return its start address.

### .rs
( -- )  
Dump the return stack.

- - -
### _rs
( -- aaddr u )  
Push the base return stack address and size (depth).

- - -
### _stack_dump
( aaddr u -- )  
Utility word used to define `.S` and `.rs`.

- - -

## Supported THROW Codes 

* -1 `ABORT`  
* -2 `ABORT"`  
* -3 stack overflow  
* -4 stack underflow  
* -5 return stack overflow  
* -6 return stack underflow  
* -9 invalid memory address (SIGSEGV)  
* -13 undefined word  
* -17 pictured numeric output string overflow  
* -23 address alignment exception (SIGBUS)  
* -28 user interrupt  
* -29 compiler nesting  
* -31 word not defined by CREATE
* -55 floating-point unidentified fault (SIGFPE)  
* -56 `QUIT`  
* -61 `ALLOT` or `RESIZE`
* -256 `BYE`  

- - -
- - -


References
----------

Forth 200x  
<http://www.forth200x.org/>

Forth Standard  
<https://forth-standard.org/>

