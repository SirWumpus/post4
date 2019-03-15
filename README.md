Post4 (Post-Forth)
==================

Copyright 2007, 2019 Anthony Howe.  All rights reserved.


Overview
--------

Post4 is a hosted indirect threaded Forth dialect written in C, based on the ["Forth 200x Draft 18.1, 2018-08-27"](http://www.forth200x.org/documents/forth18-1.pdf).  Post4 aims to implement the fewest possible built-in words in C, those that are needed to interact with memory and I/O, leaving the remaining standard words to be implemented in Forth.

        usage: post4 [-V][-b file][-c file][-d size][-i file][-r size] [script [args ...]]

        -b file         block file; default ./.post4.blk or $HOME/.post4.blk
        -c file         word definition file; default post4.p4 from $POST4_PATH
        -d size         data stack size in cells; default 64
        -i file         include file; can be repeated; searches $POST4_PATH
        -r size         return stack size in cells; default 64
        -V              build and version information
        
        If script is "-", read it from standard input.


The environment variable `POST4_PATH` provides a colon separated search path for the `post4.p4` core word definitions file.  If `POST4_PATH` is undefined, then an OS specific default path is used.  A specific word definition file can be specified with `-c`.

By default a user block file, `.post4.blk`, is opened from the current directory or user's `HOME` directory.  This can be overridden with the `-b` option.

Post4 reads input from standard input and writes to standard output, which can be redirected:

        echo "123 69 + ." | post4


Examples
--------

### dumpargs.p4

Simple demostration on how to access the command line arguments.

        $ post4 dumpargs.p4 hello world, tell us more!


### ed.p4 - Block Editor

There are actually three block editor word sets:

1. An `ed(1)` like block line editor using `LIST`, `PRINT`, and `CHANGE`.
2. An interactive single line editor using `EDIT`; left & right cursor keys, tab toggles insert or replace mode, delete backspace, ESC quits.
3. A full "screen" block editor using `ED`; all the commands of the interactive single line editor, plus up & down cursor keys, `CTRL+G` goto block, `CTRL+P` & `CTRL+N` block, and `ESC` menu.

NOTE: that `EDIT` and `ED` are hard coded with ANSI terminal escape sequences.

        $ p4
        ok INCLUDE ed.p4
        ok ED


### life.p4 - [Conway's Game of Life](http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)

**Rules**

The universe of the Game of Life is an infinite two-dimensional orthogonal
grid of square cells, each of which is in one of two possible states, live
or dead. Every cell interacts with its eight neighbours, which are the cells
that are directly horizontally, vertically, or diagonally adjacent. At each
step in time, the following transitions occur:

   1. Any live cell with fewer than two live neighbours dies, as if caused by under population.
   2. Any live cell with more than three live neighbours dies, as if by overcrowding.
   3. Any live cell with two or three live neighbours lives on to the next generation.
   4. Any dead cell with exactly three live neighbours becomes a live cell.

The initial pattern constitutes the seed of the system. The first generation
is created by applying the above rules simultaneously to every cell in the
seed - births and deaths happen simultaneously, and the discrete moment at
which this happens is sometimes called a tick (in other words, each generation
is a pure function of the one before). The rules continue to be applied
repeatedly to create further generations.

        $ post4 life.p4


### rand.p4

Pseudo-random number generators.  One is the simple code example from the ISO C11 draft April 12, 2011,

        \ Example from the ISO C11 draft April 12, 2011
        
        SRAND ( -- aaddr )
            Random number seed variable.
            
        RAND ( -- u )
            Return random number between 0..32767.
        
        \ Pseudo-Random Sequence Generator for 32-Bit CPUs
        \ Bruce Schneier, Dr. Dobb's Journal, v. 17, n. 2, February 1992, pp. 34-40.
        
        RANDA ( -- aaddr )
        RANDB ( -- aaddr )
        RANDC ( -- aaddr )
            Random number seed variables for three Linear Feedback Shift
            Registers.
            
        RANDOMXOR ( bits -- u )
            Return random number u generated from bits, XORing the bits from the
            three generators.
            
        RANDOMMAJ ( bits -- u )
            Return random number u generated from bits, based on the majority of
            one or zero returned from the three generators.

Example:

        $ post4
        ok incl rand.p4
        ok 12345 srand !
        ok rand . rand . rand . CR
        21468 9988 22117
        ok 12345 randa ! 54321 randb ! $deadbeef randc !
        ok 16 randomxor U. 16 randomxor U. 16 randomxor U. CR
        52917 27383 64651
        ok 16 randommaj U. 16 randommaj U. 16 randommaj U. CR
        7 9885 36906
        ok 


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
Convert one digit of `ud1` according to the rule for `#`.  Continue conversion until the quotient is zero (0).

- - -
### ' name
( `<spaces>name` -- `xt` )  
Find `name` and place its execution token on the stack.  Throw undefined word (-13) if not found.

- - -
### ( ccc)
( `ccc<paren>` -- ) immediate  
Parse and ignore characters up to the closing right parenthesis, which an empty string or span multiple lines.

- - -

### \*
( `n1|u1` `n2|u2` -- `n3|u3` )  
Multiply the top two stack values.

- - -
### \*/
( `n1` `n2` `dsor` -- `quot` )  
Multiply `n1` by `n2` then divide by divisor `dsor` giving the quotient `quot`.

- - -
### \*/MOD
( `n1` `n2` `dsor` -- `rem` `quot` )  
Multiply `n1` by `n2` then divide by divisor `dsor` giving the remainder `rem` and quotient `quot`.

- - -
### +
( `n1|u1` `n2|u2` -- `n3|u3` )  
Add the top two stack values.

- - -
### +!
( `n|u` `aaddr` -- )  
Add `n|u` to the cell at `aaddr`.

- - -
### +FIELD name
( `addr` -- `addr'` )  
Define unaligned structure field `name`, which when executed adds the field offset to `addr` giving `addr'`.

        Structure name defined last:

          0                         \ initial total byte count
            1 CELLS +FIELD p.x      \ single cell field named p.x
            1 CELLS +FIELD p.y      \ single cell field named p.y
          CONSTANT point            \ save structure size

        Structure name defined first:

          BEGIN-STRUCTURE point     \ create the named structure
            1 CELLS +FIELD p.x      \ A single cell filed named p.x
            1 CELLS +FIELD p.y      \ A single cell field named p.y
          END-STRUCTURE

- - -
### +LOOP
( `step` -- )  
Add `step` to the loop index.  If the loop index did not cross the boundary between the loop limit minus one and the loop limit, continue execution at the beginning of the loop.  Otherwise, end the current loop and continue execution immediately following the loop.

        : SOMEWORD ... limit first DO ... step +LOOP ... ;

- - -
### ,
( `x` -- )  
//Align// and reserve one cell of data-space and store `x` there.

- - -
### -
( `n1|u1` `n2|u2` -- `n3|u3` )  
Subtract the top two stack values.

- - -
### -TRAILING
( `caddr` `u` -- `caddr` `u'` )  
Trim trailing spaces from end of string `caddr` `u`.

- - -
### .
( `n` -- )  
Display `n` in free field format.

- - -
### ." ccc"
( `ccc<quote>` -- )  
Display `ccc`.

- - -
### .( ccc)
( `ccc<paren>` -- ) immediate  
Parse and display text until a closing parenthesis.

- - -
### /
( `dend` `dsor` -- `quot` )  
Divide the dividend `dend` by the divisor `dsor` leaving the symmetric quotient `quot` on top of the stack.  This is the same as the `SM/REM` quotient.

- - -
### /MOD
( `dend` `dsor` -- `rem` `quot` )  
Divide the dividend `dend` by the divisor `dsor` leaving the remainder `rem` and quotient `quot` on top the stack.  This is the same as the `SM/REM` remainder.

- - - 
### /STRING
( `caddr` `u` `n` -- `caddr'` `u'` )  
Adjust the character string at `caddr u` by offset `n` characters, by adding `n` to `caddr` and subtracting `n` from length `u`.

- - -
### 0<
( `n` -- `bool` )  
`bool` is true if and only if `n` is less than zero (0).

- - -
### 0<>
( `nu` -- `bool` )  
`bool` is true if `nu` is not equal to zero (0).

- - -
### 0>
( `nu` -- `bool` )  
`bool` is true if `nu` is greater to zero (0).

- - -
### 0=
( `x` -- `bool` )  
`bool` is true if and only if `x` is equal to zero (0).

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
`x2` is the result of shifting `x1` one bit toward the most-significant bit, filling the vacated least-significant bit with zero (0).

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
( `x1` `x2` `x3` `x4` -- `x1` `x2` `x3` `x4` `x1` `x2` )  
Copy cell pair `x1 x2` to the top of the stack.

- - -
### 2SWAP
( `x1` `x2` `x3` `x4` -- `x3` `x4` `x1` `x2` )  
Exchange the top two cell pairs.

- - -
### : name ...
Start definition of word `name`.  The current definition shall not be findable in the dictionary until it is ended (or until the execution of `DOES>`).

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
Initialise the pictured numeric output conversion process.

- - -
### =
( `x1` `x2` -- `bool` )  
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
Move top of the data stack to the return stack.

- - -
### ?
( `aaddr` -- )  
Display the value stored at `aaddr`.

- - -
### ?DO
( `n1|u1` `n2|u2` -- ) (R: -- `loop-sys` )  
Mark the start of `?DO ... +LOOP` or `?DO ... LOOP`.

- - -
### ?DUP
( `x` -- `x` `x` )  
Duplicate `x` if it is non-zero.

- - -
### ?LEAVE
( -- )  

        : SOMEWORD ... limit first DO ... test ?LEAVE ... LOOP ... ;

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
If `x1` is not equal to zero (0), display the message that follows up to the closing double-quote and perform an `ABORT`.

- - -
### ABS
( `n` -- `u` )  
Return the absolute value of `n`.

- - -
### ACTION-OF name
( `<spaces>name` -- `xt` )  
Return the execution token `xt` of a deferred `name`.  See `DEFER`.

- - -
### ACCEPT
( `caddr` `+n1` -- `+n2` )  
Accept up to `+n1` (between 1 and +32767) characters into the buffer given by `caddr`; the number of characters actually received, `+n2`, is pushed.

- - -
### AGAIN
( -- )  
Loop back to matching `BEGIN` indefinitely.

        BEGIN
            \ loop body
        AGAIN

- - -
### AHEAD
( -- )  
Jump ahead over a block of code.  A building block for several flow control words.

        AHEAD
            \ not executed
        THEN
        \ continue

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
If `n` is greater than zero (0), reserve `n` address units of data space.  If `n` is less than zero (0), release `|n|` address units of data space.  If `n` is zero (0), leave the data-space pointer unchanged.

- - -
### AND
( `x1` `x2` -- `x3` )  
Bit-wise and of the top two stack values.

- - -
### AT-XY
( `col` `row` -- )  
Position cursor on the terminal, `row` and `col` are 0 based.

- - -
### BASE
( -- `aaddr` )  
`aaddr` is the address of a cell containing the current number conversion radix, between 2..36.

- - -
### BEGIN
( -- )  
Mark the start of `BEGIN ... AGAIN` or `BEGIN ... WHILE ... REPEAT` loops.

- - -
### BEGIN-STRUCTURE name
( `<spaces>name` -- `addr` `0` )  

- - -
### BL
( -- `char` )  
`char` is the character value for a space, since Forth words are delimited by space.  A space can also be specified with `'\s'`.

- - -
### BLK
( -- `aaddr` )  
`aaddr` is the address of a cell containing zero (0) or the number of the mass-storage block being interpreted.  If `BLK` contains zero (0), the input source is not a block and can be identified by `SOURCE-ID`.  If a program alters `BLK`, the results are undefined.

- - -
### BLOCK
( `blk_num` -- `aaddr` )  
`BLOCK` assigns a block buffer, `aaddr`, writing the old contents to mass storage if dirty, before reading the new contents of block `blk_num` from mass storage.   The block buffer pointed to by `aaddr` is now the current block buffer assigned to `blk_num`.

- - -
### BUFFER
( `blk_num` -- `aaddr` )  
`BUFFER` assigns a block buffer, `aaddr`, writing the old contents to mass storage if dirty, but does not read anything.  The block buffer pointed to by `aaddr` is now the current block buffer assigned to `blk_num`.

- - -
### C!
( `char` `caddr` -- )  
Store `char` at `caddr`.

- - -
### C" ccc"
( `ccc<quote>` -- `caddr` )  
When interpreting, copy the string `ccc` as-is to a transient buffer and return counted string `caddr`.  When compiling, append the string `ccc` as-is to the current word so when executed it leaves the counted string `caddr` on the stack.  See also `S"`.

- - -
### C,
( `char` -- )  
Reserve one character of data space and store `char` there.

- - -
### C@
( `caddr` -- `char` )  
Fetch from `caddr` the character `char` stored there.

- - -
### CASE
( -- )  

        CASE
            test1 OF ... ENDOF
            ...
            testN OF ... ENDOF
            default action
        ENDCASE

- - -
### CATCH
( `i*x` `xt` -- `j*x` `0` | `i*x` `n` )  
Push an exception frame on the exception stack and then execute the execution token `xt` (as with `EXECUTE`) in such a way that control can be transferred to a point just after `CATCH` if `THROW` is executed during the execution of `xt`.

If the execution of `xt` completes normally (ie. the exception frame pushed by this `CATCH` is not popped by an execution of `THROW`) pop the exception frame and return zero (0) on top of the data stack, above whatever stack items would have been returned by `xt EXECUTE`.  Otherwise, the remainder of the execution semantics are given by `THROW`.

- - -
### CELL+
( `aaddr1` -- `aaddr2` )  
Add the size in address units of a cell to `aaddr1` giving `aaddr2`.

- - -
### CELLS
( `n1` -- `n2` )  
`n2` is the size in address units of `n1` cells.

- - -
### CFIELD: name
( `addr` -- `addr'` )  
Define character structure field `name`, which when executed adds the field offset to `addr` giving `addr'`.

- - -
### CHAR ccc
( `<spaces>ccc` -- `char` )  
Parse `text` placing the first character of text on the stack.

- - -
### CHAR+
( `caddr1` -- `caddr2` )  
Add the size in address units of a character.

- - -
### CHARS
( `n1` -- `n2` )  
`n2` is the size in address units of `n1` characters.

- - -
### CMOVE
( `src` `dst` `u` -- )  

- - -
### CMOVE>
( `src` `dst` `u` -- )  

- - -
### COMPARE
( `caddr1` `u1` `caddr2` `u2` -- `n` )  
Compare the two strings lexicographically.  Return `n` as 1, zero (0), or -1, according to whether the string `caddr1` `u1` is greater than, equal to, or less than the string `caddr2` `u2`.

- - -
### COMPILE,
( -- )  

- - -
### CONSTANT name
( `x` `<spaces>name` -- )  
Define the word `name` to represent the constant value `x`.  When `name` is executed, the value `x` is pushed on to the stack.

- - -
### COUNT
( `caddr1` -- `caddr2` `u` )  
Return the character string `caddr2 u` specification for the counted string `caddr1`.

- - -
### CR
( -- )  
Write a newline to standard output.

- - -
### CREATE name
( `<spaces>name` -- )  
Create a new word definition.  When `name` is executed, the `aaddr` of its data space is pushed onto the stack.  `CREATE` can be used to define new data structure, eg.

        \ Define how to make an array.
        : array ( n "<spaces>name" -- ) CREATE CELLS ALLOT ;

        \ Define a new array of 3 cells.
        3 array fred

        \ Store 123 into fred[1].
        123 fred 1 CELLS + !

Or when combined with `DOES>` make new "defining" words, eg. 

        \ General structure:
        : word1 CREATE ( build data space of word2 ) DOES> ( actions applied to data of word2 ) ;

        \ Make a new defining word CONSTANT.
        : CONSTANT CREATE , DOES> @ ;

        \ Use CONSTANT to define a word representing a value.
        377 CONSTANT monaco

        \ Use the new word.
        monaco ( -- 377 )

- - -
### CS-PICK
( `xu` `xu-1` ... `x1` `x0` `u` -- `xu` `xu-1` ... `x1` `x0` `xu` )  
Remove `u`.  Copy the `xu` to the top of the control-stack.  `0 CS-PICK` equivalent to `DUP`, `1 CS-PICK` equivalent to `OVER`.

- - -
### CS-ROLL
( `xu` `xu-1` ... `x0` `u` -- `xu-1` ... `x0` `xu` )  
Left rotate the control-stack `u` cells.

- - -
### DECIMAL
( -- )  
Set the numeric conversion radix to ten (decimal).

- - -
### DEFER name
( `<spaces>name` -- )  
Create a definition for `name`, which when executed will execute the saved execution token; default `ABORT`.  (Think indirect word execution.)

        ok DEFER foo
        ok foo
        -1 thrown: ABORT
        ok : hello S\" Hello world! woot woot\n" TYPE ;
        ok ' hello IS foo
        ok foo
        Hello world! woot woot
        ok ACTION-OF foo EXECUTE
        Hello world! woot woot
        ok

- - -
### DEFER!
( `xt2` `xt1` -- )  
Set the deferred word `xt1` to execute `xt2` when called.

- - -
### DEFER@
( `xt1` -- `xt2` )  
Get the execute token `xt2` stored by deferred word `xt1`.

- - -
### DEPTH
( -- `u` )  
Number of cells on the data stack before `u` was placed there.

- - -
### DO
( `n1|u1` `n2|u2` -- ) (R: -- `loop-sys` )  
Mark the start of `DO ... +LOOP` or `DO ... LOOP`.

- - -
### DOES>
Define the execution semantics for the most recently defined word by `CREATE`.  Throws -31 if `DOES>` is applied to a word not defined by `CREATE`.

        \ General structure:
        : word1 CREATE ( build data space of word2 ) DOES> ( actions applied to data of word2 ) ;

        \ Make a new defining word CONSTANT.
        : CONSTANT CREATE , DOES> @ ;

        \ Use CONSTANT to define a word representing a value.
        377 CONSTANT monaco

        \ Use the new word.
        monaco ( -- 377 )

- - -
### DROP
( `x` -- )  
Remove the top of the stack.

- - -
### DUMP
( `addr` `u` -- )  
Display the contents of `u` consecutive addresses starting at `addr`.

- - -
### DUP
( `x` -- `x` `x` )  
Duplicate `x`.

- - -
### ELSE
( -- )

        test IF
            \ execute for true
        ELSE
            \ execute for false
        THEN
        \ continue

- - -
### EMIT
( `char` -- )  
Write the character octet to standard output.

- - -
### EMPTY-BUFFERS
( -- )  
Mark all block buffers as free without saving any dirty buffers.

- - -
### END-STRUCTURE
( `addr` `0` -- )  
Terminate definition of a structure started by `BEGIN-STRUCTURE`.

- - -
### ENDCASE
( `x` --- )  
Discard the case selector `x` and continue execution.

        CASE
            test1 OF ... ENDOF
            ...
            testN OF ... ENDOF
            default action
        ENDCASE

- - -
### ENDOF
( -- )  

        CASE
            test1 OF ... ENDOF
            ...
            testN OF ... ENDOF
            default action
        ENDCASE

- - -
### EVALUATE
( `i*x` `caddr` `u` -- `j*x` )  
Save the current input source specification and make the string described by `caddr` and `u` both the input source and buffer, reset >IN to zero (0), and interpret.  When the parse area is empty, restore the prior input source specification.

- - -
### EXECUTE
( `i*x` `xt` -- `j*x` )  
Remove the execution token `xt` from the stack and perform the semantics identified by it. 

- - -
### EXIT
( -- )(R: `ip` -- )  
Return control from the currently executing word to its caller.  Before executing `EXIT` within a do-loop, a program shall discard the loop-control parameters by executing `UNLOOP`.

- - -
### FALSE
( -- `false` )  
Return false value, equivalent to zero (0).

- - -
### FIELD: name
( `addr` -- `addr'` )  
Define cell aligned structure field `name`, which when executed adds the field offset to `addr` giving `addr'`.

- - -
### FILL
( `caddr` `u` `char` -- )  
Fill memory at address `caddr` with `u` consecutive characters `char`.

- - -
### FLUSH
( -- )  
Save and free all dirty block buffers.

- - -
### FM/MOD
( `dend` `dsor` -- `mod` `quot` )  
Floored division of the dividend `dend` by the divisor `dsor` leaving the modulus `mod` and quotient `quot`.  In floored division the modulus `mod` carries the sign of the divisor `dsor`.

        Dividend Divisor Remainder Quotient
            10       7         3        1
           -10       7         4       -2
            10      -7        -4       -2
           -10      -7        -3        1

- - -
### HERE
( -- `addr` )  
`addr` is the data-space pointer that will next be assigned by `,`, `ALIGN`, `ALLOT`, `C,`, `COMPILE,`.  See `RESERVE` below.

- - -
### HEX
( -- )  
Set the numeric conversion radix to sixteen (hexdecimal).

- - -
### HOLD
( `char` -- )  
Append `char` to the picture numeric output string.

- - -
### HOLDS
( `caddr` `u` -- )
Append string `caddr` `u` to the pictured numeric output string.

- - -
### I
( -- `n|u` )  
`n|u` is a copy of the current (innermost) loop index.

- - -
### IF
( `bool` -- )  
If `bool` is zero (0) (`FALSE`), continue execution following the matching `ELSE` or `THEN`.

        test IF 
            \ execute for true
        THEN
        \ continue

        test IF
            \ execute for true
        ELSE
            \ execute for false
        THEN
        \ continue

- - -
### IMMEDIATE
( -- )  
Make the most recent definition an immediate word.

- - -
### INCLUDE filepath
( `<spaces>filepath` --  )  

- - -
### INCLUDED
( `caddr` `u` -- )  

- - -
### INVERT
( `x1` -- `x2` )  
Take the one's complement of `x1`.

- - -
### IS name
( `xt` `<spaces>name` -- )  
Set deferred word `name` to the execution token `xt`.  See `DEFER`.

- - -
### J
( -- `n|u` )  
`n|u` is a copy of the next-outer loop index.

- - -
### KEY
( -- `char` )  
Receive one character `char`.  Characters received by `KEY` are not displayed.

- - -
### LEAVE
( -- ) (R: `loop-sys` -- )  
Leave the current loop, resume execution following the `+LOOP` or `LOOP`.

- - -
### LIST
( `blk_num` -- )  
List the block given by `blk_num`, 1 based, and save `blk_num` in `SCR` on success.

- - -
### LITERAL
( `x` -- )  
Compile `x` into the definition so that it is later pushed onto the stack during execution of the definition.

         : SOMEWORD ... [ x ] LITERAL ... ;

- - -
### LOAD
( `i*x` `blk_num` -- `j*x` )  
Save the current input-source specification.  Store `blk_num` in `BLK` (thus making block `blk_num` the input source and setting the input buffer to encompass its contents), set `>IN` to zero (0), and interpret.  When the parse area is exhausted, restore the prior input source specification.  Other stack effects are due to the words interpreted.

- - -
### LOOP
Add `n` to the loop index.  If the loop index did not cross the boundary between the loop limit minus one and the loop limit, continue execution at the beginning of the loop.  Otherwise, end the current loop and continue execution immediately following the loop.

        : SOMEWORD ... limit first DO ... LOOP ... ;

Or

        : SOMEWORD ... limit first ?DO ... LOOP ... ;

- - -
### LSHIFT
( `x1` `u` -- `x2`)  
Logical left shift of `x1` by `u` bits, putting `u` zeroes into the least significant bits.

- - -
### MARKER name
( `<spaces>name` -- )  
Create `name` in the dictionary, which when executed will remove `name` and restore the dictionary to the state it had just prior to the `name` being added.

- - -
### MAX
( `n1` `n2` -- `n3` )  
`n3` is the greater of `n1` and `n2`.

### MIN
( `n1` `n2` -- `n3` )  
`n3` is the lesser of `n1` and `n2`.

- - -
### MOD
( `dend` `dsor` -- `rem` )  
Divide the dividend `dend` by the divisor `dsor` leaving the remainder `rem`.

- - -
### MOVE
( `src` `dst` `u` -- )  
If `u` is greater than zero (0), copy from `src` the contents of `u` consecutive address units to `dst`.

- - -
### MS
( `u` -- )  
Wait at least `u` milliseconds.

- - -
### NEGATE
( `n1` -- `n2` )  
Negate `n1`, giving its arithmetic inverse `n2`.

- - -
### NIP
( `x1` `x2` -- `x2` )  
Drop the first item below the top of stack.

- - -
### N>R
( `i*x` `n` –– ) (R: –– `i*x` `n` )

- - -
### NR>
( –– `i*x` `n` ) (R: `i*x` `n` –– )

- - -
### OF
( `x1` `x2` --  | `x1` )  

        CASE
            test1 OF ... ENDOF
            ...
            testN OF ... ENDOF
            default action
        ENDCASE

- - -
### OR
( `x1` `x2` -- `x3` )  
Bit-wise or of the top two stack values.

- - -
### OVER
( `x1` `x2` -- `x1` `x2` `x1` )  
Copy the second value `x1` below the stack to the top.

- - -
### PAD
( -- `aaddr` )  
A character buffer space available to developers and //not// used by standard words.

- - -
### PAGE
( -- )  
Clear the terminal (advance next page).

- - -
### PARSE
( `char` `ccc<char>` -- `caddr` `u` )  
Parse `ccc` delimited by the delimiter `char`.  `caddr` and `u` are the address and length within the input buffer of the parsed string.  If the parse area was empty, the resulting string has a zero (0) length.

- - -
### PARSE-NAME name
( `<spaces>name<space>` -- `caddr` `u` )  
Skip leading space delimiters. Parse name delimited by a space.

- - -
### PICK
( `xu` `xu-1` ... `x1` `x0` `u` -- `xu` `xu-1` ... `x1` `x0` `xu` )  
Remove `u`.  Copy the `xu` to the top of the stack.  `0 PICK` equivalent to `DUP`, `1 PICK` equivalent to `OVER`.

- - -
### POSTPONE name
( `<spaces>name` --  )  
Parse and find `name` appending the compilation semantics of `name` to the current definition.  For an immediate word, it is compiled into the definition, instead of executed.  Otherwise compile the word during the definition of another word.

An example:

        : [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE

- - -
### QUIT
( -- )(R: `i*x` -- )  
Empty the return stack, reset `SOURCE-ID` to zero (0), set the console as the input source, enter interpretation state, and start REPL:

  * Accept a line from the input source into the input buffer, set `>IN` to zero (0), and interpret.
  * Display the prompt if in interpretation state, all processing has been completed, and no ambiguous condition exists.

- - -
### R>
( -- `x` )(R: `x` -- )  
Move top of the return stack to the data stack.

- - -
### R@
( -- `x` )(R: `x` -- `x` )  
Copy top of return stack to the data stack.

- - -
### RECURSE
( -- )  
Call the current definition.  The behaviour of `RECURSE` following a `DOES>` is undefined.

        : FACTORIAL ( +n1 -- +n2)
            DUP 2 < IF DROP 1 EXIT THEN DUP 1- RECURSE *
        ;

- - -
### REPEAT
( -- )  
Loop back to matching `BEGIN`.

        BEGIN
            \ test expression
        WHILE
            \ loop body while true
        REPEAT
        \ continue once test is false

- - -
### REFILL
( -- `bool` )  
Attempt to fill the input buffer from the input source, returning a true if successful.

When the input source is the user input device, attempt to receive input into the terminal input buffer.  If successful, make the result the input buffer, set `>IN` to zero (0), and return true.  Receipt of a line containing no characters is considered successful.  If there is no input available from the current input source, return false.

When the input source is a string from `EVALUATE`, return false and perform no other action.

When the input source is a block, make the next block the input source and current input buffer by adding one to the value of `BLK` and setting `>IN` to zero (0).  Return true if the new value of `BLK` is a valid block number, otherwise false.

- - -
### ROLL
( `xu` `xu-1` ... `x0` `u` -- `xu-1` ... `x0` `xu` )  
Left rotate the stack `u` cells.

- - -
### ROT
( `x1` `x2` `x3` -- `x2` `x3` `x1` )  
Rotate the top three stack entries.

- - -
### RSHIFT
( `x1` `u` -- `x2`)  
Logical right shift of `x1` by `u` bits, putting `u` zeroes into the most significant bits.

- - -
### S" ccc"
( `ccc<quote>` -- `caddr` `u` )  
When interpreting, copy the string `ccc` as-is to a transient buffer and return `caddr u`.  When compiling, append the string `ccc` as-is to the current word so when executed it leaves `caddr u` of the string on the stack.  Note as an extension strings are also `NUL` terminated to facilitate use of host environment functions.

- - -
### S\\" ccc"
( `ccc<quote>` -- `caddr` `u` )  
When interpreting, copy the escaped string `ccc` to a transient buffer and return `caddr u`.  When compiling, append the escaped string `ccc` to the current word so when executed it leaves `caddr u` of the string on the stack. Note as an extension strings are also `NUL` terminated to facilitate use of host environment functions.

- - -
### SAVE-BUFFERS
( -- )  
Save all the dirty block buffers to the block file.

- - -
### SCR
( -- `aaddr` )  
`aaddr` is the address of a cell containing the block number from the most recent `LIST`.

- - -
### SIGN
( `n` -- )  
If `n` is negative, add a minus sign to the beginning of the pictured numeric output string.  An ambiguous condition exists if `SIGN` executes outside of a `<# #>` delimited number conversion.

- - -
### SLITERAL
( `caddr` `u` -- )  
Compile the string `caddr` and length `u` into the definition so that it is later pushed onto the stack during execution of the definition.

        \ Simplistic version of S"
        : S"
            [CHAR] " PARSE POSTPONE SLITERAL
        ; IMMEDIATE

- - -
### SM/REM
( `dend` `dsor` -- `rem` `quot` )  
Symmetric division of the dividend `dend` by the divisor `dsor` leaving the remainder `rem` and quotient `quot`.  In symmetric division the remainder `rem` carries the sign of the dividend `dend`.

        Dividend Divisor Remainder Quotient
            10       7         3        1
           -10       7        -3       -1
            10      -7         3       -1
           -10      -7        -3        1

- - -
### SOURCE
( -- `caddr` `u` )  
Push the string address `caddr` and the number of characters `u` in the input buffer.

- - -
### SPACE
( -- )  
Display a space.

- - -
### SPACES
( `n` -- )  
If `n` is greater than zero (0), display `n` spaces.

- - -
### STATE
( -- `aaddr` )  
Return the `aaddr` address of the compilation state; true when compiling, otherwise false when interpreting.

- - -
### SWAP
( `x1` `x2` -- `x2` `x1` )  
Exchange the top two stack items.

- - -
### THRU
( `u1` `u2` -- )  
`LOAD` in sequence blocks `u1` through to `u2` inclusive.

- - -
### TIME&DATE
( -- `sec` `min` `hour` `day` `month` `year` )  
Time current local system time.  `sec` {0..59}, `min` {0..59}, `hour` {0..23}, `day` {1..31}, `month` {1..12}, `year` eg. 1970.

- - -
### THEN
( -- )
See `IF`.

        AHEAD
            \ not executed
        THEN
        \ continue

        test IF
            \ execute for true
        THEN
        \ continue

        test IF
            \ execute for true
        ELSE
            \ execute for false
        THEN
        \ continue

- - -
### THROW
( `k*x` `n` -- `k*x` | `i*x` `n` )  
If `n` is non-zero, pop the top most exception frame from the exception stack, along with everything on the return stack above that frame.  Then restore the input source specification in use before the corresponding `CATCH` and adjust the depths of all stacks so that they are the same as the depths saved in the exception frame (`i` is the same number as the `i` in the input arguments to the corresponding `CATCH`), put `n` on top of the data stack, and transfer control to a point just after the `CATCH` that pushed that exception frame.

If the top of the stack is non-zero and there is no exception frame on the exception stack the system may display an implementation-dependent message giving information about the condition associated with the `THROW` code `n`.  Subsequently, the system shall perform the function of `ABORT`.

- - -
### TO
( `i*x` `<spaces>name` -- )  

See `VALUE`.

- - -
### TRUE
( -- `true` )  
Return a true flag, a single-cell value with all bits set.

- - -
### TUCK
( `x1` `x2` -- `x2` `x1` `x2` )  
Copy the first (top) stack item below the second stack item.

- - -
### TYPE
( `caddr` `u` -- )  
If `u` is greater than zero (0), display the character string specified by `caddr` and `u`.

- - -
### U.
( `u` -- )  
Display u in free field format.

- - -
### U<
( `u1` `u2` -- `bool` )  
`bool` is true if and only if `n1` is less than `n2`.

- - -
### U>
( `u1` `u2` -- `bool` )  
`bool` is true if and only if `n1` is greater than `n2`.

- - -
### UM/MOD
( `dend` `dsor` -- `mod` `quot` )  
Divide `dend` by `dsor`, giving the quotient `quot` and the remainder `mod`. All values and arithmetic are unsigned.

- - -
### UNLOOP
( -- ) ( R: `loop-sys` -- )  
Drop the loop control for the current loop.  An `UNLOOP` is needed for each nesting level before a definition may leave by means of `EXIT`.

        limit first DO
            \ stuff
            \ test expression
            IF
                \ other stuff
                UNLOOP EXIT
            THEN
            \ more stuff
        LOOP
        \ continue

- - -
### UNUSED
( -- `u` )  
`u` is the amount of data space remaining in the region addressed by `HERE`, in address units.

- - -
### UPDATE
( -- )  
Mark the current block as dirty.

- - -
### UNTIL
( `x` -- )  
Loop back to `BEGIN` until `x` equals zero (0).

        BEGIN
            \ loop body
            \ test expression
        UNTIL

- - -
### VALUE name
( `x` `<spaces>name` -- )  
Create `name` with one cell of data assigned `x`.  When `name` is executed, the value `x` is pushed to the stack.  See `TO`.

        69 VALUE position
        : change ( new -- old ) position SWAP TO position ;

- - -
### VARIABLE name
( `<spaces>name` -- )  
Create `name` with one cell of data.  When `name` is executed push the `aaddr` of the data cell.

        VARIABLE name           \ define name
        123 name !              \ store value to variable
        name @                  \ fetch value from variable

- - -
### WHILE
( `x` -- )  
If `x` equals zero (0), continue execution following `REPEAT`.

        BEGIN
            \ test expression
        WHILE
            \ loop body while true
        REPEAT
        \ continue

- - -
### WORD
( `char` `<chars>ccc<char>` -- `caddr` )  
Skip leading delimiters.  Parse characters `ccc` delimited by `char`.

- - -
### XOR
( `x1` `x2` -- `x3` )  
Bit-wise exclusive-or of the top two stack values.

- - -
### [
( -- ) immediate
Enter interpretation state.

- - -
### ['] name
( `<spaces>name` -- `xt` )  
Place name's execution token xt on the stack.

- - -
### [CHAR] ccc
( `<spaces>ccc` -- `char` )  
Place char, the value of the first character of name, on the stack.

- - -
### [DEFINED]
( `<spaces>name` -- `bool` ) immediate  
Return `TRUE` if `name` is word that can be found in the dictionary; otherwise `FALSE`.


- - -
### [ELSE]
( `<spaces>name ...` -- ) immediate  
Discard space delimited words, including nested `[IF]...[THEN]` and `[IF]...[ELSE]...[THEN]`, until the matching (same level) `[THEN]` is found.

- - -
### [IF]
( `bool` | `bool` `<spaces>name ...` -- ) immediate  
If `bool` is false then discard space delimited words, including nested `[IF]...[THEN]` and `[IF]...[ELSE]...[THEN]`, until the  matching (same level) `[ELSE]` or `[THEN]` is found.

- - -
### [THEN]
( -- ) immediate  
End conditional source block.

- - -
### [UNDEFINED]
( `<spaces>name` -- `bool` ) immediate  
Return `FALSE` if `name` is word that can be found in the dictionary; otherwise `TRUE`.

- - -
### \\ ccc<LF>
( `ccc<LF>` -- ) immediate  
Parse and discard the remainder of the parse area.

- - -
### ]
( -- )  
Enter compilation state.

- - -

Post4 Specific Words
--------------------

### .rs
( -- )  
Dump the return stack.

- - -
### /char
( -- `u` )  
Size of an character in octets.

- - -
### /cell
( -- `u` )  
Size of a cell in octets.

- - -
### /counted-string
( -- `u` )  
Maximum size of a counted string in characters.  This is a deviation from `ENVIRONMENT?` queries.

- - -
### /hold
( -- `u` )  
Size of a numeric picture buffer in characters.  This is a deviation from `ENVIRONMENT?` queries.

- - -
### /pad
( -- `u` )  
Size of a pad buffer in characters.  This is a deviation from `ENVIRONMENT?` queries.

- - -
### >here
( -- `u` )  
Offset into the current data-space for the word being compiled.  Similar to the word `HERE`, except expressed as an offset from the start of the data-space when the current word was created.  During the compiliation of a word in C based implementations, the data-space region may be relocated when its enlarged by `,`, `ALIGN`, `ALLOT`, `C,`, `COMPILE,` thus invalidating previous values of `HERE` on the stack.  Providing an offset into the current data-region allows for computing relative locations.

- - -
### args
( -- `argv` `argc` )  
Return the number of arguments on the command line `argc` and the NULL terminated array of C string pointers `argv`.  See example `dumpargs.p4`.

- - -
### address-unit-bits
( -- `u` )  
Size of one address unit in bits.  This is a deviation from `ENVIRONMENT?` queries.

- - -
### blocks
( -- `u` )  
Number of blocks `u` currently in the block file, one through to `u`.  The block file can be extended by writing to block `u`, the file will be extended with intervening blank blocks from the current end upto but not including block `u`, which the actual write will fill.

- - -
### bye_code
( exit_code -- )  
Terminate and return to the host OS an exit code; zero (0) for normal/success, non-zero an error occurred.

- - -
### c\\" ccc"
( `ccc<quote>` -- `caddr` )  
When interpreting, copy the escaped string `ccc` to a transient buffer and return counted string `caddr`.  When compiling, append the escaped string `ccc` to the current word so when executed it leaves the counted string `caddr` on the stack.  See also `S\"`.

### char-
( `caddr1` -- `caddr2` )  
Subtract the size in address units of a character.

- - -
### cputs
( caddr -- )  
Print the counted string.  See also `puts`.

- - -
### epoch-seconds
( -- `u` )  
System clock time in seconds from the epoch.

- - -
### env
( `key` `k` -- `value` `v` )  
Lookup the environment variable string `key` `k`.  Return string `value` `v`; if length `v` is `-1`, then the environment variable `key` was not found and `value` is invalid.

        S" HOME" env puts CR
        S" USER" env puts CR

- - -
### floored
( -- `false` )  
True if floored division is the default.  This is a deviation from `ENVIRONMENT?` queries.

- - -
### list+
( -- )  
Increment variable `SCR` and list next block.

- - -
### llor
( `xu` `xu-1` ... `x0` `u` -- `x0` `xu` `xu-1` ... `x1` )  
Right rotate the stack `u` cells; `ROLL` in the opposite direction.

- - -
### max-char
( -- `u` )  
Maximum value of any character.  Currently Post4 only supports ASCII and addressable units are octets.  This is a deviation from `ENVIRONMENT?` queries.

- - -
### max-n
( -- `u` )  
Largest usable signed integer.  This is a deviation from `ENVIRONMENT?` queries.

- - -
### max-u
( -- `u` )  
Largest usable unsigned integer.  This is a deviation from `ENVIRONMENT?` queries.

- - -
### octal
( -- )  
Set the numeric conversion radix to eight (octal).

- - -
### parse-escape
( `char` `ccc<char>` -- caddr u )  
Parse `ccc` delimited by the delimiter `char`.  `caddr` and `u` are the address and length within the input buffer of the parsed C-style escaped string.  If the parse area was empty, the resulting string has a zero (0) length.  Supported escapes:

        \?          delete
        \\          backslash
        \a          bell
        \b          backspace
        \e          escape
        \f          formfeed
        \n          linefeed
        \r          carriage-return
        \s          space
        \t          tab
        \v          vertical tab
        \0          nul

Backslash followed by any other character escapes that character, ie. `\\` is a literal backslash, `\"` is a literal double quote.  Note that alpha-numeric characters are reserved for future extensions.

- - -
### puts
( caddr -- )  
Print a NUL terminated string.

        CREATE greet S\" Hello world.\n"
        greet drop puts

- - -
### reserve

( `n` -- `addr` )  
Similar to `ALLOT`, reserve `n` address-units of data-space and return its start address.  While defining a word in C based implementations, like Post4, data-space regions may be relocated when they are enlarged, thus invalidating previous values of `HERE`.  Therefore:

        HERE 100 CELLS ALLOT

Should `ALLOT` enlarge and relocate the data-space, the address saved by `HERE` on the stack will now point into invalid memory.  With `reserve` the address of the region just reserved is on top of the stack insuring that the address is valid until the next enlargement of the data-space by `reserve`,`,`, `ALIGN`, `ALLOT`, `C,`, or `COMPILE,`.

- - -
### strcmp
( `caddr1` `u1` `caddr2` `u2` -- `n` )  
Compare the two strings lexicographically.  Return `n` greater than, equal to, or less than zero (0), according to whether the string `caddr1` `u1` is greater than, equal to, or less than the string `caddr2` `u2`.

- - -
### strlen
( caddr -- u )  
String length of NUL terminated string.  

- - -
### _bp
( -- )  
Breakpoint.

- - -
### _branch
( -- )  
Branch relative.  The integer that immediately follows is the relative distance in address units from the integer's address.  Used in the definition of flow control words, like `AGAIN` and `AHEAD`, `ELSE`, `REPEAT`.

- - -
### _branchz
( `bool` -- )  
Branch relative if zero (0).  The integer that immediately follows is the relative distance in address units from the integer's address.  Used in the definition of flow control words, like `IF`, `WHILE`, `UNTIL`.

- - -
### _call
( -- )  
Call relative.  The integer that immediately follows is the relative distance in address units from the integer's address.  Used in the definition of flow control words, like `RECURSE`.

- - -
### _ds
( -- `aaddr` `n` )  
Push the data stack base address and current depth.  This stack is a fixed size and grows upward.

- - -
### _ds_size
( -- `n`)  
Push the data stack's size.

- - -
### _dsp!
( `aaddr` -- )  
Store `addr` into the data stack pointer.

- - -
### _dsp@
( -- `aaddr` )  
Fetch the data stack pointer.

- - -
### _is_immediate
( `xt` -- `bool` )  
Return true if the execution token is for an immediate word.

- - -
### _longjmp
( `n` -- )  
Return to the context saved at the start of the REPL (`QUIT`) passing `n`.  Values of `n` from -1 to -255 are the Forth 200x standard `THROW` codes.  Passing -256 is equivalent to `BYE`.

- - -
### _rs
( -- `aaddr` `n` )  
Push the return stack base address and current depth.  This stack is a fixed size and grows upward.

### _rs_size
( -- `n`)  
Push the return stack's size.

- - -
### _rsp!
( `aaddr` -- )  
Store `addr` into the return stack pointer.

- - -
### _rsp@
( -- `aaddr` )  
Fetch the return stack pointer.

- - -
### _stack_dump
( `aaddr` `u` -- )  
Utility word used to define `.S` and `.rs`.

- - -

## THROW Codes Used

This is a list of `THROW` codes used internally by Post4.

* -1 `ABORT`  
* -2 `ABORT"`  
* -3 stack overflow  
* -4 stack underflow  
* -5 return stack overflow  
* -6 return stack underflow  
* -9 invalid memory address (`SIGSEGV`)  
* -13 undefined word  
* -14 interpreting a compile-only word  
* -17 pictured numeric output string overflow  
* -21 unsupported operation  
* -23 address alignment exception (`SIGBUS`)  
* -24 invalid numeric argument
* -28 user interrupt (`SIGINT`)  
* -29 compiler nesting  
* -31 word not defined by `CREATE`
* -33 block read exception
* -34 block write exception
* -35 invalid block number, such as zero (0)
* -55 floating-point unidentified fault (`SIGFPE`)  
* -56 `QUIT`  
* -61 `ALLOT` or `RESIZE`

- - -


References
----------

Forth 200x  
<http://www.forth200x.org/>

Forth Standard  
<https://forth-standard.org/>

