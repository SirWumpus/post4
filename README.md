Post4 (Post-Forth)
==================

Copyright 2007, 2019 Anthony Howe.  All rights reserved.


Overview
--------

Post4 (`p4`) is a hosted indirect threaded Forth dialect written in C, based on the ["Forth 200x Draft 18.1, 2018-08-27"](http://www.forth200x.org/documents/forth18-1.pdf).  Post4 aims to implement the fewest possible built-in words in C, those that are needed to interact with memory and I/O, leaving the remaining standard words to be implemented in Forth.

```lang=shell
usage: p4 [-V][-b file][-c file][-d size][-r size] [script [args ...]]

-b file         block file; default ./.p4.blk or $HOME/.p4.blk
-c file         word definition file; default p4.p4
-d size         data stack size in cells; default 32
-r size         return stack size in cells; default 32
-V              build and version information
```

The environment variable `POST4_PATH` provides a colon separated search path for the `p4.p4` core word definitions file.  If `POST4_PATH` is undefined, then an OS specific default path is used.  A specific word definition file can be specified with `-c`.

By default a user block file, `.p4.blk`, is opened from the current directory or user's `HOME` directory.  This can be overridden with the `-b` option.

`p4` reads input from standard input and writes to standard output, which can be redirected:

```lang=shell
echo "123 69 + ." | p4
```


Examples
--------

### ed.p4 - Block Editor

There are actually three block editor word sets:

1. An `ed(1)` like block line editor using `LIST`, `PRINT`, and `CHANGE`.
2. An interactive single line editor using `EDIT`; left & right cursor keys, tab toggles insert or replace mode, delete backspace, ESC quits.
3. A full "screen" block editor using `ED`; all the commands of the interactive single line editor, plus up & down cursor keys, `CTRL+G` goto block, `CTRL+P` & `CTRL+N` block, and `ESC` menu.

NOTE: that `EDIT` and `ED` are hard coded with ANSI terminal escape sequences.

```lang=shell
$ p4
ok INCLUDE ed.p4
ok ED
```

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

```lang=shell
$ p4 life.p4
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
( `<spaces>name` -- `xt` )  
Find `name` and place its execution token on the stack.  Throw undefined word (-13) if not found.

- - -
### ( ccc)
( `ccc<paren>` -- ) immediate  
Parse and ignore characters up to the closing right parenthesis.

- - -

### \*
( `n1|u1` `n2|u2` -- `n3|u3` )  
Multiply the top two stack values.

- - -

### +
( `n1|u1` `n2|u2` -- `n3|u3` )  
Add the top two stack values.

- - -

### +!
( `n|u` `aaddr` -- )  
Add `n|u` to the cell at `aaddr`.

- - -

### ,
( `x` -- )  
//Align// and reserve one cell of data-space and store `x` there.

- - -

### -
( `n1|u1` `n2|u2` -- `n3|u3` )  
Subtract the top two stack values.

- - -
### .
( `n` -- )  
Display `n` in free field format.

- - -
### ." ccc"
( `ccc<quote>` -- )  
Display `ccc`.

- - -

### /
( `dend` `dsor` -- `quot` )  
Divide the dividend `dend` by the divisor `dsor` leaving the symmetric quotient `quot` on top of the stack.  This is the same as the `SM/REM` quotient.

- - -

### /MOD
( `dend` `dsor` -- `rem` `quot` )  
Divide the dividend `dend` by the divisor `dsor` leaving the remainder `rem` and quotient `quot` on top the stack.  This is the same as the `SM/REM` remainder.

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
Move cell `x` from the data stack to the return stack.

- - -
### ?DO
( `n1|u1` `n2|u2` -- ) (R: -- `loop-sys` )  
Mark the start of `?DO ... +LOOP` or `?DO ... LOOP`.

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
If `x1` is not equal to zero, display the message that follows up to the closing double-quote and perform an `ABORT`.

- - -
### ABS
( `n` -- `u` )  
Return the absolute value of `n`.

- - -
### ACCEPT
( `caddr` `+n1` -- `+n2` )  
Accept up to `+n1` (between 1 and +32767) characters into the buffer given by `caddr`; the number of characters actually received, `+n2`, is pushed.

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
If `n` is greater than zero, reserve `n` address units of data space.  If `n` is less than zero, release `|n|` address units of data space.  If `n` is zero, leave the data-space pointer unchanged.

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
Mark the start of `BEGIN...AGAIN` or `BEGIN...WHILE...REPEAT` loop.

- - -
### BL
( -- `char` )  
`char` is the character value for a space, since Forth words are delimited by space.  A space can also be specified with `'\s'`.

- - -
### BLK
( -- `aaddr` )  
`aaddr` is the address of a cell containing zero or the number of the mass-storage block being interpreted.  If `BLK` contains zero, the input source is not a block and can be identified by `SOURCE-ID`.  If a program alters `BLK`, the results are undefined.

- - -
### BLOCK
( `blk_num` -- `aaddr` )  

- - -
### BUFFER
( `blk_num` -- `aaddr` )  

- - -
### C!
( `char` `caddr` -- )  
Store `char` at `caddr`.

- - -
### C,
( `char` -- )  
Reserve one character of data space and store `char` there.

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
### CHAR ccc
( `<spaces>ccc` -- `char` )  
Parse `text` placing the first character of text on the stack.

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
### COMPILE,
( -- )  

- - -
### CONSTANT name
( `x` `<spaces>name` -- )  
Define the word `name` to represent the constant value `x`.  When `name` is executed, the value `x` is pushed on to the stack.

- - -
### CR
( -- )  
Write a newline to standard output.

- - -
### CREATE
( `<spaces>name` -- )  
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
### CS-PICK

- - -
### CS-ROLL

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
( `n1|u1` `n2|u2` -- ) (R: -- `loop-sys` )  
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
### DUMP
( `addr` `u` -- )  
Display the contents of `u` consecutive addresses starting at `addr`.

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
### EMPTY-BUFFERS
( -- )  
Mark all block buffers as free without saving any dirty buffers.

- - -
### EVALUATE
( `i*x` `caddr` `u` -- `j*x` )  
Save the current input source specification and make the string described by `caddr` and `u` both the input source and buffer, reset >IN to zero, and interpret.  When the parse area is empty, restore the prior input source specification.

- - -
### EXECUTE
( `i*x` `xt` -- `j*x` )  
Remove the execution token `xt` from the stack and perform the semantics identified by it. 

- - -
### EXIT
( -- )(R: `ip` -- )  
Return control from the currently executing word to its caller.  Before executing `EXIT` within a do-loop, a program shall discard the loop-control parameters by executing `UNLOOP`.

- - -
### FILL
( `caddr` `u` `char` -- )  
Fill memory at address `caddr` with `u` consecutive characters `char`.

- - -
### FLUSH
( -- )  
Save and free all dirty buffers.

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
`addr` is the data-space pointer that will next be assigned by `,`, `ALIGN`, `ALLOT`, `C,`, `COMPILE,`.

- - -
### HOLD
( `char` -- )  
Append `char` to the picture numeric output string.

- - -
### I
( -- `n|u` )  
`n|u` is a copy of the current (innermost) loop index.

- - -
### IF
( `bool` -- )  
If `bool` is zero (`FALSE`), continue execution following the matching `ELSE` or `THEN`.

```
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
```

- - -
### IMMEDIATE
( -- )  
Make the most recent definition an immediate word.

- - -
### INCLUDE filepath
( `"<spaces>filepath"` --  )  

- - -
### INCLUDED
( `caddr` `u` -- )  

- - -
### INVERT
( `x1` -- `x2` )  
Take the one's complement of `x1`.

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

- - -
### LOAD
( `blk_num` -- )  

- - -
### MOVE
( `src` `dst` `u` -- )  

- - -
### PICK

- - -
### ROLL
( `xu` `xu-1` ... `x0` `u` -- `xu-1` ... `x0` `xu` )  
Left rotate the stack `u` cells.

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
( `char` `ccc<char>` –– caddr u )  
Parse `ccc` delimited by the delimiter `char`.  `caddr` and `u` are the address and length within the input buffer of the parsed string.  If the parse area was empty, the resulting string has a zero length.

- - -
### POSTPONE word
( `"<spaces>word"` --  )  

- - -
### S" ccc"
( `ccc<quote>` -- `caddr` `u` )  
When interpreting, copy the string `ccc` as-is to a transient buffer and return `caddr u`.  When compiling, append the string `ccc` as-is to the current word so when executed it leaves `caddr u` of the string on the stack.

- - -
### S\\" ccc"
( `ccc<quote>` -- `caddr` `u` )  
When interpreting, copy the escaped string `ccc` to a transient buffer and return `caddr u`.  When compiling, append the escaped string `ccc` to the current word so when executed it leaves `caddr u` of the string on the stack.

- - -
### SAVE-BUFFERS
( -- )  
Save all the dirty buffer to the block file.

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
### UM/MOD
( `dend` `dsor` -- `mod` `quot` )  
Divide `dend` by `dsor`, giving the quotient `quot` and the remainder `mod`. All values and arithmetic are unsigned.

- - -
### UNLOOP
( -- ) ( R: `loop-sys` -- )  

- - -
### UNUSED
( -- `u` )  
`u` is the amount of data space remaining in the region addressed by `HERE`, in address units.

- - -
### UPDATE
( -- )  
Mark the current block as dirty.

- - -

Post4 Specific Words
--------------------

### .rs
( -- )  
Dump the return stack.

- - -
### ," ccc"
( `<spaces>ccc"` -- )  
Append NUL terminated string to the most recent word's data space.

```
CREATE greet ," Hello world.\n"
```

- - -
### /char
( -- `u` )  
Size of an address unit.

- - -
### /cell
( -- `u` )  
Size of a cell.

- - -
### /hold
( -- `u` )  
Size of a numeric picture buffer in characters.

- - -
### /pad
( -- `u` )  
Size of a pad buffer in characters.

- - -
### >here
( -- `u` )  
Offset into the current data-space for the word being compiled.  Similar to the word `HERE`, except expressed as an offset from the start of the data-space when the current word was created.  During the compiliation of a word in C based implementations, the data-space region may be relocated when its enlarged by `,`, `ALIGN`, `ALLOT`, `C,`, `COMPILE,` thus invalidating previous values of `HERE` on the stack.  Providing an offset into the current data-region allows for computing relative locations.

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
Branch relative if zero.  The integer that immediately follows is the relative distance in address units from the integer's address.  Used in the definition of flow control words, like `IF`, `WHILE`, `UNTIL`.

- - -
### _ds
( -- `aaddr` `n` )  
Push the data stack base address and current depth.  This stack is a fixed size (default 32 cells) and grows upward.

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
Fetch data stack pointer.

- - -
### _ip
( -- `aaddr` )  
Address of the cell holding the inner interpreter's instruction pointer.

- - -
### _is_immediate
( xt -- flag )  
Return true if the execution token is for an immediate word.

- - -
### _longjmp
( `n` -- )  
Return to the context saved at the start of the REPL (`QUIT`) passing `n`.  Values of `n` from -1 to -255 are the Forth 200x standard `THROW` codes.  Passing -256 is equivalent to `BYE`.

- - -
### _rs
( -- `aaddr` `n` )  
Push the return stack base address and current depth.  This stack is a fixed size (default 32 cells) and grows upward.

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
Fetch return stack pointer.

- - -
### _stack_dump
( `aaddr` `u` -- )  
Utility word used to define `.S` and `.rs`.

- - -
### bye_code
( exit_code -- )  
Terminate and return to the host OS an exit code; zero (0) for normal/success, non-zero an error occurred.

- - -
### list+
( -- )  
Increment variable `SCR` and list next block.

- - -
### llor

( `xu` `xu-1` ... `x0` `u` -- `x0` `xu` `xu-1` ... `x1` )  
Right rotate the stack `u` cells; `ROLL` in the opposite direction.

- - -
### reserve

( `n` -- `addr` )  
Similar to `ALLOT`, reserve `n` address-units of data-space and return its start address.  While defining a word in C based implementations, like `p4`, data-space regions may be relocated when they are enlarged, thus invalidating previous values of `HERE`.  Therefore:

```
HERE 100 CELLS ALLOT
```

Should `ALLOT` enlarge and relocate the data-space, the address saved by `HERE` on the stack will now point into invalid memory.  With `reserve` the address of the region just reserved is on top of the stack insuring that the address is valid until the next enlargement of the data-space by `reserve`,`,`, `ALIGN`, `ALLOT`, `C,`, or `COMPILE,`.

- - -
### strlen
( caddr -- u )  
String length of NUL terminated string.

- - -
### type0
( caddr -- )  
Print a NUL terminated string.

```
CREATE greet ," Hello world.\n"
greet TYPE0
```

- - -

## THROW Codes Used

This is a list of `THROW` codes used internally by `p4`.

* -1 `ABORT`  
* -2 `ABORT"`  
* -3 stack overflow  
* -4 stack underflow  
* -5 return stack overflow  
* -6 return stack underflow  
* -9 invalid memory address (SIGSEGV)  
* -13 undefined word  
* -14 interpreting a compile-only word  
* -17 pictured numeric output string overflow  
* -21 unsupported operation  
* -23 address alignment exception (SIGBUS)  
* -28 user interrupt (SIGINT)  
* -29 compiler nesting  
* -31 word not defined by `CREATE`
* -33 block read exception
* -34 block write exception
* -35 invalid block number, such as zero (0)
* -55 floating-point unidentified fault (SIGFPE)  
* -56 `QUIT`  
* -61 `ALLOT` or `RESIZE`

- - -


References
----------

Forth 200x  
<http://www.forth200x.org/>

Forth Standard  
<https://forth-standard.org/>

