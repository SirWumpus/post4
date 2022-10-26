Post4 (Post-Forth)
==================

Copyright 2007, 2022 Anthony Howe.  All rights reserved.


Overview
--------

Post4 is a hosted indirect threaded Forth dialect written in C, based on the ["Forth 200x Draft 19.1, 2019-09-30"](http://www.forth200x.org/documents/forth19-1.pdf).  Post4 aims to implement the fewest possible built-in words in C, those that are needed to interact with memory and I/O, leaving the remaining standard words to be implemented in Forth.

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

Post4 cell size is equivalent to C's `intptr_t`, for current systems are either 32 or 64 bit values.


Interactive Usage
-----------------

When Post4 is invoked without a script file argument or redirected input, then the user is presented with an `ok` prompt.  The user can enter Post4 numbers and words.  Pressing the terminal `Enter` (`Return`) key ends the input line, which will be interpreted.  All Post4 words are treated as case-insensitive.

To leave Post4 either type the `EOF` terminal character, `BYE`, or `code BYE-CODE` (where `code` is number to return to the user's shell).  For those not familiar with Forth, there is a `QUIT` word, but it only terminates a running program; it does not return to the host OS.

### Numeric I/O

The default numeric input/output base is decimal (base 10).  Setting variable `BASE` will change the default radix used for numeric I/O, which can be between between 2 and 36, eg. `16 BASE !` sets hexadecimal.  There are four shorthand words `BINARY`, `OCTAL`, `DECIMAL`, and `HEX` that set `BASE` to 2, 8, 10, or 16 repectively.

Regardless of the current value of `BASE`, it is possible to input numbers in one of the four common bases without having to change the value of `BASE`.  Prefixing a number with `%`, `0` (zero), `#`, `$` or `0x` can set a binary, octal, decimal, or hex value; an optional minus sign given after the prefix to indicate a negative number.  For example:

        %1111111 = #127 = 0177 = $7f = 0x7f = $000000000000007f
        %-1111111 = #-127 = 0-177 = $-7f = 0x-7f = $ffffffffffffff81

It is also possible to input a character constant or backslash escape character.  Simple use single-quotes around the character or backslash-escape string (see also `CHAR` and `[CHAR]`).  For example:

        'A'     ASCII upper case A.
        'b'     ASCII lower case B.
        '9'     ASCII digit 9.
        '_'     ASCII underscore.
        '\n'    ASCII linefeed.

The following C-style backslash escapes are supported:

        \?      delete
        \\      backslash
        \a      bell
        \b      backspace
        \e      escape
        \f      formfeed
        \n      linefeed
        \r      carriage-return
        \s      space
        \t      tab
        \v      vertical tab
        \0      nul

Because Forth uses whitespace for input delimiters, in particular space (ASCII 32), the only way to input a literal space character is with:

        32      ASCII numeric value.
        '\s'    Backslash escape.
        BL      Forth word, short for blank.


Examples
--------

### cat.p4

Example of implementing `cat(1)`.

        $ post4 cat.p4 </etc/hosts


### catch_throw.p4

A demonstration of using CATCH and THROW.

        $ post4 -i catch_throw.p4
        Enter TRY-IT or RETRY-IT words to test CATCH / THROW.
        ok 


### dumpargs.p4

Simple demonstration on how to access the command line arguments.

        $ post4 dumpargs.p4 hello world, tell us more!


### ed.p4 - Block Editor

There are actually three block editor word sets:

1. An `ed(1)` like block line editor using `LIST`, `PRINT`, and `CHANGE`.
2. An interactive single line editor using `EDIT`; left & right cursor keys, tab toggles insert or replace mode, delete backspace, ESC quits.
3. A full "screen" block editor using `ED`; all the commands of the interactive single line editor, plus up & down cursor keys, `CTRL+G` goto block, `CTRL+P` & `CTRL+N` block, and `ESC` menu.

NOTE: that `EDIT` and `ED` are hard coded with ANSI terminal escape sequences.

        $ post4 -i ed.p4
        Type ED to start editor.
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


### life1d.p4 - [One Dimensional Cellular Atomata](https://en.wikipedia.org/wiki/Cellular_automaton)

This example demostrates five  Wolfram rules 30, 90, 110, 184, and 104 using the same initial state.

        $ post4 life1d.p4


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
        ok INCLUDE rand.p4
        ok 12345 SRAND !
        ok RAND . RAND . RAND . CR
        21468 9988 22117
        ok 12345 RANDA ! 54321 RANDB ! $deadbeef RANDC !
        ok 16 RANDOMXOR U. 16 RANDOMXOR U. 16 RANDOMXOR U. CR
        52917 27383 64651
        ok 16 RANDOMMAJ U. 16 RANDOMMAJ U. 16 RANDOMMAJ U. CR
        7 9885 36906
        ok 


### wumpus.p4

[Hunt The Wumpus](https://en.wikipedia.org/wiki/Hunt_the_Wumpus) game ported from the original BASIC source written by Gregory Yob.

        $ port4 -i wumpus.p4
        ... instructions ...
        Type PLAY to start.
        ok PLAY
        
        BATS NEARBY
        YOU ARE IN ROOM 8
        TUNNELS LEAD TO    1   7   9
        (S)HOOT OR (M)OVE? M
        WHERE TO? 7
        ZAP--SUPER BAT SNATCH! ELSEWHEREVILLE FOR YOU!
        
        YOU ARE IN ROOM 13
        TUNNELS LEAD TO   12  14  20
        (S)HOOT OR (M)OVE? M
        WHERE TO? 14
        
        I SMELL A WUMPUS!
        YOU ARE IN ROOM 14
        TUNNELS LEAD TO    4  13  15
        (S)HOOT OR (M)OVE? 


Glossary
--------

### Standard Words

#### \!
( `x` `aaddr` -- )  
Store `x` at `aaddr`.

- - -
#### \#
( `ud1` -- `ud2` )  
Divide `ud1` by the number in `BASE` giving the quotient `ud2` and the remainder `n`.  (`n` is the least significant digit of `ud1`.)  Convert `n` to external form and append the resulting character to the pictured numeric output string. 

- - -
#### \#\>
( `xd` -- `caddr` `u` )  
Drop `xd`.  Make the pictured numeric output string available as a character string.  `caddr` and `u` specify the resulting character string.  A program may replace characters within the string.

- - -
#### \#S
( `ud1` -- `0` )  
Convert one digit of `ud1` according to the rule for `#`.  Continue conversion until the quotient is zero (0).

- - -
#### '
( `<spaces>name` -- `xt` )  
Find `name` and place its execution token on the stack.  Throw undefined word (-13) if not found.

- - -
#### ( ccc)
( `ccc<paren>` -- ) immediate  
Parse and ignore characters up to the closing right parenthesis, which an empty string or span multiple lines.

- - -

#### \*
( `n1|u1` `n2|u2` -- `n3|u3` )  
Multiply the top two stack values.

- - -
#### \*/
( `n1` `n2` `dsor` -- `quot` )  
Multiply `n1` by `n2` then divide by divisor `dsor` giving the quotient `quot`.

- - -
#### \*/MOD
( `n1` `n2` `dsor` -- `rem` `quot` )  
Multiply `n1` by `n2` then divide by divisor `dsor` giving the remainder `rem` and quotient `quot`.

- - -
#### +
( `n1|u1` `n2|u2` -- `n3|u3` )  
Add the top two stack values.

- - -
#### +!
( `n|u` `aaddr` -- )  
Add `n|u` to the cell at `aaddr`.

- - -
#### +FIELD
( `addr` `<spaces>name` -- `addr'` )  
Define unaligned structure field `name`, which when executed adds the field offset to `addr` giving `addr'`.

Structure name defined last:

        0                         \ initial total byte count
          1 CELLS +FIELD p.x      \ single cell field named p.x
          1 CELLS +FIELD p.y      \ single cell field named p.y
        CONSTANT point            \ save structure size

Structure name defined first:

        BEGIN-STRUCTURE point     \ create the named structure
          1 CELLS +FIELD p.x      \ A single cell field named p.x
          1 CELLS +FIELD p.y      \ A single cell field named p.y
        END-STRUCTURE

- - -
#### +LOOP
( `step` -- ) immediate  
Add `step` to the loop index.  If the loop index did not cross the boundary between the loop limit minus one and the loop limit, continue execution at the beginning of the loop.  Otherwise, end the current loop and continue execution immediately following the loop.

        : SOMEWORD ... limit first DO ... step +LOOP ... ;

- - -
#### ,
( `x` -- )  
Align and reserve one cell of data-space and store `x` there.

- - -
#### -
( `n1|u1` `n2|u2` -- `n3|u3` )  
Subtract the top two stack values.

- - -
#### -TRAILING
( `caddr` `u` -- `caddr` `u'` )  
Trim trailing spaces from end of string `caddr` `u`.

- - -
#### .
( `n` -- )  
Display `n` in free field format.

- - -
#### ." ccc"
( `ccc<quote>` -- ) immediate  
Display `ccc`.

- - -
#### .( ccc)
( `ccc<paren>` -- ) immediate  
Parse and display text until an unescaped closing parenthesis.  Backslash followed by any other character escapes that character, ie. `\\` is a literal backslash, `\)` is a literal closing parenthesis.

- - -
#### .S
( -- )  
Dump the data stack.

- - -
#### /
( `dend` `dsor` -- `quot` )  
Divide the dividend `dend` by the divisor `dsor` leaving the symmetric quotient `quot` on top of the stack..

- - -
#### /MOD
( `dend` `dsor` -- `rem` `quot` )  
Divide the dividend `dend` by the divisor `dsor` leaving the remainder `rem` and quotient `quot` on top the stack.

- - - 
#### /STRING
( `caddr` `u` `n` -- `caddr'` `u'` )  
Adjust the character string at `caddr` `u` by offset `n` characters, by adding `n` to `caddr` and subtracting `n` from length `u`.

- - -
#### 0<
( `n` -- `bool` )  
`bool` is true if and only if `n` is less than zero (0).

- - -
#### 0<>
( `nu` -- `bool` )  
`bool` is true if `nu` is not equal to zero (0).

- - -
#### 0>
( `nu` -- `bool` )  
`bool` is true if `nu` is greater to zero (0).

- - -
#### 0=
( `x` -- `bool` )  
`bool` is true if and only if `x` is equal to zero (0).

- - -
#### 1+

( `n1|u1` -- `n2|u2` )  
Add one (1) to `n1|u1` giving the sum `n2|u2`.

- - -
#### 1-

( `n1|u1` -- `n2|u2` )  
Subtract one (1) from `n1|u1` giving the difference `n2|u2`.

- - -
#### 2!
( `lo` `hi` `aaddr` -- )  
Store the cell pair `lo hi` at `aaddr` as `hi lo`.

- - -
#### 2*
( `x1` -- `x2` )  
`x2` is the result of shifting `x1` one bit toward the most-significant bit, filling the vacated least-significant bit with zero (0).

- - -
#### 2/
( `x1` -- `x2` )  
`x2` is the result of shifting `x1` one bit toward the least-significant bit, leaving the most-significant bit unchanged.

- - -
#### 2@
( `aaddr` -- `lo` `hi` )  
Fetch the cell pair `hi lo` stored at `aaddr` and place on the stack as `lo hi`.

- - -
#### 2DROP
( `x1` `x2` -- )  
Drop cell pair `x1 x2` from the stack.

- - -
#### 2DUP
( `x1` `x2` -- `x1` `x2` `x1` `x2` )  
Duplicate cell pair `x1 x2`.

- - -
#### 2OVER
( `x1` `x2` `x3` `x4` -- `x1` `x2` `x3` `x4` `x1` `x2` )  
Copy cell pair `x1 x2` to the top of the stack.

- - -
#### 2ROT
( `x1` `x2` `x3` `x4` `x5` `x6` -- `x3` `x4` `x4` `x5` `x1` `x2` )  
Rotate the top three cell pairs on the stack bringing cell pair `x1` `x2` to the top of the stack.

- - -
#### 2SWAP
( `x1` `x2` `x3` `x4` -- `x3` `x4` `x1` `x2` )  
Exchange the top two cell pairs.

- - -
#### 2VALUE
( `lo` `hi` `<spaces>name` -- )  
Create `name` with two cells of data assigned `hi` and `lo`.  When `name` is executed, the values `lo` `hi` are pushed to the stack.  See `TO`.

- - -
#### :
(C: `<spaces>name` -- `colon-sys` )  
Start definition of word `name`.  The current definition shall not be findable in the dictionary until it is ended (or until the execution of `DOES>`).  See also `;`, `CREATE`, `DOES>`, `:NONAME`, `[:` and `;]`.

        : SOMENAME words that do stuff ;

- - -
#### :NONAME ...
(C: -- `colon-sys` )(S: -- `xt` )  
Create a nameless word definition, which when terminated by `;` leaves `xt` on the data stack.  The nameless word cannot be found with `'` nor `FIND-NAME`.  See also `[:` and `;]`.

        \ Create forward reference.
        DEFER print
        
        \ Define the implementation.
        :NONAME ( n -- ) . ; IS print
        
        \ Alternative to : temp_word some words ;
        :NONAME some words ; CONSTANT temp_word
        : foo ... temp_word ... ;

- - -
#### ;
(C: `colon-sys` -- ) immediate  
End definition of word.

- - -
#### ;]
(C: `quotation-sys` `colon-sys` -- )(S: -- `xt`) immediate  
Ends the current nested definition started by `[:`, leaving `xt` on the data stack, and resumes compilation of the previous enclosing definition.

        : foo 123 [: ." wave " ;] execute . ;

- - -
#### <
( `n1` `n2` -- `bool` )  
`bool` is true if and only if `n1` is less than `n2`.

- - -
#### <\#
( -- )  
Initialise the pictured numeric output conversion process.

- - -
#### =
( `x1` `x2` -- `bool` )  
`bool` is true if and only if `x1` is bit-for-bit the same as `x2`.

- - -
#### >
( `n1` `n2` -- `bool` )  
`bool` is true if and only if `n1` is greater than `n2`.

- - -
#### >BODY
( `xt` -- `aaddr` )  
`aaddr` is the data-field address corresponding to `xt`.  Will throw not created (-31) if `xt` is not a word defined by `CREATE`.

- - -
#### >IN
( -- `aaddr` )  
`aaddr` is the address of a cell containing the offset in characters from the start of the
input buffer to the start of the parse area.

- - -
#### >NUMBER
( `acc` `caddr` `len` -- `acc'` `caddr'` `len'` )  
`acc'` is the unsigned result of converting the characters within the string specified by `caddr` `len` into digits according to the number in `BASE`, and adding each into `acc` after multiplying `acc` by the number in `BASE`.  Conversion continues left-to-right until a character that is not convertible, including any "+" or "-", is encountered or the string is entirely converted.  `caddr'` is the location of the first unconverted character or the first character past the end of the string if the string was entirely converted.  `len'` is the number of unconverted characters in the string.  An ambiguous condition exists if `acc'` overflows during the conversion.

- - -
#### >R
( `x` -- )(R: -- `x` )  
Move top of the data stack to the return stack.

- - -
#### ?
( `aaddr` -- )  
Display the value stored at `aaddr`.

- - -
#### ?DO
( `n1|u1` `n2|u2` -- ) (R: -- `loop-sys` ) immediate  
Mark the start of `?DO ... +LOOP` or `?DO ... LOOP`.

- - -
#### ?DUP
( `x` -- `x` `x` )  
Duplicate `x` if it is non-zero.

- - -
#### ?LEAVE
( -- )  

        : SOMEWORD ... limit first DO ... test ?LEAVE ... LOOP ... ;

- - -
#### @
( `aaddr` -- `x` )  
Fetch from `aaddr` the value `x` stored there.

- - -
#### ABORT
( `i*x` -- ) ( R: `j*x` -- )  
Throw abort (-1).  Clear the data stack and perform a `QUIT`, which clears the return stack too.  Never returns to caller.

- - -
#### ABORT" ccc"
( `i*x` `x1` -- | `i*x` ) ( R: `j*x` -- | `j*x` )  
If `x1` is not equal to zero (0), display the message that follows up to the closing double-quote and perform an `ABORT`.  Never returns to caller.

- - -
#### ABS
( `n` -- `u` )  
Return the absolute value of `n`.

- - -
#### ACTION-OF
( `<spaces>name` -- `xt` ) immediate  
Return the execution token `xt` of a deferred `name`.  See `DEFER`.

- - -
#### ACCEPT
( `caddr` `+n1` -- `+n2` )  
Accept up to `+n1` (between 1 and +32767) characters into the buffer given by `caddr`; the number of characters actually received, `+n2`, is pushed.

- - -
#### AGAIN
( -- ) immediate  
Loop back to matching `BEGIN` indefinitely.

        BEGIN
            \ loop body
        AGAIN

- - -
#### AHEAD
( -- ) immediate  
Jump ahead over a block of code.  A building block for several flow control words.

        AHEAD
            \ not executed
        THEN
        \ continue

- - -
#### ALIGN
( -- )  
If the data-space pointer is not aligned, `reserve` enough space to align it.

- - -
#### ALIGNED
( `addr` -- `aaddr` )  
`aaddr` is the first cell aligned address greater than or equal to `addr`.

- - -
#### ALLOT
( `n` -- )  
If `n` is greater than zero (0), reserve `n` address units of data space.  If `n` is less than zero (0), release `|n|` address units of data space.  If `n` is zero (0), leave the data-space pointer unchanged.

- - -
#### AND
( `x1` `x2` -- `x3` )  
Bit-wise and of the top two stack values.

- - -
#### AT-XY
( `col` `row` -- )  
Position cursor on the terminal, `row` and `col` are zero (0) based.

- - -
#### BASE
( -- `aaddr` )  
`aaddr` is the address of a cell containing the current number conversion radix, between 2..36.

- - -
#### BEGIN
( -- ) immediate  
Mark the start of `BEGIN ... AGAIN` or `BEGIN ... WHILE ... REPEAT` loops.

- - -
#### BEGIN-STRUCTURE
( `<spaces>name` -- `addr` `0` )  
Parse `name` delimited by a space and create a definition for `name`.  Return a struct-sys (zero or more implementation dependent items) that will be used by `END-STRUCTURE` and an initial offset of zero (0).  When `name` is executed the size of the structure in address units is pushed to the stack.  See also `+FIELD`, `CFIELD:`, and `FIELD:`.

- - -
#### BL
( -- `char` ) constant  
`char` is the character value for a space, since Forth words are delimited by space.  A space can also be specified with `'\s'`.

- - -
#### BLK
( -- `aaddr` )  
`aaddr` is the address of a cell containing zero (0) or the number of the mass-storage block being interpreted.  If `BLK` contains zero (0), the input source is not a block and can be identified by `SOURCE-ID`.  If a program alters `BLK`, the results are undefined.

- - -
#### BLOCK
( `blk_num` -- `aaddr` )  
`BLOCK` assigns a block buffer, `aaddr`, writing the old contents to mass storage if dirty, before reading the new contents of block `blk_num` from mass storage.   The block buffer pointed to by `aaddr` is now the current block buffer assigned to `blk_num`.

- - -
#### BUFFER
( `blk_num` -- `aaddr` )  
`BUFFER` assigns a block buffer, `aaddr`, writing the old contents to mass storage if dirty, but does not read anything.  The block buffer pointed to by `aaddr` is now the current block buffer assigned to `blk_num`.

- - -
#### C!
( `char` `caddr` -- )  
Store `char` at `caddr`.

- - -
#### C" ccc"
( `ccc<quote>` -- `caddr` ) immediate  
Compile the string `ccc` as-is into the current word so when executed it leaves the counted string `caddr` on the stack.  See also `S"`.

- - -
#### C,
( `char` -- )  
Reserve one character of data space and store `char` there.

- - -
#### C@
( `caddr` -- `char` )  
Fetch from `caddr` the character `char` stored there.

- - -
#### CASE
( -- ) immediate  

        CASE
            test1 OF ... ENDOF
            ...
            testN OF ... ENDOF
            default action
        ENDCASE

- - -
#### CATCH
( `i*x` `xt` -- `j*x` `0` | `i*x` `n` )  
Push an exception frame on the exception stack and then execute the execution token `xt` (as with `EXECUTE`) in such a way that control can be transferred to a point just after `CATCH` if `THROW` is executed during the execution of `xt`.

If the execution of `xt` completes normally (ie. the exception frame pushed by this `CATCH` is not popped by an execution of `THROW`) pop the exception frame and return zero (0) on top of the data stack, above whatever stack items would have been returned by `xt EXECUTE`.  Otherwise, the remainder of the execution semantics are given by `THROW`.

- - -
#### CELL+
( `aaddr1` -- `aaddr2` )  
Add the size in address units of a cell to `aaddr1` giving `aaddr2`.

- - -
#### CELLS
( `n1` -- `n2` )  
`n2` is the size in address units of `n1` cells.

- - -
#### CFIELD:
( `addr` `<spaces>name` -- `addr'` )  
Define character structure field `name`, which when executed adds the field offset to `addr` giving `addr'`.

- - -
#### CHAR ccc
( `<spaces>ccc` -- `char` )  
Parse `ccc` placing the first character of text on the stack.

- - -
#### CHAR+
( `caddr1` -- `caddr2` )  
Add the size in address units of a character.

- - -
#### CHARS
( `n1` -- `n2` )  
`n2` is the size in address units of `n1` characters.

- - -
#### CMOVE
( `src` `dst` `u` -- )  
Move `u` characters from character `src` address to `dst` address, proceeding from low to higher addresses.

- - -
#### CMOVE>
( `src` `dst` `u` -- )  
Move `u` characters from character `src` address to `dst` address, proceeding from high to lower addresses.

- - -
#### COMPARE
( `caddr1` `u1` `caddr2` `u2` -- `n` )  
Compare the two strings lexicographically.  Return `n` as 1, zero (0), or -1, according to whether the string `caddr1` `u1` is greater than, equal to, or less than the string `caddr2` `u2`.

- - -
#### COMPILE,
( `xt` -- )  
Append the execution semantics of the definition represented by `xt` to the execution semantics of the current definition.

- - -
#### CONSTANT
( `x` `<spaces>name` -- )  
Define the word `name` to represent the constant value `x`.  When `name` is executed, the value `x` is pushed on to the stack.


        377 CONSTANT monaco

- - -
#### COUNT
( `caddr1` -- `caddr2` `u` )  
Return the character string `caddr2 u` specification for the counted string `caddr1`.

- - -
#### CR
( -- )  
Write a newline to standard output.

- - -
#### CREATE
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
#### CS-PICK
( `xu` `xu-1` ... `x1` `x0` `u` -- `xu` `xu-1` ... `x1` `x0` `xu` )  
Remove `u`.  Copy the `xu` to the top of the control-stack.  `0 CS-PICK` equivalent to `DUP`, `1 CS-PICK` equivalent to `OVER`.

- - -
#### CS-ROLL
( `xu` `xu-1` ... `x0` `u` -- `xu-1` ... `x0` `xu` )  
Left rotate the control-stack `u` cells.

- - -
#### D0<
( `dl` `dh` -- `bool` )  
`bool` is true if and only if the double-cell `dl` `dh` is less than to zero (0).

- - -
#### D0=
( `dl` `dh` -- `bool` )  
`bool` is true if and only if the double-cell `dl` `dh` is equal to zero (0).

- - -
#### D<
( `d1` `d2` -- `bool` )  
`bool` is true if and only if the double-cells `d1` is less then `d2`.

- - -
#### D=
( `d1` `d2` -- `bool` )  
`bool` is true if and only if the double-cell `d1` is equal to `d2`.

- - -
#### D>S
( `d` -- `n` )  
Convert the double-cell number `d` (`lo` `hi`) to the single-cell number `n` with the same numerical value.

- - -
#### DABS
( `d` -- `ud` )  
Double-cell `ud` is the absolute value of `d`.

- - -
#### DECIMAL
( -- )  
Set the numeric conversion radix to ten (decimal).

- - -
#### DEFER
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
#### DEFER!
( `xt2` `xt1` -- )  
Set the deferred word `xt1` to execute `xt2` when called.

- - -
#### DEFER@
( `xt1` -- `xt2` )  
Get the execute token `xt2` stored by deferred word `xt1`.

- - -
#### DEPTH
( -- `u` )  
Number of cells on the data stack before `u` was placed there.

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
#### DO
(C: -- `do-sys`) ( `n1|u1` `n2|u2` -- ) (R: -- `loop-sys` ) immediate  
Mark the start of `DO ... +LOOP` or `DO ... LOOP`.

- - -
#### DOES>
Define the execution semantics for the most recently defined word by `CREATE`.  Throws not created (-31) if `DOES>` is applied to a word not defined by `CREATE`.

        \ General structure:
        : word1 CREATE ( build data space of word2 ) DOES> ( actions applied to data of word2 ) ;

        \ Make a new defining word CONSTANT.
        : CONSTANT CREATE , DOES> @ ;

        \ Use CONSTANT to define a word representing a value.
        377 CONSTANT monaco

        \ Use the new word.
        monaco ( -- 377 )

- - -
#### DROP
( `x` -- )  
Remove the top of the stack.

- - -
#### DUMP
( `addr` `u` -- )  
Display the contents of `u` consecutive addresses starting at `addr`.

- - -
#### DUP
( `x` -- `x` `x` )  
Duplicate `x`.

- - -
#### ELSE
( -- ) immediate  

        test IF
            \ execute for true
        ELSE
            \ execute for false
        THEN
        \ continue

- - -
#### EMIT
( `char` -- )  
Write the character octet to standard output.

- - -
#### EMPTY-BUFFERS
( -- )  
Mark all block buffers as free without saving any dirty buffers.

- - -
#### END-STRUCTURE
( `addr` `0` -- )  
Terminate definition of a structure started by `BEGIN-STRUCTURE`.

- - -
#### ENDCASE
( `x` --- ) immediate  
Discard the case selector `x` and continue execution.

        CASE
            test1 OF ... ENDOF
            ...
            testN OF ... ENDOF
            default action
        ENDCASE

- - -
#### ENDOF
( -- ) immediate  

        CASE
            test1 OF ... ENDOF
            ...
            testN OF ... ENDOF
            default action
        ENDCASE

- - -
#### EVALUATE
( `i*x` `caddr` `u` -- `j*x` )  
Save the current input source specification and make the string described by `caddr` and `u` both the input source and buffer, reset `>IN` to zero (0), and interpret.  When the parse area is empty, restore the prior input source specification.

- - -
#### EXECUTE
( `i*x` `xt` -- `j*x` )  
Remove the execution token `xt` from the stack and perform the semantics identified by it. 

- - -
#### EXIT
( -- )(R: `ip` -- )  
Return control from the currently executing word to its caller.  Before executing `EXIT` within a do-loop, a program shall discard the loop-control parameters by executing `UNLOOP`.

- - -
#### FALSE
( -- `false` ) constant  
Return false value, equivalent to zero (0).

- - -
#### FIELD:
( `addr` `<spaces>name` -- `addr'` )  
Define cell aligned structure field `name`, which when executed adds the field offset to `addr` giving `addr'`.

        BEGIN-STRUCTURE point     \ create the named structure
          FIELD: p.x              \ A single cell field named p.x
          FIELD: p.y              \ A single cell field named p.y
        END-STRUCTURE

- - -
#### FILL
( `caddr` `u` `char` -- )  
Fill memory at address `caddr` with `u` consecutive characters `char`.

- - -
#### FIND
( `caddr` -- `caddr` 0 | `xt` 1 | `xt` -1 )  
Given the counted string `caddr`, find its execution token `xt`.  If found and `xt` is an immediate, return `xt 1`; if not immediate, return `xt -1`; otherwise not found, return `caddr 0`.  See `FIND-NAME`.

        ... BL WORD FIND DUP 0= -13 AND THROW ...

- - -
#### FIND-NAME
( `caddr` `u` -- `xt` | 0 )  
Find the definition identified by the string `caddr` `u` in the current search order.  Return its execution token `xt` if found, otherwise zero (0).

- - -
#### FLUSH
( -- )  
Save and free all dirty block buffers.

- - -
#### FM/MOD
( `d` `dsor` -- `mod` `quot` )  
Floored division of the double-cell dividend `d` (`lo` `hi`) by the divisor `dsor` leaving the modulus `mod` and quotient `quot`.  In floored division the modulus `mod` carries the sign of the divisor `dsor`.

        Dividend Divisor Remainder Quotient
            10       7         3        1
           -10       7         4       -2
            10      -7        -4       -2
           -10      -7        -3        1

- - -
#### HERE
( -- `addr` )  
`addr` is the data-space pointer that will next be assigned by `,`, `ALIGN`, `ALLOT`, `C,`, `COMPILE,`.  See `RESERVE` below.

- - -
#### HEX
( -- )  
Set the numeric conversion radix to sixteen (hexadecimal).

- - -
#### HOLD
( `char` -- )  
Append `char` to the picture numeric output string.

- - -
#### HOLDS
( `caddr` `u` -- )  
Append string `caddr` `u` to the pictured numeric output string.

- - -
#### I
( -- `n|u` )  
`n|u` is a copy of the current (innermost) loop index.

- - -
#### IF
( `bool` -- ) immediate  
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
#### IMMEDIATE
( -- ) immediate  
Make the most recent definition an immediate word.

- - -
#### INCLUDE
( `<spaces>filepath` --  )  
Skip leading white space and parse `filepath` delimited by a white space character.  Push the
address and length of the `filepath` on the stack and perform the function of `INCLUDED`.

        ok INCLUDE ../examples/wumpus.p4

- - -
#### INCLUDED
( `caddr` `u` -- )  
Save the current input source specification, including the current value of `SOURCE-ID`.  Open the file specified by `caddr` `u`, store the resulting file-id in `SOURCE-ID`, and make it the input source.  Store zero (0) in `BLK`.  Interpret the file line by line until end of file.  Other stack effects are due to the words included.  The input source specification is restored after the file is closed.

- - -
#### INVERT
( `x1` -- `x2` )  
Take the one's complement of `x1`.

- - -
#### IS
( `xt` `<spaces>name` -- ) immediate  
Set deferred word `name` to the execution token `xt`.  See `DEFER`.

- - -
#### J
( -- `n|u` )  
`n|u` is a copy of the next-outer loop index.

- - -
#### KEY
( -- `char` )  
Receive one character `char`.  Characters received by `KEY` are not displayed.

- - -
#### KEY?
( -- `bool` )  
If a character is available, return true.  Otherwise, return false.  The character shall be returned by the next execution of KEY.

- - -
#### LEAVE
( -- ) (R: `loop-sys` -- ) immediate  
Leave the current loop, resume execution following the `+LOOP` or `LOOP`.

- - -
#### LIST
( `blk_num` -- )  
List the block given by `blk_num`, one (1) based, and save `blk_num` in `SCR` on success.

- - -
#### LITERAL
( `x` -- ) immediate  
Compile `x` into the definition so that it is later pushed onto the stack during execution of the definition.

         : SOMEWORD ... [ x ] LITERAL ... ;

- - -
#### LOAD
( `i*x` `blk_num` -- `j*x` )  
Save the current input-source specification.  Store `blk_num` in `BLK` (thus making block `blk_num` the input source and setting the input buffer to encompass its contents), set `>IN` to zero (0), and interpret.  When the parse area is exhausted, restore the prior input source specification.  Other stack effects are due to the words interpreted.

- - -
#### LOOP
(C: `do-sys` -- ) immediate  
Add `n` to the loop index.  If the loop index did not cross the boundary between the loop limit minus one and the loop limit, continue execution at the beginning of the loop.  Otherwise, end the current loop and continue execution immediately following the loop.

        : SOMEWORD ... limit first DO ... LOOP ... ;

Or

        : SOMEWORD ... limit first ?DO ... LOOP ... ;

- - -
#### LSHIFT
( `x1` `u` -- `x2`)  
Logical left shift of `x1` by `u` bits, putting `u` zeroes into the least significant bits.

- - -
#### M*
( `n1` `n2` -- `d` )  
`d` (`lo` `hi` on the stack) is the signed product of `n1` times `n2`.

- - -
#### MARKER
( `<spaces>name` -- )  
Create `name` in the dictionary, which when executed will remove all the words down to and including `name`, restoring the dictionary to the state it had just prior to the `name` being added.  See `WORDS`.

        ok INCLUDE ../examples/wumpus.p4
        ... instructions ...
        ok WORDS
        ... list of words ...
        ok PLAY
        ok rm_wumpus
        ok PLAY
        "PLAY" -13 thrown: undefined word
        ok

- - -
#### MAX
( `n1` `n2` -- `n3` )  
`n3` is the greater of `n1` and `n2`.

- - -
#### MIN
( `n1` `n2` -- `n3` )  
`n3` is the lesser of `n1` and `n2`.

- - -
#### MOD
( `dend` `dsor` -- `rem` )  
Divide the dividend `dend` by the divisor `dsor` leaving the remainder `rem`.

- - -
#### MOVE
( `src` `dst` `u` -- )  
If `u` is greater than zero (0), copy from `src` the contents of `u` consecutive address units to `dst`.

- - -
#### MS
( `u` -- )  
Wait at least `u` milliseconds.

- - -
#### NAME>COMPILE
( `nt` -- `xt` `xt-compile` )  
Given a name token `nt` (aka `xt` in Post4) return the word's `xt` and the action `xt-compile` to perform when compiling.

- - -
#### NAME>INTERPRET
( `nt` -- `xt` | 0 )  
Given a name token `nt` (aka `xt` in Post4) return the word's `xt` for the interpretation semantics.  Otherwise if there are  no interpretation semantics, return 0.

- - -
#### NAME>STRING
( `nt` -- `caddr` `u` )  
Given a name token `nt` (aka `xt` in Post4) return the word's read-only name string.

- - -
#### NEGATE
( `n1` -- `n2` )  
Negate `n1`, giving its arithmetic inverse `n2`.

- - -
#### NIP
( `x1` `x2` -- `x2` )  
Drop the first item below the top of stack.

- - -
#### N>R
( `i*x` `u` -- ) (R: -- `i*x` `u` )  
Move `u+1` items from the data stack to the return stack for later retrieval using `NR>`.  Note the data will not be overwritten by a subsequent invocation of `N>R` and a program may not access data placed on the return stack before the invocation of `N>R`.

- - -
#### NR>
( -- `i*x` `u` ) (R: `i*x` `u` -- )  
Move `u+1` items, previously stored with `N>R`, from the return stack to the data stack.  The behaviour is undefined if `NR>` is used with data not stored by `N>R`.

- - -
#### OF
( `x1` `x2` --  | `x1` ) immediate  

        CASE
            test1 OF ... ENDOF
            ...
            testN OF ... ENDOF
            default action
        ENDCASE

- - -
#### OR
( `x1` `x2` -- `x3` )  
Bit-wise or of the top two stack values.

- - -
#### OVER
( `x1` `x2` -- `x1` `x2` `x1` )  
Copy the second value `x1` below the stack to the top.

- - -
#### PAD
( -- `aaddr` )  
A character buffer space available to developers and *not* used by standard words.

- - -
#### PAGE
( -- )  
Clear the terminal (advance next page).

- - -
#### PARSE
( `char` `ccc<char>` -- `caddr` `u` )  
Parse `ccc` delimited by the delimiter `char`.  `caddr` and `u` are the address and length within the input buffer of the parsed string.  If the parse area was empty, the resulting string has a zero (0) length.

- - -
#### PARSE-NAME
( `<spaces>name<space>` -- `caddr` `u` )  
Skip leading space delimiters. Parse name delimited by a space.

- - -
#### PICK
( `xu` `xu-1` ... `x1` `x0` `u` -- `xu` `xu-1` ... `x1` `x0` `xu` )  
Remove `u`.  Copy the `xu` to the top of the stack.  `0 PICK` equivalent to `DUP`, `1 PICK` equivalent to `OVER`.

- - -
#### POSTPONE
( `<spaces>name` --  ) immediate  
Parse and find `name` appending the compilation semantics of `name` to the current definition.  For an immediate word, it is compiled into the definition, instead of executed.  Otherwise compile the word during the definition of another word.

An example:

        : [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE

- - -
#### QUIT
( -- )(R: `i*x` -- )  
Empty the return stack (never returns to caller), reset `SOURCE-ID` to zero (0), set the console as the input source, enter interpretation state, and start REPL:

  * Accept a line from the input source into the input buffer, set `>IN` to zero (0), and interpret.
  * Display the prompt if in interpretation state, all processing has been completed, and no ambiguous condition exists.

- - -
#### R>
( -- `x` )(R: `x` -- )  
Move top of the return stack to the data stack.

- - -
#### R@
( -- `x` )(R: `x` -- `x` )  
Copy top of return stack to the data stack.

- - -
#### RECURSE
( -- ) immediate  
Call the current definition.  The behaviour of `RECURSE` following a `DOES>` is undefined.

        : FACTORIAL ( +n1 -- +n2)
            DUP 2 < IF DROP 1 EXIT THEN DUP 1- RECURSE *
        ;

- - -
#### REFILL
( -- `bool` )  
Attempt to fill the input buffer from the input source, returning true if successful.

When the input source is the user input device, attempt to receive input into the terminal input buffer.  If successful, make the result the input buffer, set `>IN` to zero (0), and return true.  Receipt of a line containing no characters is considered successful.  If there is no input available from the current input source, return false.

When the input source is a string from `EVALUATE`, return false and perform no other action.

When the input source is a block, make the next block the input source and current input buffer by adding one to the value of `BLK` and setting `>IN` to zero (0).  Return true if the new value of `BLK` is a valid block number, otherwise false.

- - -
#### REPEAT
( -- ) immediate  
Loop back to matching `BEGIN`.

        BEGIN
            \ test expression
        WHILE
            \ loop body while true
        REPEAT
        \ continue once test is false

- - -
#### RESTORE-INPUT
( `xn`...`x1` `n` -- bool )  
Restore the input source state described by `x1` through `xn`.  `bool` is true if the input source cannot be so restored (terminal, pipeline).

- - -
#### ROLL
( `xu` `xu-1` ... `x0` `u` -- `xu-1` ... `x0` `xu` )  
Left rotate the stack `u` cells.

- - -
#### ROT
( `x1` `x2` `x3` -- `x2` `x3` `x1` )  
Rotate the top three stack entries.

- - -
#### RSHIFT
( `x1` `u` -- `x2` )  
Logical right shift of `x1` by `u` bits, putting `u` zeroes into the most significant bits.

- - -
#### S" ccc"
( `ccc<quote>` -- `caddr` `u` ) immediate  
When interpreting, copy the string `ccc` as-is to a transient buffer and return `caddr u`.  When compiling, append the string `ccc` as-is to the current word so when executed it leaves `caddr u` of the string on the stack.  Note as an extension strings are also `NUL` terminated to facilitate use of host environment functions.

- - -
#### S>D
( `n` -- `d` )  
Convert the number `n` to the double-cell number `d` (`lo` `hi`) with the same numerical value.

- - -
#### S\\" ccc"
( `ccc<quote>` -- `caddr` `u` ) immediate  
When interpreting, copy the escaped string `ccc` to a transient buffer and return `caddr u`.  When compiling, append the escaped string `ccc` to the current word so when executed it leaves `caddr u` of the string on the stack. Note as an extension strings are also `NUL` terminated to facilitate use of host environment functions.

- - -
#### SAVE-BUFFERS
( -- )  
Save all the dirty block buffers to the block file.

- - -
#### SAVE-INPUT
( -- `xn`...`x1` `n` )  
Save the current input source state for later use by `RESTORE-INPUT`.

- - -
#### SCR
( -- `aaddr` )  
`aaddr` is the address of a cell containing the block number from the most recent `LIST`.

- - -
#### SEARCH
( `caddr1` `u1` `caddr2` `u2` -- `caddr3` `u3` `bool` )  
Search the string `caddr1` `u1` for the string `caddr2` `u2`.  If `bool` is true, a match was found at `caddr3` with `u3` characters remaining.  If `bool` is false there was no match and `caddr3` `u3` are just `caddr1` `u1`.

- - -
#### SEE
( `<spaces>name` -- )  
Display an implementation-defined human-readable representation of the word `name`.

- - -
#### SIGN
( `n` -- )  
If `n` is negative, add a minus sign to the beginning of the pictured numeric output string.  An ambiguous condition exists if `SIGN` executes outside of a `<# #>` delimited number conversion.

- - -
#### SLITERAL
( `caddr` `u` -- ) immediate  
Compile the string given by `caddr` and `u` into the definition so that it is later pushed onto the stack during execution of the definition.  See `S"` and `S\"`.

- - -
#### SM/REM
( `d` `dsor` -- `rem` `quot` )  
Symmetric division of the double-cell dividend `d` (`lo` `hi`) by the divisor `dsor` leaving the remainder `rem` and quotient `quot`.  In symmetric division the remainder `rem` carries the sign of the dividend `d`.

        Dividend Divisor Remainder Quotient
            10       7         3        1
           -10       7        -3       -1
            10      -7         3       -1
           -10      -7        -3        1

- - -
#### SOURCE
( -- `caddr` `u` )  
Push the string address `caddr` and the number of characters `u` in the input buffer.

- - -
#### SPACE
( -- )  
Display a space.

- - -
#### SPACES
( `n` -- )  
If `n` is greater than zero (0), display `n` spaces.

- - -
#### STATE
( -- `aaddr` )  
Return the `aaddr` address of the compilation state; true when compiling, otherwise false when interpreting.  See also words `[` and `]`.

- - -
#### SWAP
( `x1` `x2` -- `x2` `x1` )  
Exchange the top two stack items.

- - -
#### THRU
( `u1` `u2` -- )  
`LOAD` in sequence blocks `u1` through to `u2` inclusive.

- - -
#### TIME&DATE
( -- `sec` `min` `hour` `day` `month` `year` )  
Time current local system time.  `sec` {0..59}, `min` {0..59}, `hour` {0..23}, `day` {1..31}, `month` {1..12}, `year` eg. 1970.

- - -
#### THEN
( -- ) immediate  
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
#### THROW
( `k*x` `n` -- `k*x` | `i*x` `n` )  
If `n` is non-zero, pop the top most exception frame from the exception stack, along with everything on the return stack above that frame.  Then restore the input source specification in use before the corresponding `CATCH` and adjust the depths of all stacks so that they are the same as the depths saved in the exception frame (`i` is the same number as the `i` in the input arguments to the corresponding `CATCH`), put `n` on top of the data stack, and transfer control to a point just after the `CATCH` that pushed that exception frame.

If the top of the stack is non-zero and there is no exception frame on the exception stack the system may display an implementation-dependent message giving information about the condition associated with the `THROW` code `n`.  Subsequently, the system shall perform the function of `ABORT`.

- - -
#### TO
( `i*x` `<spaces>name` -- )  

See `VALUE` and `2VALUE`.

- - -
#### TRUE
( -- `true` ) constant  
Return a true flag, a single-cell value with all bits set.

- - -
#### TUCK
( `x1` `x2` -- `x2` `x1` `x2` )  
Copy the first (top) stack item below the second stack item.

- - -
#### TYPE
( `caddr` `u` -- )  
If `u` is greater than zero (0), display the character string specified by `caddr` and `u`.

- - -
#### U.
( `u` -- )  
Display `u` in free field format.

- - -
#### U<
( `u1` `u2` -- `bool` )  
`bool` is true if and only if `n1` is less than `n2`.

- - -
#### U>
( `u1` `u2` -- `bool` )  
`bool` is true if and only if `n1` is greater than `n2`.

- - -
#### UM*
( `u1` `u2` -- `ud` )  
`ud` (`lo` `hi` on the stack) is the unsigned product of `u1` times `u2`.

- - -
#### UM/MOD
( `d` `dsor` -- `mod` `quot` )  
Divide the double-cell dividend `d` (`lo` `hi`) by divisor `dsor`, giving the quotient `quot` and the remainder `mod`.  All values and arithmetic are unsigned.

- - -
#### UNLOOP
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
#### UNTIL
( `x` -- ) immediate  
Loop back to `BEGIN` until `x` equals zero (0).

        BEGIN
            \ loop body
            \ test expression
        UNTIL

- - -
#### UNUSED
( -- `u` )  
`u` is the amount of data space remaining in the region addressed by `HERE`, in address units.

- - -
#### UPDATE
( -- )  
Mark the current block as dirty.

- - -
#### VALUE
( `x` `<spaces>name` -- )  
Create `name` with one cell of data assigned `x`.  When `name` is executed, the value `x` is pushed to the stack.  See `TO`.

        ok 69 VALUE position
        ok postion .
        69 ok
        ok 66 TO position
        ok postion .
        66 ok

- - -
#### VARIABLE
( `<spaces>name` -- )  
Create `name` with one cell of data.  When `name` is executed push the `aaddr` of the data cell.

        VARIABLE name           \ define name
        123 name !              \ store value to variable
        name @                  \ fetch value from variable

- - -
#### WHILE
( `x` -- ) immediate  
If `x` equals zero (0), continue execution following `REPEAT`.

        BEGIN
            \ test expression
        WHILE
            \ loop body while true
        REPEAT
        \ continue

- - -
#### WORD
( `char` `<chars>ccc<char>` -- `caddr` )  
Skip leading delimiters.  Parse characters `ccc` delimited by `char` and return `caddr` of the counted string.  See `PARSE-NAME`.

        ... BL WORD FIND DUP 0= -13 AND THROW ...

- - -
#### XOR
( `x1` `x2` -- `x3` )  
Bit-wise exclusive-or of the top two stack values.

- - -
#### [
( -- ) immediate  
Enter interpretation state.

- - -
#### [']
( `<spaces>name` -- `xt` ) immediate  
Place name's execution token xt on the stack.

- - -
#### [:
(C: -- `quotation-sys` `colon-sys` ) immediate  
Suspends compilation of the current (enclosing) definition, continues compilation with this nested definition until terminated by `;]` that leaves `xt` on the data stack for the enclosing definition.

        : foo 123 [: ." wave " ;] execute . ;

- - -
#### [CHAR] ccc
( `<spaces>ccc` -- `char` ) immediate  
Place char, the value of the first character of `ccc`, on the stack.

- - -
#### [DEFINED]
( `<spaces>name` -- `bool` ) immediate  
Return `TRUE` if `name` is word that can be found in the dictionary; otherwise `FALSE`.


- - -
#### [ELSE]
( `<spaces>name ...` -- ) immediate  
Discard space delimited words, including nested `[IF]...[THEN]` and `[IF]...[ELSE]...[THEN]`, until the matching (same level) `[THEN]` is found.

- - -
#### [IF]
( `bool` | `bool` `<spaces>name ...` -- ) immediate  
If `bool` is false then discard space delimited words, including nested `[IF]...[THEN]` and `[IF]...[ELSE]...[THEN]`, until the  matching (same level) `[ELSE]` or `[THEN]` is found.

- - -
#### [THEN]
( -- ) immediate  
End conditional source block.

- - -
#### [UNDEFINED]
( `<spaces>name` -- `bool` ) immediate  
Return `FALSE` if `name` is word that can be found in the dictionary; otherwise `TRUE`.

- - -
#### \\
( `ccc<LF>` -- ) immediate  
Parse and discard the remainder of the parse area, ie. comment line.

- - -
#### ]
( -- )  
Enter compilation state.

- - -

### Post4 Specific Words

#### ..
( `x` -- )  
Display value `x` in several bases.

- - -
#### .rs
( -- )  
Dump the return stack.

- - -
#### /cell
( -- `u` ) constant  
Size of a cell in octets.

- - -
#### /char
( -- `u` ) constant  
Size of a character in octets.

- - -
#### /counted-string
( -- `u` ) constant  
Maximum size of a counted string in characters.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### /hold
( -- `u` ) constant  
Size of a numeric picture buffer in characters.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### /pad
( -- `u` ) constant  
Size of a pad buffer in characters.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### <=
( `n1` `n2` -- `bool` )  
`bool` is true if and only if `n1` is less than or equal to `n2`.

- - -
#### >=
( `n1` `n2` -- `bool` )  
`bool` is true if and only if `n1` is greater than or equal `n2`.

- - -
#### >here
( -- `u` )  
Offset into the current data-space for the word being compiled.  Similar to the word `HERE`, except expressed as an offset from the start of the data-space when the current word was created.  During the compilation of a word in C based implementations, the data-space region may be relocated when its enlarged by `,`, `ALIGN`, `ALLOT`, `C,`, `COMPILE,` thus invalidating previous values of `HERE` on the stack.  Providing an offset into the current data-region allows for computing relative locations.

- - -
#### args
( -- `argv` `argc` )  
Return the number of arguments on the command line `argc` and the NULL terminated array of C string pointers `argv`.  See example `dumpargs.p4`.

- - -
#### address-unit-bits
( -- `u` ) constant  
Size of one address unit in bits.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### binary
( -- )  
Set the numeric conversion radix to 2 (binary).

- - -
#### blocks
( -- `u` )  
Number of blocks `u` currently in the block file, one (1) through to `u`.  The block file can be extended by writing to block `u'`, the file will be extended with intervening blank blocks from the current end up to but not including block `u'`, which the actual block write of `u'` will fill.

- - -
#### bye-code
( `exit_code` -- )  
Terminate and return to the host OS an exit code; zero (0) for normal/success, non-zero an error occurred.

- - -
#### c\\" ccc"
( `ccc<quote>` -- `caddr` ) immediate  
Compile the escaped string `ccc` into the current word so when executed it leaves the counted string `caddr` on the stack.  See also `S\"`.

- - -
#### char-
( `caddr1` -- `caddr2` )  
Subtract the size in address units of a character.

- - -
#### cell-
( `aaddr1` -- `aaddr2` )  
Subtract the size in address units of a cell from `aaddr1` giving `aaddr2`.

- - -
#### compile-only
( -- ) immediate  
Make the most recent definition as compile-only.  See `NAME>INTERPRET`.

- - -
#### compile-only?
( `xt` -- `bool` )  
Return `TRUE` if `xt` references a compile-only word; otherwise `FALSE`.

- - -
#### cputs
( `caddr` -- )  
Print the counted string.  See also `puts`.

- - -
#### epoch-seconds
( -- `u` )  
System clock time in seconds from the epoch.

- - -
#### dropall
( i*x -- )  
Empty the data stack.

- - -
#### env
( `key` `k` -- `value` `v` )  
Lookup the environment variable string `key` `k`.  Return string `value` `v`; if length `v` is `-1`, then the environment variable `key` was not found and `value` is invalid.

        S" HOME" env puts CR
        S" USER" env puts CR

- - -
#### floored
( -- `false` ) constant  
True if floored division is the default.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### immediate?
( `xt` -- `bool` )  
Return `TRUE` if `xt` references an immediate word; otherwise `FALSE`.

- - -
#### list+
( -- )  
Increment variable `SCR` and list next block.

- - -
#### max-char
( -- `u` ) constant  
Maximum value of any character.  Currently Post4 only supports ASCII and addressable units are octets.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### max-d
( -- `d` ) constant  
Largest usable signed double integer.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### max-n
( -- `u` ) constant  
Largest usable signed integer.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### max-u
( -- `u` ) constant  
Largest usable unsigned integer.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### max-ud
( -- `ud` ) constant  
Largest usable unsigned double integer.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### n!
( `i*x` `aaddr` -- )  

- - -
#### n,
( `i*x` `i` -- )  

- - -
#### n@
( `aaddr` -- `i*x` )  

- - -
#### octal
( -- )  
Set the numeric conversion radix to eight (octal).

- - -
#### parse-escape
( `char` `ccc<char>` -- `caddr` `u` )  
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
#### puts
( `caddr` -- )  
Print a NUL terminated string.

        CREATE greet S\" Hello world.\n"
        greet drop puts

- - -
#### reserve
( `n` -- `addr` )  
Similar to `ALLOT`, reserve `n` address-units of data-space and return its start address.  While defining a word in C based implementations, like Post4, data-space regions may be relocated when they are enlarged, thus invalidating previous values of `HERE`.  Therefore consider:

        HERE 100 CELLS ALLOT

Should `ALLOT` enlarge and relocate the data-space, the address saved by `HERE` on the stack will now point into invalid memory.  With `reserve` the address of the region just reserved is on top of the stack insuring that the address is valid until the next enlargement of the data-space by `reserve`,`,`, `ALIGN`, `ALLOT`, `C,`, or `COMPILE,`.

- - -
#### return-stack-cells
( -- `u`) constant  
Push the return stack's size.

- - -
#### stack-cells
( -- `u`) constant  
Push the data stack's size.

- - -
#### starts-with
( `caddr1` `u1` `caddr2` `u2` -- `bool` )  
Return true if string `caddr1` `u1` starts with string `caddr2` `u2`; otherwise false.

- - -
#### strcmp
( `caddr1` `u1` `caddr2` `u2` -- `n` )  
Compare the two strings lexicographically.  Return `n` greater than, equal to, or less than zero (0), according to whether the string `caddr1` `u1` is greater than, equal to, or less than the string `caddr2` `u2`.

- - -
#### strlen
( `caddr` -- `u` )  
String length of NUL terminated string.

- - -
#### strrev
( `caddr` `u` -- )  
Reverse the string in place.

- - -
#### _bp
( -- ) immediate  
Breakpoint.

- - -
#### _branch
( -- )  
Branch relative.  The integer that immediately follows is the relative distance in address units from the integer's address.  Used in the definition of flow control words, like `AGAIN` and `AHEAD`, `ELSE`, `REPEAT`.

- - -
#### _branchz
( `bool` -- )  
Branch relative if zero (0).  The integer that immediately follows is the relative distance in address units from the integer's address.  Used in the definition of flow control words, like `IF`, `WHILE`, `UNTIL`.

- - -
#### _call
( -- )  
Call relative.  The integer that immediately follows is the relative distance in address units from the integer's address.  Used in the definition of flow control words, like `RECURSE`.

- - -
#### _ctx
( -- `aaddr` )  
The base address of the Post4 current machine context.

- - -
#### _ds
( -- `aaddr` `n` )  
Push the data stack base `aaddr` address and current depth `n` (before executing `_ds`).  This stack is a fixed size and grows upward.

- - -
#### _dsp!
( `aaddr` -- )  
Store `aaddr` into the data stack pointer.

- - -
#### _dsp@
( -- `aaddr` )  
Fetch the data stack pointer.

- - -
#### _longjmp
( `n` -- )  
Return to the context saved at the start of the REPL (`QUIT`) passing `n`.  Values of `n` from -1 to -255 are the Forth 200x standard `THROW` codes.  Passing -256 is equivalent to `BYE`.

- - -
#### _parse
( `char` `bool` -- `caddr` `u` )  
Parse `ccc` delimited by the delimiter `char`.  When `bool` is false, behaviour like `PARSE`, otherwise when true parse the escaped string like `PARSE-ESCAPE`.  `caddr` and `u` are the address and length within the input buffer of the parsed (escaped) string.  If the parse area was empty, the resulting string has a zero (0) length.

- - -
#### _rs
( -- `aaddr` `n` )  
Push the return stack base address and current depth.  This stack is a fixed size and grows upward.

- - -
#### _rsp!
( `aaddr` -- )  
Store `aaddr` into the return stack pointer.

- - -
#### _rsp@
( -- `aaddr` )  
Fetch the return stack pointer.

- - -
#### _seext
( `xt` -- )  
Display an implementation-defined human-readable representation referenced by `xt`.

- - -
#### _stack_dump
( `aaddr` `u` -- )  
Utility word used to define `.S` and `.rs`.

- - -
#### _stdin
( -- )  
Set source to standard input; see `cat.p4` example.

- - -
#### _window
( -- `rows` `cols` )  
Return the terminal window dimensions.

- - -

THROW Codes
-----------

This is a list of `THROW` codes used internally by Post4.

* -1 `ABORT`  
* -2 `ABORT"`  
* -3 stack overflow  
* -4 stack underflow  
* -5 return stack overflow  
* -6 return stack underflow  
* -9 invalid memory address (`SIGSEGV`)  
* -10 division by zero  
* -13 undefined word  
* -14 interpreting a compile-only word  
* -17 pictured numeric output string overflow  
* -21 unsupported operation  
* -22 control structure mismatch  
* -24 invalid numeric argument
* -28 user interrupt (`SIGINT`)  
* -29 compiler nesting  
* -31 word not defined by `CREATE`
* -33 block read exception
* -34 block write exception
* -35 invalid block number, such as zero (0)
* -55 floating-point unidentified fault (`SIGFPE`)  
* -56 `QUIT`  
* -59 `ALLOCATE`
* -61 `ALLOT` or `RESIZE`

- - -


References
----------

Forth 200x  
<http://www.forth200x.org/>

Forth Standard  
<https://forth-standard.org/>

Forth Discussions  
<https://github.com/ForthHub/discussion/discussions>

Forth  
<https://en.wikipedia.org/wiki/Forth_(programming_language)>

Starting Forth  
<https://www.forth.com/starting-forth/>
