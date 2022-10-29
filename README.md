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

By default a user block file, `.post4.blk`, is opened from the current directory or user's `HOME` directory.  This can be overridden with the `-b` option.  To skip the block file, specify an empty filepath, eg. `-b ''`.

Post4 reads input from standard input and writes to standard output, which can be redirected:

        echo "123 69 + ." | post4

Post4 cell size is equivalent to C's `intptr_t`, for current systems are either 32 or 64 bit values.


Building
---------

Post4 is written in ISO C11 using only one extension, `Labels As Values`, which is supported by `gcc` and `clang` compilers.

        $ ./configure --help
        $ ./configure
        $ make
        $ sudo make install
        $ post4


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

        $ post4 -i wumpus.p4
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

* [Standard Words](./doc/standard.md)
* [Block File Words](./doc/block.md)
* [Double-Cell Words](./doc/double.md)
* [Floating-Point Words](./doc/float.md)
* [Tool Words](./doc/tools.md)


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
* -40 invalid BASE for floating point conversion
* -55 floating-point unidentified fault (`SIGFPE`)  
* -56 `QUIT`  
* -59 `ALLOCATE`
* -61 `ALLOT` or `RESIZE`


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
