Post4 (Post-Forth)
==================

Copyright 2007, 2024 Anthony Howe.  All rights reserved.


Overview
--------

Post4 is a hosted indirect threaded Forth dialect written in C, based on the ["Forth 200x Draft 19.1, 2019-09-30"](http://www.forth200x.org/documents/forth19-1.pdf).  Post4 aims to implement the fewest possible built-in words in C, those that are needed to interact with memory and I/O, leaving the remaining standard words to be implemented in Forth.

        usage: post4 [-TV][-b file][-c file][-d size][-f size][-i file][-m size]
                     [-r size][script [args ...]]

        -b file         open a block file
        -c file         word definition file; default post4.p4 from $POST4_PATH
        -d size         data stack size in cells; default 64
        -f size         float stack size; default 6
        -i file         include file; can be repeated; searches $POST4_PATH
        -m size         data space memory in KB; default 128
        -r size         return stack size in cells; default 64
        -T              enable tracing; see TRACE
        -V              build and version information

        If script is "-", read it from standard input.


The environment variable `POST4_PATH` provides a colon separated search path for the `post4.p4` core word definitions file and include files.  If `POST4_PATH` is undefined, then an OS specific default path is used.  A specific word definition file can be specified with `-c`.

By default no block file is opened.  Use `-b file` to open a block file at start-up; otherwise see [BLOCK-OPEN](./doc/block.md) and [BLOCK-CLOSE](./doc/block.md) words.

Post4 reads input from standard input and writes to standard output, which can be redirected:

**Example 1 - pipe input**  

        echo "123 69 + ." | post4

**Example 2 - here document**  

        post4 - hey babe <<EOF
        include-path dumpargs.p4
        args .s dropall cr
        s\" Hello world (again)!\r\n" type
        EOF

Post4 cell size is equivalent to C's `intptr_t`, which for most current systems are 64 bits.


Glossary of Words
-----------------

* [Standard Core](./doc/standard.md)
* [Block File](./doc/block.md)
* [Double-Cell](./doc/double.md)
* [File Access](./doc/file.md)
* [Floating-Point](./doc/float.md)
* [Memory](./doc/memory.md)
* [Seach-Order](./doc/search.md)
* [String](./doc/string.md)
* [Tool](./doc/tools.md)
* [THROW Codes](./doc/throw_codes.md)


Building
--------

Post4 is written in ISO C11 using only one extension, `Labels As Values`, which is supported by `gcc` and `clang` compilers.  Post4 has been built and tested on [NetBSD](https://netbsd.org/), [Cygwin](https://www.cygwin.com/), [Alpine Linux](https://alpinelinux.org/), and [Rocky Linux](https://rockylinux.org).

        $ ./configure --help
        $ ./configure
        $ make tests
        $ make run              # Manually test in the source tree.
        ok BYE
        $ sudo make install
        $ post4

If `gcc` is the default compiler, it is possible to override that to test building with a different C compiler suite, most likely [Clang](https://clang.llvm.org/) for example:

        $ make CC=clang clean build

Building for debug can be done in two ways.  The first enables debug options in Post4 amd JNI as needed:

        $ ./configure --enable-debug
        $ make clean build tests

Alternative following an earlier configuration, possible to build a temporary debug build:

        $ ./configure
        $ cd src
        $ make DBG='-g -O0' clean build

The `DBG` macro can be used to override default `CFLAGS` to try different compiler optimisations.  In the case of `-O` the last one specified overrides the previous occurences.  By default Post4 builds with `-Os`, because `small is beautiful`.  If speed is more of concerning simply use `DBG='-O2'` or `DBG='-O3'`.

Java Native Interface
---------------------

JNI support is availble.  See the [JNI documentation](./jni/README.md) about building, API, and examples.


Interactive Usage
-----------------

When Post4 is invoked without a script file argument or redirected input, then the user is presented with an `ok` prompt.  The user can enter Post4 numbers and words.  Pressing the terminal `Enter` (`Return`) key ends the input line, which will be interpreted.  All Post4 words are treated as case-insensitive.

To leave Post4 either type the `EOF` terminal character, `BYE`, or `code BYE-CODE` (where `code` is number to return to the user's shell).  For those not familiar with Forth, there is a `QUIT` word, but it only terminates a running program; it does not return to the host OS.

### Numeric I/O

The default numeric input/output base is decimal (base 10).  Setting variable `BASE` will change the default radix used for numeric I/O, which can be between between 2 and 36, eg. `16 BASE !` sets hexadecimal.  There are four shorthand words `BINARY`, `OCTAL`, `DECIMAL`, and `HEX` that set `BASE` to 2, 8, 10, or 16 repectively.

Regardless of the current value of `BASE`, it is possible to input numbers in one of the three common bases without having to change the value of `BASE`.  Prefixing a number with `%`, `#`, `$` can set a binary, decimal, or hex value; an optional minus sign given after the prefix to indicate a negative number.  For example:

        %1111111  = #127  = $7f  = $000000000000007f
        %-1111111 = #-127 = $-7f = $ffffffffffffff81

When current `BASE` is ten (10) its possible to input floating point numbers (provided support is enabled) with a decimal point and/or scientific notation.  For example:

         0.0    =  0.  =  0E0  =  0e0
         1.0    =  1.  =  1E0  =  1e0  =  +1.E0
         12.3   =  123E-1
        -0.123  = -123e-3

Floating point numbers are placed on the float stack, which is separate from the data and return stacks.  Also the float stack is small, though at least six (6) deep.  See [Floating-Point Words](./doc/float.md), in particular words [f.](./doc/float.md) and [fs.](./doc/float.md) to start with.

- *Note the double-cell input notation, `1234.` (equivalent to `1234 0`), is not supported.*
- *Note the input notation, `123E`, where there is no value following the exponent is not supported.*

It is also possible to input a character constant or backslash escape character.  Simple use single-quotes around the character or backslash-escape string (see also `CHAR` and `[CHAR]`).  For example:

        'A'     ASCII upper case A.             '_'     ASCII underscore.
        'b'     ASCII lower case B.             '\n'    ASCII linefeed.
        '9'     ASCII digit 9.


The following C-style backslash escapes are supported:

        \?      delete                          \n      linefeed
        \\      backslash                       \r      carriage-return
        \a      bell                            \s      space
        \b      backspace                       \t      tab
        \e      escape                          \v      vertical tab
        \f      formfeed                        \z      nul


Because Forth uses whitespace for input delimiters, in particular space (ASCII 32), the only way to input a literal space character is with:

        32      ASCII numeric value.
        '\s'    Backslash escape.
        BL      Forth word, short for blank.

Forth has strings: counted and length prefixed.  Counted strings are a carry-over from historical Forth, where the string stored in memory, is prefixed by a length octet, limiting the string length to 255 ASCII octets.  The standard Forth character string, when stored in memory, is prefixed with an unsigned numeric length cell.  As a Post4 extension, character strings are always NUL terminated as a convenience for interacting with the C library and host OS API.

Some examples of words using strings.  Note that the whitespace character (space, tab, newline) immediately after the word, eg. `S\"`, delimits the word from the text that follows and is not part of the text:

        ." some text"                           Print some text at runtime.
        C" counted text"                        Define a counted string.
        ."  <- space printed"                   Print text with leading space (or tab, hard to tell).
        .\" \s<- space printed"                 Print text with leading space escaped for clarity.
        S" some text"                           Store some text.
        S\" escaped\atext\r\n"                  Store some escaped text.
        .( lots of text
           spanning multiple lines)             Show some text (now).
        .( ed(1\) is the standard text editor)  Escape terminating delimiter.
        ABORT" text message"                    What I do wrong now?


Examples
--------

### catch_throw.p4

A demonstration of using CATCH and THROW.

        $ post4 -i catch_throw.p4
        Enter TRY-IT or RETRY-IT words to test CATCH / THROW.
        ok 


### dumpargs.p4

Simple demonstration on how to access the command line arguments.

        $ post4 dumpargs.p4 hello world, tell us more!


### ed.p4

Block Editor;  actually three block editor word sets:

1. An `ed(1)` like block line editor using `LIST`, `PRINT`, and `CHANGE`.
2. An interactive single line editor using `EDIT`; left & right cursor keys, tab toggles insert or replace mode, delete backspace, ESC quits.
3. A full "screen" block editor using `ED`; all the commands of the interactive single line editor, plus up & down cursor keys, `CTRL+G` goto block, `CTRL+P` & `CTRL+N` block, and `ESC` menu.

NOTE: that `EDIT` and `ED` are hard coded with ANSI terminal escape sequences.

        $ post4 -b .post4.blk -i ed.p4
        Type ED to start editor.
        ok ED


### life.p4

[Conway's Game of Life](http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)

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


### life1d.p4

[One Dimensional Cellular Atomata](https://en.wikipedia.org/wiki/Cellular_automaton)

This example demostrates five Wolfram rules 30, 90, 110, 184, and 104 using the same initial state.

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

Moving Forth Series  
<https://www.bradrodriguez.com/papers/>
