Post4 (Post-Forth)
==================

Copyright 2007, 2023 Anthony Howe.  All rights reserved.


### Standard Core Words

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
#### .
( `n` -- )  
Display `n` in free field format.

- - -
#### .R
( `n1` `n2` -- )  
Display `n1` right aligned in a field `n2` characters wide.  If the number of characters required to display `n1` is
greater than `n2`, all digits are displayed with no leading spaces in a field as wide as necessary.

- - -
#### ." ccc"
( `ccc<quote>` -- ) immediate  
Display `ccc`.

- - -
#### .( ccc)
( `ccc<paren>` -- ) immediate  
Parse and display text until an unescaped closing parenthesis.  Backslash followed by any other character escapes that character, ie. `\\` is a literal backslash, `\)` is a literal closing parenthesis.

- - -
#### /
( `dend` `dsor` -- `quot` )  
Divide the dividend `dend` by the divisor `dsor` leaving the symmetric quotient `quot` on top of the stack..

- - -
#### /MOD
( `dend` `dsor` -- `rem` `quot` )  
Divide the dividend `dend` by the divisor `dsor` leaving the remainder `rem` and quotient `quot` on top the stack.

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
`x2` is the result of shifting `x1` one bit toward the least-significant bit, leaving the most-significant bit unchanged, .ie arithmetic right shift.

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
#### 2SWAP
( `x1` `x2` `x3` `x4` -- `x3` `x4` `x1` `x2` )  
Exchange the top two cell pairs.

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
( `n1` `<spaces>name` -- `n2` )  
Skip leading space delimiters.  Parse name delimited by a space.  Offset is the first float aligned value greater than or equal to `n1`.  `n2 = offset + 1` float.

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
#### LITERAL
( `x` -- ) immediate  
Compile `x` into the definition so that it is later pushed onto the stack during execution of the definition.

         : SOMEWORD ... [ x ] LITERAL ... ;

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
The double-cell `d` (`lo` `hi`) is the signed product of `n1` times `n2`.

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
When interpreting, copy the escaped string `ccc` to a transient buffer and return `caddr u`.  When compiling, append the escaped string `ccc` to the current word so when executed it leaves `caddr u` of the string on the stack.  Note as an extension strings are also `NUL` terminated to facilitate use of host environment functions.

- - -
#### SAVE-INPUT
( -- `xn`...`x1` `n` )  
Save the current input source state for later use by `RESTORE-INPUT`.

- - -
#### SIGN
( `n` -- )  
If `n` is negative, add a minus sign to the beginning of the pictured numeric output string.  An ambiguous condition exists if `SIGN` executes outside of a `<# #>` delimited number conversion.

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
#### U.R
( `u` `n` -- )  
Display `u` right aligned in a field `n` characters wide.  If the number of characters required to display `u` is
greater than `n`, all digits are displayed with no leading spaces in a field as wide as necessary.

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
#### \\
( `ccc<LF>` -- ) immediate  
Parse and discard the remainder of the parse area, ie. comment line.

- - -
#### ]
( -- )  
Enter compilation state.

- - -

### Post4 Specific Words

#### .\\" ccc"
( `ccc<quote>` -- ) immediate  
Display the escaped string `ccc`.

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
#### cell-bits
( -- u )  
Size of a cell in bits.

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
#### max-char
( -- `u` ) constant  
Maximum value of any character.  Currently Post4 only supports ASCII and addressable units are octets.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### max-n
( -- `n` ) constant  
Largest usable signed integer.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### max-u
( -- `u` ) constant  
Largest usable unsigned integer.  This is a deviation from `ENVIRONMENT?` queries.

- - -
#### min-n
( -- `n` ) constant  
Smallest usable signed integer.

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
( -- `aaddr` `n` `s` )  
Push the data stack base `aaddr` address, depth, and size (before executing `_ds`).  This stack is a fixed size and grows upward.

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
( -- `aaddr` `n` `s` )  
Push the return stack base address, depth, and size.  This stack is a fixed size and grows upward.

- - -
#### _rsp!
( `aaddr` -- )  
Store `aaddr` into the return stack pointer.

- - -
#### _rsp@
( -- `aaddr` )  
Fetch the return stack pointer.

- - -
#### _stdin
( -- )  
Set source to standard input; see `cat.p4` example.

- - -
#### _window
( -- `rows` `cols` )  
Return the terminal window dimensions.

- - -
