Post4 (Post-Forth)
==================

Copyright 2007, 2024 Anthony Howe.  All rights reserved.


### Tool Words

#### ;]
(C: `quotation-sys` `colon-sys` -- )(S: -- `xt`) immediate  
Ends the current nested definition started by `[:`, leaving `xt` on the data stack, and resumes compilation of the previous enclosing definition.

        : foo 123 [: ." wave " ;] execute . ;

- - -
#### .S
( -- )  
Dump the data stack.

- - -
#### ?
( `aaddr` -- )  
Display the cell value stored at `aaddr`.

- - -
#### AHEAD
( -- ) immediate  
Jump ahead over a block of code.  A building block for several flow control words.

        AHEAD
            \ not executed
        THEN
        \ continue

- - -
#### BYE
( -- )  
Terminate and return to the host OS with exit code zero (0), if any.

- - -
#### CS-PICK
( `xu` `xu-1` ... `x1` `x0` `u` -- `xu` `xu-1` ... `x1` `x0` `xu` )  
Remove `u`.  Copy the `xu` to the top of the control-stack.  `0 CS-PICK` equivalent to `DUP`, `1 CS-PICK` equivalent to `OVER`.

- - -
#### CS-ROLL
( `xu` `xu-1` ... `x0` `u` -- `xu-1` ... `x0` `xu` )  
Left rotate the control-stack `u` cells.

- - -
#### DUMP
( `addr` `u` -- )  
Display the contents of `u` consecutive addresses starting at `addr`.

- - -
#### FIND-NAME
( `caddr` `u` -- `nt` | 0 )  
Find the definition identified by the string `caddr` `u` in the current search order.  Return its name token `nt` if found, otherwise zero (0).

- - -
#### FIND-NAME-IN
( `caddr` `u` `wid` -- `nt` | 0 )  
Find the definition identified by the string `caddr` `u` in the wordlist `wid`.  Return its name token `nt` if found, otherwise zero (0).

- - -
#### N>R
( `i*x` `u` -- ) (R: -- `i*x` `u` )  
Move `u+1` items from the data stack to the return stack for later retrieval using `NR>`.  Note the data will not be overwritten by a subsequent invocation of `N>R` and a program may not access data placed on the return stack before the invocation of `N>R`.

- - -
#### NR>
( -- `i*x` `u` ) (R: `i*x` `u` -- )  
Move `u+1` items, previously stored with `N>R`, from the return stack to the data stack.  The behaviour is undefined if `NR>` is used with data not stored by `N>R`.

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
#### SEE
( `<spaces>name` -- )  
Display an implementation-defined human-readable representation of the word `name`.

- - -
#### TRAVERSE-WORDLIST 
( `i*x` `xt` `wid` -- `j*x` )  
Execute `xt`, which has stack effect `( k*x nt -- l*x bool )`, once for every word in the word list `wid` passing the name token `nt` of the word to `xt`, until the word list is exhausted or until `xt` returns false.  

- - -
#### WORDS
( -- )  
List defined words

- - -
#### [:
(C: -- `quotation-sys` `colon-sys` ) immediate  
Suspends compilation of the current (enclosing) definition, continues compilation with this nested definition until terminated by `;]` that leaves `xt` on the data stack for the enclosing definition.

        : foo 123 [: ." wave " ;] execute . ;

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

### Post4 Specific Words

#### .fs
( -- )  
Dump the float stack.

- - -
#### .rs
( -- )  
Dump the return stack.

- - -
#### alias
( `xt` `<spaces>name` --  )  
Create an alias word for `xt`.

- - -
#### bye-status
( `exit_code` -- )  
Terminate and return to the host OS an exit code; zero (0) for normal/success, non-zero an error occurred.

- - -
#### c?
( `caddr` -- )  
Display the character value stored at `caddr`.

- - -
#### stack_length
( `stk` -- `u` )  
Return the current stack length (depth).

- - -
#### stack_new 
( `u` `<spaces>name` -- )  
Create a stack of `u` cells with `name`.  Invoking `name` will puts `stk` address on the stack.

- - -
#### stack_pop
( `stk` -- `x` )  
Pop the top of the stack `stk` returning `x`.

- - -
#### stack_push
( `x` `stk` -- )  
Push `x` onto the stack `stk`.  *Currently no overflow checking.*

- - -
#### stack_tmp
( `u` -- `stk` )  
Allocate a stack of `u` cells returning `stk`.  Use `FREE` to discard.

- - -
#### trace
( -- `aaddr` )  
Return the address `aaddr` of the trace variable; set true for tracing, otherwise false to disable.  See also option `-T`.

- - -
#### words-in
( `wid` -- )  
List the words from word-list `wid`.

- - -
#### _bp
( -- )  
Breakpoint.  Display current source buffer position.  Intended as convenient function, with no side effects, to set a breakpoint while debugging.

- - -
#### _seext
( `xt` -- )  
Display an implementation-defined human-readable representation referenced by `xt`.

- - -
#### _stack_check
( -- | ⊥ )  
Perform an explicit stack check and throw on error.

- - -
#### _stack_dump
( `aaddr` `u` -- )  
Utility word used to define `.S` and `.rs`.

- - -
