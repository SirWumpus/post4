Post4 (Post-Forth)
==================

Copyright 2007, 2022 Anthony Howe.  All rights reserved.


### Tool Words

#### .S
( -- )  
Dump the data stack.

- - -
#### ?
( `aaddr` -- )  
Display the value stored at `aaddr`.

- - -
#### DUMP
( `addr` `u` -- )  
Display the contents of `u` consecutive addresses starting at `addr`.

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
#### SEE
( `<spaces>name` -- )  
Display an implementation-defined human-readable representation of the word `name`.

- - -
#### WORDS
( -- )  
List defined words

- - -

### Post4 Specific Words

#### ..
( `x` -- )  
Display value `x` in several bases.

- - -
#### _bp
( -- ) immediate  
Breakpoint.

- - -
#### .fs
( -- )  
Dump the float stack.

- - -
#### .rs
( -- )  
Dump the return stack.

- - -
#### _seext
( `xt` -- )  
Display an implementation-defined human-readable representation referenced by `xt`.

- - -
#### _stack_dump
( `aaddr` `u` -- )  
Utility word used to define `.S` and `.rs`.

- - -
