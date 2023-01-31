Post4 (Post-Forth)
==================

Copyright 2007, 2023 Anthony Howe.  All rights reserved.


### String Words

#### -TRAILING
( `caddr` `u` -- `caddr` `u'` )  
Trim trailing spaces from end of string `caddr` `u`.

- - -
#### /STRING
( `caddr` `u` `n` -- `caddr'` `u'` )  
Adjust the character string at `caddr` `u` by offset `n` characters, by adding `n` to `caddr` and subtracting `n` from length `u`.

- - -
#### BLANK
( `caddr` `u` -- )  
If `u` is greater than zero, store the character value for space in `u` consecutive character
positions beginning at `caddr`.

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
#### SEARCH
( `caddr1` `u1` `caddr2` `u2` -- `caddr3` `u3` `bool` )  
Search the string `caddr1` `u1` for the string `caddr2` `u2`.  If `bool` is true, a match was found at `caddr3` with `u3` characters remaining.  If `bool` is false there was no match and `caddr3` `u3` are just `caddr1` `u1`.

- - -
#### SLITERAL
( `caddr` `u` -- ) immediate  
Compile the string given by `caddr` and `u` into the definition so that it is later pushed onto the stack during execution of the definition.  See `S"` and `S\"`.

- - -

### Post4 Specific Words

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
String length of NUL terminated string.  See `args`, `env`, and `puts`.

- - -
#### strrev
( `caddr` `u` -- )  
Reverse the string in place.

- - -
