Post4 (Post-Forth)
==================

Copyright 2007, 2024 Anthony Howe.  All rights reserved.


### Search-Order Words

#### ALSO
( -- )  
Transform the word search order consisting of `widn`, ... `wid2`, `wid1` (where `wid1` is searched first) into `widn`, ... `wid2`, `wid1`, `wid1`.  An ambiguous condition exists if there are too many word lists in the word search order.

- - -
#### DEFINITIONS
( -- )  
Make the compilation word list the same as the first word list in the word search order.  New words will be added to the compilation list.  Subsequent to the word search order will not affect the compilation word list.

- - -
#### FORTH
( -- )  
Replace the first word-list id, `wid1`, with the Forth word-list id `widf`, such that the word search order `widn`, ... `wid2`, `wid1` becomes `widn`, ... `wid2`, `widf`.

- - -
#### FORTH-WORDLIST
( -- `wid` )  
Return the `wid` of the word list that includes all standard words provided by the implementation.  This word list is initially the compilation word list and is part of the initial word search order.

- - -
#### GET-CURRENT
( -- `wid` )  
Return the `wid` of the compilation word list for new words.

- - -
#### GET-ORDER
( -- `widn`, ... `wid2`, `wid1` `n` )  
Returns the number of word lists `n` in the word search order and the word list identifiers `widn`, ... `wid1` where `wid1` is searched first.  The word search order is unaffected.

- - -
#### ONLY
( -- )  
Set the word search order to the implementation-defined minimum functional word search order.  The minimum functional word search order shall include the words `FORTH-WORDLIST` and `SET-ORDER`.

- - -
#### ORDER
( -- )  
Display each set of words in word search order given by `GET-ORDER` followed by the current compilation word list given by `GET-CURRENT`.

- - -
#### PREVIOUS
( -- )  
Remove the top word set from the word search order, such that the word search order `widn`, ... `wid2`, `wid1` becomes `widn`, ... `wid2`.  An ambiguous condition exists if the word search order was empty before `PREVIOUS` was executed.

- - -
#### SEARCH-WORDLIST
( `caddr` `u` `wid` -- 0 | `xt` 1 | `xt` -1 )  
Find a word given by `caddr` `u` in the word list `wid`.  If the definition is not found, return zero (0).  If the definition is found, return its execution token `xt` and one (1) if the definition is immediate, minus-one (-1) otherwise.

- - -
#### SET-CURRENT
( `wid` -- )  
Set the compilation word list to the word list identified by `wid`.

- - -
#### SET-ORDER
( `widn`, ... `wid2`, `wid1` `n` -- )  
Set the word search order to the word lists given by `widn`, ... `wid1` where `wid1` is searched first.  If `n` is `-1` reset the word search order to implementation-defined minimum functional word search order that includes the words `FORTH-WORDLIST` and `SET-ORDER`.  If `n` is zero (0), empty the word search order.  Note that interactively setting an empty word search order will disable the system.

- - -
#### WORDLIST
( -- `wid` )  
Create (or assign) a new empty word list, returning its word list identifier `wid`.

- - -

### Post4 Specific Words

#### wordlists
( -- `u` ) constant  
Maximum number of word lists usable in the word search order.

- - -
