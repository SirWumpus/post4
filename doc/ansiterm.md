Post4 (Post-Forth)
==================

Copyright 2007, 2024 Anthony Howe.  All rights reserved.


### ANSI Terminal Words

Post4 assumes that the terminal is ANSI / VT-100 compatible.


#### AT-XY
( `col` `row` -- )  
Position cursor on the terminal, `row` and `col` are zero (0) based.  This word is a standard word.

- - -
#### PAGE
( -- )  
Clear the terminal (advance next page).  This word is a standard word.

- - -

### Post4 Specific Words

To use this extension word set:

        include-path post4/ansiterm.p4


####  ansi_normal
( -- )  

- - -
####  ansi_bold
( -- )  

- - -
####  ansi_dim
( -- )  

- - -
####  ansi_standout
( -- )  

- - -
####  ansi_underline
( -- )  

- - -
####  ansi_blink
( -- )  

- - -
####  ansi_reverse
( -- )  

- - -
####  ansi_hide
( -- )  

- - -
####  ansi_black
( -- )  

- - -
####  ansi_red
( -- )  

- - -
####  ansi_green
( -- )  

- - -
####  ansi_yellow
( -- )  

- - -
####  ansi_blue
( -- )  

- - -
####  ansi_magenta
( -- )  

- - -
####  ansi_cyan
( -- )  

- - -
####  ansi_white
( -- )  

- - -
####  ansi_bg_black
( -- )  

- - -
####  ansi_bg_red
( -- )  

- - -
####  ansi_bg_green
( -- )  

- - -
####  ansi_bg_yellow
( -- )  

- - -
####  ansi_bg_blue
( -- )  

- - -
####  ansi_bg_magenta
( -- )  

- - -
####  ansi_bg_cyan
( -- )  

- - -
####  ansi_bg_white
( -- )  

- - -
