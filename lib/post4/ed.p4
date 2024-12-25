\ ed
\
\ block editor
\
\ Actually there are 3 editors here:
\
\   1.	An ed(1) like line editor using LIST, PRINT, and CHANGE
\
\   2.	An interactive single line editor using EDIT; left & right
\	cursor keys, tab toggles insert or replace mode, delete &
\	backspace, ESC quits.
\
\   3.	A full "screen" editor using ED; all the commands of the
\	interactive single line editor, plus up & down cursor keys,
\	CTRL+G goto block, CTRL+P & CTRL+N block, ESC menu.
\
\ The interactive editors assume ANSI escape sequences.
\

[DEFINED] WORDLIST [IF]
WORDLIST CONSTANT editor-wordlist
: EDITOR ( -- )
	GET-ORDER DUP 1 = IF
	  \ Keep Forth and add editor word list to search.
	  DROP editor-wordlist 2
	ELSE
	  \ Replace first word list in search.
	  NIP editor-wordlist SWAP
	THEN SET-ORDER
;
ONLY FORTH ALSO EDITOR DEFINITIONS
[THEN]

MARKER rm_change

#64 CONSTANT block_width
#16 CONSTANT block_height
block_width block_height * CONSTANT block_size

1 SCR !

: block_append ( -- )
	SCR @ blocks U> IF
	  \ Extend block file by a blank block.
	  SCR @ BUFFER block_size BLANK UPDATE SAVE-BUFFERS
	THEN
;

( row -- c- )
: block_row block_width * SCR @ BLOCK + ;

( row -- )
: print_row DUP 2 .R '|' EMIT block_row block_width TYPE '|' EMIT ;
: PRINT print_row CR ;

( block -- )
: LIST SCR ! block_height 0 DO I PRINT LOOP ;

( row ccc<eol> -- )
: CHANGE block_row block_width ACCEPT . CR UPDATE ;

MARKER rm_edit

$001b5b41 CONSTANT key_up	\ ANSI
$001b5b42 CONSTANT key_down	\ ANSI
$001b5b44 CONSTANT key_left	\ ANSI
$001b5b43 CONSTANT key_right	\ ANSI
$001b4f50 CONSTANT key_f1	\ vt100
$001b4f51 CONSTANT key_f2	\ vt100
$001b4f52 CONSTANT key_f3	\ vt100
$001b4f53 CONSTANT key_f4	\ vt100
$1b5b317e CONSTANT key_home
$1b5b327e CONSTANT key_ins
$1b5b337e CONSTANT key_del
$1b5b347e CONSTANT key_end
$1b5b357e CONSTANT key_pgup
$1b5b367e CONSTANT key_pgdn

( -- key | ANSI key code )
: key_in 0 BEGIN #8 LSHIFT KEY OR KEY? 0= UNTIL ;

( -- number delim )
: key_number
	0
	BEGIN
		KEY
		DUP '0' '9' 1+ WITHIN
	WHILE
		'0' -
		SWAP 10 *
		+
	REPEAT
;

( -- x y )
\ Query the cursor position, "\e[6n" , responds with "\e[rrr;cccR"
\ where rrr and ccc are one or more ASCII digits, 1-based.
: ansi_report S\" \e[6n" TYPE KEY KEY 2DROP key_number DROP 1- key_number DROP 1- SWAP ;

VARIABLE edit_x
VARIABLE edit_y
VARIABLE edit_mode

block_width 1- CONSTANT edit_max_x
block_height 1- CONSTANT edit_max_y

( row -- c- )
: block_row_col block_row edit_x @ + ;

( -- length )
: block_row_tail_length block_width edit_x @ - ;

( -- )
: edit_inc_x edit_x @ 1+ edit_max_x MIN edit_x ! ;
: edit_dec_x edit_x @ 1- 0 MAX edit_x ! ;
: edit_inc_y edit_y @ 1+ edit_max_y MIN edit_y ! ;
: edit_dec_y edit_y @ 1- 0 MAX edit_y ! ;

( some_key this_key -- some_key )
: edit_on_key OVER <> IF R> DROP THEN ;

( row k -- row k | -- )
: edit_quit '\n' edit_on_key 2DROP CR QUIT ;

( k -- k )
: edit_up key_up edit_on_key edit_dec_y ;
: edit_down key_down edit_on_key edit_inc_y ;
: edit_left key_left edit_on_key edit_dec_x ;
: edit_right key_right edit_on_key edit_inc_x ;

( row -- )
: edit_shift_right 			\ row
	block_row_col			\ c-
	DUP 1+				\ c- c-'
	block_row_tail_length 1-	\ c- c-' u
	MOVE				\ --
	UPDATE
;

( row -- )
: edit_shift_left			\ row
	DUP block_row block_width 1- +	\ row c-$
	SWAP block_row_col		\ c-$ c-
	DUP 1+ SWAP 			\ c-$ c-' c-
	block_row_tail_length		\ c-$ c-' c- u
	MOVE				\ c-$
	BL SWAP C!			\ --
	UPDATE
;

( -- )
: edit_is_insert edit_mode @ 'I' = ;
: edit_is_replace edit_mode @ 'R' = ;
: edit_do_toggle edit_is_insert 'R' AND edit_is_replace 'I' AND OR edit_mode ! ;

: edit_is_col_edge edit_x @ 1 edit_max_x WITHIN INVERT ;
: edit_is_not_col_edge edit_x @ 1 edit_max_x WITHIN ;

( k -- k )
: edit_mode_toggle '\t' edit_on_key edit_do_toggle ;

( row k -- row 0 )
: edit_replace OVER block_row_col C! edit_inc_x 0 UPDATE ;

( row k -- row k )
: edit_insert edit_is_not_col_edge IF OVER edit_shift_right THEN ;

( k -- f )
: is_print #32 #127 WITHIN ;
: is_not_print is_print 0= ;

( k -- k | -- )
: edit_only_print DUP is_not_print IF R> DROP THEN ;

( row k -- row k )
: edit_ins_rep
	edit_only_print			\ row k
	edit_is_insert			\ row k f
	IF edit_insert THEN		\ row k
	edit_replace			\ row
	UPDATE
;

( row k -- row k )
: edit_delete edit_on_key OVER edit_shift_left ;
: edit_delete_ascii '\?' edit_delete ;
: edit_delete_ansi key_del edit_delete ;

( row k -- row k )
: edit_backspace
	'\b' edit_on_key		\ row k
	edit_is_not_col_edge		\ row k f
	IF 				\ row k
		edit_dec_x		\ row k
		OVER edit_shift_left	\ row k
	THEN
;

( -- )
: edit_cursor edit_x @ 3 + edit_y @ AT-XY ;

( row -- )
: edit_row				\ row
	BEGIN
		'\r' EMIT		\ row
		DUP print_row		\ row
		edit_cursor		\ row
		key_in			\ row k
		edit_quit		\ row k
		edit_left		\ row k
		edit_right		\ row k
		edit_delete_ascii	\ row k
		edit_delete_ansi	\ row k
		edit_backspace		\ row k
		edit_mode_toggle	\ row k
		edit_ins_rep		\ row k
		DROP			\ row
	AGAIN
;

( row -- )
: EDIT
	'I' edit_mode !			\ row
	ansi_report			\ row x y
	edit_y ! edit_x !		\ row
	edit_row			\ --
;

MARKER rm_ed

: ansi_erase_line S\" \e[2K" TYPE ;

( -- c- )
: block_end SCR @ BLOCK block_size + ;

( k -- k )
: ed_quit 'q' edit_on_key SAVE-BUFFERS 2DROP 0 block_height 1+ AT-XY QUIT ;
: ed_prev $10 edit_on_key SCR @ 1- 1 MAX DUP SCR ! BLOCK DROP ;
: ed_next $0e edit_on_key 1 SCR +! block_append SCR @ BLOCK DROP ;

( row -- )
: ed_erase_line block_row block_width BLANK 0 edit_x ! UPDATE ;

( -- )
: ed_erase_block SCR @ BUFFER block_size BLANK 0 edit_x ! 0 edit_y ! UPDATE ;

( row k -- row k )
: ed_wipe_line 'w' edit_on_key OVER ed_erase_line ;
: ed_wipe_block 'W' edit_on_key ed_erase_block ;

: ed_line_delete
	'd' edit_on_key 		\ row k
	OVER block_row			\ row k c-
	DUP block_width + SWAP		\ row k c-' c-
	block_end 2 PICK -		\ row k c-' c- u
	MOVE				\ row k
	edit_max_y ed_erase_line	\ row k
;

( row -- )
: ed_insert_line			\ row
	block_row 			\ c-
	DUP block_width + 		\ c- c-'
	block_end block_width -		\ c- c-' u
 	2 PICK -			\ c- c-' u'
	MOVE				\ --
	UPDATE
;

( row k -- row k )
: ed_line_insert
	'i' edit_on_key
	OVER ed_insert_line
	OVER ed_erase_line
;

( -- )
: ed_is_not_last_row edit_y @ edit_max_y < ;

( row -- )
: ed_erase_tail block_row_col block_row_tail_length BLANK ;

( row k -- row k )
: ed_newline
	'\n' edit_on_key				\ row k
	ed_is_not_last_row				\ row k f
	IF
		edit_inc_y				\ row k
		edit_is_insert				\ row k f
		IF
			OVER ed_insert_line		\ row k
			OVER ed_erase_tail		\ row k
			edit_y @			\ row k row'
			edit_x @ 0 ?DO			\ row k row'
				edit_dec_x		\ row k row'
				DUP edit_shift_left	\ row k row'
			LOOP DROP
		THEN
		0 edit_x !
	THEN
;

( -- number delim )
: key_number_echo
	0
	BEGIN
		KEY
		DUP '0' '9' 1+ WITHIN
	WHILE
		DUP EMIT
		'0' -
		SWAP #10 *
		+
	REPEAT
;

( k -- k )
: ed_goto_block
	'\a' edit_on_key
	0 0 AT-XY
	ansi_erase_line
	." Goto block? " key_number_echo DROP
	?DUP IF SCR ! SCR @ BLOCK DROP THEN
	ansi_erase_line
;

( row k -- row k' )
: ed_menu
	'\e' edit_on_key
	0 block_height 1+ AT-XY
	." Cmd (q)uit, (d)el line, (i)ns line, (w)ipe line, (W)ipe block?"
	key_in NIP			\ row k'
	ansi_erase_line
	ed_line_delete			\ row k'
	ed_line_insert			\ row k'
	ed_wipe_line			\ row k'
	ed_wipe_block			\ row k'
	ed_quit				\ row k'
	DROP 0				\ row 0
;

: ed_home 0 0 AT-XY ;
: ed_mode edit_mode @ EMIT SPACE ;
: ed_block_number ." Block " SCR @ . ." of " blocks . ;
: ed_status ed_home ed_mode ed_block_number ."  ^G goto ^I toggle ^N next ^P prev ^[ menu" CR ;
: ed_cursor edit_x @ #3 + edit_y @ 1 + AT-XY ;
: ed_screen ed_status SCR @ LIST ed_cursor ;
: ed_command
	key_in				\ k
	edit_up				\ k
	edit_down			\ k
	edit_left			\ k
	edit_right			\ k
	edit_mode_toggle		\ k
	ed_prev				\ k
	ed_next				\ k
	ed_goto_block			\ k
	edit_y @ SWAP			\ row k
	edit_delete_ascii		\ row k
	edit_delete_ansi		\ row k
	edit_backspace			\ row k
	edit_ins_rep			\ row k
	ed_newline			\ row k
	ed_menu				\ row k
	2DROP				\ --
;

: ED ( -- )
	\ Assert block file has at least one block.
	['] block_append CATCH ABORT" Try OPEN-BLOCK first."
	PAGE 'I' edit_mode ! BEGIN ed_screen ed_command AGAIN
;

.( Type ED to start editor.
)
