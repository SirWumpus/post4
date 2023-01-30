INCLUDE ../test/assert.p4

.( Memory Words ) test_group
: tw_write_cell_mem ( addr n -- )
	1+ 1 DO I OVER ! CELL+ LOOP DROP
;

: tw_check_cell_mem ( addr n -- )
	1+ 1 DO
		I SWAP >R >R
		T{ R> ( I ) -> R@ ( addr ) @ }T
		R> CELL+
	LOOP DROP
;

: tw_write_char_mem ( addr n -- )
	1+ 1 DO I OVER C! CHAR+ LOOP DROP
;

: tw_check_char_mem ( addr n -- )
	1+ 1 DO
		I SWAP >R >R
		T{ R> ( I ) -> R@ ( addr ) C@ }T
		R> CHAR+
	LOOP DROP
;

VARIABLE tv_addr
VARIABLE tv_here
HERE tv_here !

	\ Yep nested.
	.( ALLOCATE FREE ALIGNED ) test_group
T{ 50 CELLS ALLOCATE SWAP tv_addr ! -> 0 }T
T{ tv_addr @ ALIGNED -> tv_addr @ }T 		\ Test address is aligned
\ T{ HERE -> tv_here @ }T 			\ Check data space pointer is unaffected
tv_addr @ 50 tw_write_cell_mem
tv_addr @ 50 tw_check_cell_mem 			\ Check we can access the heap
T{ tv_addr @ FREE -> 0 }T
T{ 99 ALLOCATE SWAP tv_addr ! -> 0 }T
T{ tv_addr @ ALIGNED -> tv_addr @ }T 		\ Test address is aligned
T{ tv_addr @ FREE -> 0 }T
T{ -1 ALLOCATE SWAP DROP 0= -> FALSE }T 	\ Memory allocate failed
	test_group_end

T{ HERE -> tv_here @ }T 			\ Data space pointer unaffected by FREE
CR
	\ Yep nested.
	.( ALLOCATE FREE RESIZE ) test_group
T{ 50 CHARS ALLOCATE SWAP tv_addr ! -> 0 }T
tv_addr @ 50 tw_write_char_mem tv_addr @ 50 tw_check_char_mem

\ Resize smaller does not change content.
T{ tv_addr @ 28 CHARS RESIZE SWAP tv_addr ! -> 0 }T
tv_addr @ 28 tw_check_char_mem

\ Resize larger does not change original content.
T{ tv_addr @ 100 CHARS RESIZE SWAP tv_addr ! -> 0 }T
tv_addr @ 28 tw_check_char_mem

\ Resize error does not change addr
T{ tv_addr @ -1 RESIZE 0= -> tv_addr @ FALSE }T
T{ tv_addr @ FREE -> 0 }T
	test_group_end

T{ HERE -> tv_here @ }T				\ Data space pointer is unaffected
test_group_end


