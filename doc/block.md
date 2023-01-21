Post4 (Post-Forth)
==================

Copyright 2007, 2023 Anthony Howe.  All rights reserved.


### Block File Words

#### BLK
( -- `aaddr` )  
`aaddr` is the address of a cell containing zero (0) or the number of the mass-storage block being interpreted.  If `BLK` contains zero (0), the input source is not a block and can be identified by `SOURCE-ID`.  If a program alters `BLK`, the results are undefined.

- - -
#### BLOCK
( `blk_num` -- `aaddr` )  
`BLOCK` assigns a block buffer, `aaddr`, writing the old contents to mass storage if dirty, before reading the new contents of block `blk_num` from mass storage.   The block buffer pointed to by `aaddr` is now the current block buffer assigned to `blk_num`.

- - -
#### BUFFER
( `blk_num` -- `aaddr` )  
`BUFFER` assigns a block buffer, `aaddr`, writing the old contents to mass storage if dirty, but does not read anything.  The block buffer pointed to by `aaddr` is now the current block buffer assigned to `blk_num`.

- - -
#### EMPTY-BUFFERS
( -- )  
Mark all block buffers as free without saving any dirty buffers.

- - -
#### FLUSH
( -- )  
Save and free all dirty block buffers.

- - -
#### LIST
( `blk_num` -- )  
List the block given by `blk_num`, one (1) based, and save `blk_num` in `SCR` on success.

- - -
#### LOAD
( `i*x` `blk_num` -- `j*x` )  
Save the current input-source specification.  Store `blk_num` in `BLK` (thus making block `blk_num` the input source and setting the input buffer to encompass its contents), set `>IN` to zero (0), and interpret.  When the parse area is exhausted, restore the prior input source specification.  Other stack effects are due to the words interpreted.

- - -
#### SAVE-BUFFERS
( -- )  
Save all the dirty block buffers to the block file.

- - -
#### SCR
( -- `aaddr` )  
`aaddr` is the address of a cell containing the block number from the most recent `LIST`.

- - -
#### THRU
( `u1` `u2` -- )  
`LOAD` in sequence blocks `u1` through to `u2` inclusive.

- - -
#### UPDATE
( -- )  
Mark the current block as dirty.

- - -

### Post4 Specific Words

#### blocks
( -- `u` )  
Number of blocks `u` currently in the block file, one (1) through to `u`.  The block file can be extended by writing to block `u'`, the file will be extended with intervening blank blocks from the current end up to but not including block `u'`, which the actual block write of `u'` will fill.

- - -
#### list+
( -- )  
Increment variable `SCR` and list next block.

- - -
