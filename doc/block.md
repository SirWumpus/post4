Post4 (Post-Forth)
==================

Copyright 2007, 2024 Anthony Howe.  All rights reserved.


Block Files
-----------

Block files are an emulation of block devices, like floppy disks, hard disks, solid-state drives, and USB memory sticks; essentially any read-write mass storage device.  Post4 is a hosted Forth implementation so sits on top of an existing OS and takes advantage of the OS to interface with storage devices.  The Block word set is of historical interest and curio, modern Forth system will more likely use the File Access word set.

A Block device, or file as implemented by Post4, consists of 1024 byte data (binary or text) blocks numbered from one (1).  The `blocks` word will show the current number of blocks in the file.  If a block number off the end of the file is accessed, the file will grow to encompass that block number; intervening blocks from the last block to the new EOF are filled with blanks.

A block of text data is displayed as 16 lines of 64 byte rows per block, see `LIST` and the `ed.p4` block editor.  Most conventional text editors will have trouble trying to edit a text block file, especially if mixed with binary data.  On [SUS](https://pubs.opengroup.org/onlinepubs/9699919799.2018edition/) platforms it is possible to use `dd(1)` to convert a text block file to and from a sequential text file.

To convert from a text block to a text file, where lines are at most of 64 bytes long, trailing whitespace truncated, and newline terminated:

        $ dd cbs=64 conv=unblock if=some_file.blk  of=some_file.txt
        $ vi some_file.txt

To convert a text file back to a block file:

        $ dd cbs=64 conv=block if=some_file.txt  of=some_file.blk
        $ post4 -b some_file.blk
        ok include /usr/local/lib/post4/ed.p4
        ok ed

Note that when editing a text file that will be converted back to a block format, lines must be kept to 64 bytes (or columns).  UTF8 multibyte characters will disrupt line lengths, since text editors think in terms of characters, not bytes, unless you restrict yourself to ASCII only.


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

#### block-close
( -- )  
First save any dirty blocks, then close the block file.

- - -
#### block-open
( `caddr` `u` -- `bool` )  
First close the current block file, if any, then open the file path given by `caddr` `u` and return true on success.

        ok S" .post4.blk" OPEN-BLOCK
        ok BLOCKS .
        3 ok

- - -
#### blocks
( -- `u` )  
Number of blocks `u` currently in the block file, one (1) through to `u`.  The block file can be extended by writing to block `u'`, the file will be extended with intervening blank blocks from the current end up to but not including block `u'`, which the actual block write of `u'` will fill.

- - -
#### list+
( -- )  
Increment variable `SCR` and list next block.

- - -
