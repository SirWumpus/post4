Post4 (Post-Forth)
==================

Copyright 2007, 2024 Anthony Howe.  All rights reserved.


### File Access Words

#### BIN
( `mode1` -- `mode2` )  
Modify file access `mode1` to append a "binary" flag and return `mode2`.

- - -
#### CLOSE-FILE
( `fid` -- `ior` )  
Close the file `fid` and return `ior` with zero on success, otherwise a non-zero error code (see `errno(2)`).

- - -
#### CREATE-FILE
( `caddr` `u` `mode` -- `fid` `ior` )  
Create the file given by the character string `caddr` `u` using the file access `mode`.  On success return the file id `fid` and `ior` equal to zero; otherwise on error `fid` is zero and `ior` is a non-zero error code (see `errno(2)`).

- - -
#### DELETE-FILE
( `caddr` `u` -- `ior` )  
Delete the file given by the character string `caddr` `u`, returning `ior` with zero on success, otherwise non-zero error code (see `errno(2)`).

- - -
#### FILE-POSITION
( `fid` -- `ud` `ior` )  
Return the current file position `ud` for the file `fid`.

- - -
#### FILE-SIZE
( `fid` -- `ud` `ior` )  
Return the current file size `ud` for the file `fid` and  `ior` with zero on success or non-zero on error.

- - -
#### FLUSH-FILE
( `fid` -- `ior` )  
Force a write of all buffered data for the given file `fid`, returning `ior` with zero on success, otherwise non-zero error code (see `errno(2)`).

- - -
#### INCLUDE-FILE
( `x*i` `fid` -- `x*j` )  
Save the current input source specification, including the current value of `SOURCE-ID`.  Interpret the file, `fid`, line by line until end of file.  Other stack effects are due to the words interpreted.  The input source specification is restored after the file is closed.

- - -
#### OPEN-FILE
( `caddr` `u`  `mode` -- `fid` `ior` )  
Open the file given by the character string `caddr` `u` using the file access `mode`.  On success return the file id `fid` and `ior` equal to zero; otherwise on error `fid` is zero and `ior` is a non-zero error code (see `errno(2)`).

- - -
#### R/O
( -- `mode` )  
Set file access read-only `mode`.

- - -
#### R/W
( -- `mode` )  
Set file access read-write `mode`.

- - -
#### READ-FILE
( `caddr` `u1` `fid` -- `u2` `ior` )  
Read at most `u1` characters into the buffer `caddr` from file `fid`.  Return the number of characters `u2` actually read and `ior` with zero on success or non-zero on error.

- - -
#### READ-LINE
( `caddr` `u1` `fid` -- `u2` `flag` `ior` )  
Read the next line into a buffer `caddr` at most `u1` characters from the file `fid`.  Return the number of characters `u2` actually read, not counting the implementation-defined newline, with `flag` set true on success, and `ior` with zero on success or non-zero on error.

- - -
#### REPOSITION-FILE
( `ud` `fid` -- `ior` )  
Reposition the file `fid` to byte offset `ud` and return `ior` with zero on success, otherwise a non-zero error code (see `errno(2)`).

- - -
#### REQUIRE
( `i*x` `<spaces>filepath` -- `j*x` )  
Skip leading white space and parse `filepath` delimited by a white space character.  Push the address and length of the `filepath` on the stack and perform the function of `REQUIRED`.  See also `REQUIRE-PATH` below.

        ok REQUIRE ../examples/wumpus.p4

- - -
#### REQUIRED
( `i*x` `caddr` `u` -- `j*x` )  
If the file `caddr` `u` has been `INCLUDED` or `REQUIRED` already, discard `caddr` `u`; otherwise perform the function of `INCLUDED`.  See also `REQUIRED-PATH` below.

- - -
#### W/O
( -- `mode` )  
Set file access write-only `mode`.

- - -
#### WRITE-FILE
( `caddr` `u` `fid` -- `ior` )  
Write `u` characters from address `caddr` to the file `fid` and return `ior` with zero on success or non-zero on error.

- - -
#### WRITE-LINE
( `caddr` `u` `fid` -- `ior` )  
Write `u` characters from address `caddr` to the file `fid` followed by an implementation-dependent newline and return `ior` with zero on success or non-zero on error.

- - -

### Post4 Specific Words

#### open-file-path
( `path` `p` `file` `f`  `mode` -- `fid` `ior` )  
Search the colon (:) separated path list, `path` `p`, and open the first file, `file` `f`, found using the file access `mode`.  On success return the file id `fid` and `ior` equal to zero; otherwise on error `fid` is zero and `ior` is a non-zero error code (see `errno(2)`).

- - -
#### require-path
( `i*x` `<spaces>filepath` -- `j*x` )  
If `filepath` has been previous loaded by `INCLUDE-PATH` or `REQURIE-PATH`, then skip the file; otherwise push the
address and length of the `filepath` on the stack and perform the function of `required-path`.

        ok REQUIRE-PATH ed.p4

- - -
#### required-path
( `caddr` `u` -- )  
If the file `caddr` `u` has been `INCLUDED` or `REQUIRED` already, discard `caddr` `u`; otherwise perform the function of `INCLUDED-PATH`, which searches the colon (:) separated path list given by the environment variable `POST4_PATH` for the first file `caddr` `f`.

        ok S" ed.p4" REQUIRED-PATH

- - -
#### stdin
( -- `fid` )  
Push the standard input file handle.  For use with `READ-FILE` or `READ-LINE`.  Closing the file handle will have undefined behaviour.

- - -
#### stdout
( -- `fid` )  
Push the standard output file handle.  For use with `WRITE-FILE` or `WRITE-LINE`.  Closing the file handle will have undefined behaviour.

- - -
