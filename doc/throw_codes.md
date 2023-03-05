Post4 (Post-Forth)
==================

Copyright 2007, 2023 Anthony Howe.  All rights reserved.


### THROW Codes

This is a list of `THROW` codes used internally by Post4.

* -1 `ABORT`
* -2 `ABORT"`
* -3 stack overflow
* -4 stack underflow
* -5 return stack overflow
* -6 return stack underflow
* -9 invalid memory address (`SIGSEGV`)
* -10 division by zero
* -12 argument type mismatch
* -13 undefined word
* -14 interpreting a compile-only word
* -17 pictured numeric output string overflow
* -21 unsupported operation
* -22 control structure mismatch
* -24 invalid numeric argument
* -28 user interrupt (`SIGINT`)
* -29 compiler nesting
* -31 word not defined by `CREATE`
* -33 block read exception
* -34 block write exception
* -35 invalid block number, such as zero (0)
* -40 invalid BASE for floating point conversion
* -44 floating-point stack overflow
* -45 floating-point stack underflow
* -55 floating-point unidentified fault (`SIGFPE`)
* -56 `QUIT`
* -59 `ALLOCATE`
* -61 `ALLOT` or `RESIZE`

