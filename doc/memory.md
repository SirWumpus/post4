Post4 (Post-Forth)
==================

Copyright 2007, 2024 Anthony Howe.  All rights reserved.


### Memory Words

#### ALLOCATE
( `u` -- `aaddr` `ior` )  
Allocate `u` address units of contiguous data space.  The data-space pointer is unaffected by this operation.  The initial content of the allocated space is undefined.  If the allocation succeeds, `aaddr` is the aligned starting address of the allocated space and `ior` is zero (0).  If the operation fails, `aaddr` does not represent a valid address and `ior` is the implementation-defined I/O result code.

- - -
#### FREE
( `aaddr` -- `ior` )  
Return the contiguous region of data space indicated by `aaddr` to the system for later allocation.  `aaddr` shall indicate a region of data space that was previously obtained by `ALLOCATE` or `RESIZE`.  The data-space pointer is unaffected by this operation.  If the operation succeeds, `ior` is zero.  If the operation fails, `ior` is the implementation defined I/O result code.

- - -
#### RESIZE
( `aaddr1` `u` -- `aaddr2` `ior` )  
Change the allocation of the contiguous data space starting at the address `aaddr1`, previously allocated by `ALLOCATE` or      `RESIZE`, to `u` address units.  `u` may be either larger or smaller than the current size of the region.  The data-space pointer is unaffected by this operation.

If the operation succeeds, `aaddr2` is the aligned starting address of `u` address units of allocated memory and `ior` is zero (0).  `aaddr2` may be, but need not be, the same as `aaddr1`.  If they are not the same, the values contained in the region at `aaddr1` are copied to `aaddr2`, up to the minimum size of either of the two regions.  If they are the same, the values contained in the region are preserved to the minimum of `u` or the original size.  If `aaddr2` is not the same as `aaddr1`, the region of memory at `aaddr1` is returned to the system according to the operation of `FREE`.

If the operation fails, `aaddr2` equals `aaddr1`, the region of memory at `aaddr1` is unaffected, and `ior` is the implementation defined I/O result code.

- - -
