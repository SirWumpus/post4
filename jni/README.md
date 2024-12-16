Post4 (Post-Forth)
==================

Copyright 2007, 2024 Anthony Howe.  All rights reserved.


Java Native Interface
---------------------

Using the Java Native Interface (JNI), it is possible to create Post4 instances and execute words.


#### Building

The `./configure` script will by default attempt to build the JNI support, assuming the [OpenJDK 17+](https://jdk.java.net/) is installed and `$JAVA_HOME` is defined.  It is possible to disable JNI support using `./configure --without-java`.

        $ ./configure --help
        $ ./configure --with-java[=directory]
        $ make test
        $ cd jni
        $ make run
        ok include ../test/units.p4
        ...
        ok bye
        $

- - -
### Package post4.jni

#### Class Post4

* public Post4()
* public Post4(String[])
* public Post4(Post4Options)

Create a new Post4 machine.  If a `String[]` is given, its intended to pass something like command-line arguments.  Otherwise when a `Post4Options` object is given, they are used to alter the Post4 configuration.

- - -
* public int repl()

Start or resume an interactive Post4 machine, entering the Read-Evaluate-Print-Loop (REPL).  On EOF, return from the method.  The `BYE` or `BYE-CODE` words terminate the process, returning an exit status to the user's shell.  Any other exception is written to the terminal and a `Post4Exception.THROW_*` code returned.

- - -
* public void evalFile(String filepath) throws Post4Exception

Read and evaluate the text file of Forth words given by `filepath`.  A `Post4Exception` will be thrown if a processing error occurs.

- - -
* public void evalString(String words) throws Post4Exception

Read and evaluate the string of Forth `words`.  A `Post4Exception` will be thrown if a processing error occurs.

- - -
* public Post4Stacks stacks()

Copies of data and float stacks are returned in a `Post4Stacks` object.  This can be called after `repl()`, `evalFile()`, or `evalString()` to examine the current state of the Post4 stacks for results.  Index zero (0) is the top of each stack.  The stacks can be empty.

- - -
#### Class Post4Exception

* public Post4Exception()
* public Post4Exception(int code)
* public Post4Exception(String message)

Create a new `Post4Exception` object.  Specifying a defined `THROW` code will also define an error message.  Undefined codes will have a default message of `(unknown)`.  Specifying a message will set the `THROW_GENERIC` code.

- - -
* public final int code

The read-only `THROW` code that generated this exception.

- - -
* public final static int THROW_*

`THROW` code constants.  `THROW_OK` (0) for no error, `THROW_GENERIC` (-4095) for an unknown, generic, unspecific exception.  The Forth standard `THROW` codes are defined on the range [-255, -1], system specific codes [-4095, -256]; otherwise outside of these ranges an application can use (..., -4096] and [1, ...) to limit of the `long` type.

- - -
#### Class Post4Options

* public Post4Options()

Create an options object with the system defaults, which can then be modified before creating `Post4` instance.

- - -
* public int ds_size
* public int fs_size
* public int rs_size

Define the Post4 machine's data (64), float (6), and/or return (64) stack sizes in cells.

- - -
* public int mem_size

Set the Post4 machine's memory size in kilobytes; default 128 KB.

- - -
* public String core_file

The file name of the core word definitions file; default `post4.p4`.  The environment variable `POST4_PATH` provides a colon separated search path for the core word definitions file and include files.  If `POST4_PATH` is undefined, then an OS specific default path is used.  This field can be set to an absolute file path to load a specific core definitions file.

- - -
* public String block_file

By default no block file is opened.  Use `block_file` to open a block file at start-up; otherwise see [OPEN-BLOCK](../doc/block.md) and [CLOSE-BLOCK](../doc/block.md) words.

- - -
* public String[] argv

An array of strings that can be used as arguments.  See [args](../doc/standard.md) word and the `dumpargs.p4` example.

- - -
#### Class Post4Stacks

* private Post4Stacks()

Cannot be created directly.  See `Post4.stacks()` above.

- - -
* public final long[] ds
* public final double[] fs

Given a `Post4Stacks` instance, examine copies of the data stack `.ds` and float stack `.fs`.  For example, a simple dump of both stacks:

        Post4Stacks stk = p4.stacks();
        System.out.print(String.format("ds[%d] ", stk.ds.length));
        for (long l : stk.ds) {
            System.out.print(String.format("%d ", l));
        }
        System.out.println();

        System.out.print(String.format("fs[%d] ", stk.fs.length));
        for (double d : stk.fs) {
            System.out.print(String.format("%.6e ", d));
        }
        System.out.println();


- - -
### Java Native Interface Words

These words provide a low level interface that allows Post4 to access Java object fields and call methods.  They essentially wrap the [JNI](https://docs.oracle.com/en/java/javase/17/docs/specs/jni/index.html).  It is still a work in progress and subject to change.

- - -
#### jArrayLength
( `jarray` -- `length` )  
Return the `length` of Java Array object `jarray`.

- - -
#### jBoxArray
( `xi-1` ... `x0` `i` -- `jarray` )  
Convert `i` stack items into a Java Array object `jarray`.

- - -
#### jBoxString
( `caddr` `u` -- `jstr` )  
Convert a string `caddr` `u` into a Java String object `jstr`, which must eventually be reclaimed (see `jDeleteLocalRef` or `jPopLocalFrame`).

- - -
#### jCall
( `x*i` `obj` `method` `m` `signature` `s` -- `y` | _ )  
Given a Java object or class reference, `obj`, invoke the given static or instance method denoted by strings `method` `m` `signature` `s`, passing arguments `x*i` based on the signature.  Return either a primitive value or object reference `y` from a non-void method or nothing in the case of a void return.

Note that arguments `x*i` are in reverse order to that of the method's signature, eg. left most argument is on top of the stack and right most towards the bottom of the stack.  String or array arguments must have been already "boxed" into objects.

- - -
#### jDeleteLocalRef
( `obj` -- )  
Release a local object reference previously obtained from `jFindClass`, `jBoxArray`, `jBoxString`, `jField`, or `jCall`.  See also `jPushLocalFrame` and `jPopLocalFrame`.

- - -
#### jField
( `obj` `field` `f` `signature` `s` -- `x` )  
Given a Java object or class reference, `obj`, fetch the primitive value or object `x` denoted by strings `field` `f` `signature` `s`.  Note that if `x` is a local object reference, it eventually needs to be reclaimed by `jDeleteLocalRef` or `jPopLocalFrame`.

- - -
#### jFindClass
( `name` `u` -- `class` )  
Given a class name `name` `u`, return a local object reference `class`  or zero (0) if not found.

- - -
#### jPopLocalFrame
( -- )  
End the local frame reclaiming any outstanding local object references.

- - -
#### jPushLocalFrame
( `u` -- )  
Create a new local object reference frame with capacity `u` greater than zero (0).  The frame and outstanding object references are reclaimed by `jPopLocalFrame`.  Note some JVM implementations may limit the maximum capacity.

Using `jPushLocalFrame` and `jPopLocalFrame` can help reduce release management of individual object references using `jDeleteLocalRef`.  Only concern is making a rough guess-timate as to how many local references will be assigned in the frame.

There is no JNI function to query available capacity.

- - -
#### jSetField
( `x` `obj` `field` `f` `signature` `s` -- )  
Given a Java object or class reference, `obj`, store a primitive value or object `x` denoted  by strings `field` `f` `signature` `s`.

- - -
#### jSetLocalCapacity
( `n` -- )  
Enlarge the local object reference capacity to hold `n` references.  Before a native method (`evalFile()`, `evalString()`, or `repl()`) is called, the Java VM ensures at least 16 local references can be created.  See [JNI EnsureLocalCapacity()](https://docs.oracle.com/en/java/javase/17/docs/specs/jni/functions.html#ensurelocalcapacity).

There is no JNI function to query available capacity.

- - -
#### jStringByteLength
( `jstr` -- `length` )  
Return the byte `length` of Java String object `jstr`.

- - -
#### jUnboxArray
( `jarray` -- `xi-1` ... `x0` `i` )  
Given a Java Array object `jarray`, return `i` items on the stack.

- - -
#### jUnboxString
( `jstr` -- `caddr` `u` )  
Given a Java String object `jstr`, return `u` bytes of allocate memory `caddr` containing a copy of the string.  The caller must eventually `FREE` the allocated memory `caddr`.

- - -
### java.lang.System Words

        ok INCLUDE java.lang.System.p4
        ok S" Bonjour le monde! " print jNewline jprints

- - -
#### jprintd
( F: `f`  -- )  
Print double precision float `f` using `java.lang.System.out.print(double)`.  See also `F.` and `FS.`.

- - -
#### jprintn
( `n` -- )  
Print signed integer `n` using `java.lang.System.out.print(long)`.  See also `.`.

- - -
#### jprints
( `jstr` -- )  
Print the Java String object using `java.lang.System.out.print(String)`.

- - -
#### print
( `caddr` `u` -- )  
Print the string `caddr` `u` using `java.lang.System.out.print(String)` by first "boxing" the string into an object.  See also `TYPE`.

- - -
### java.lang.Thread Words

        ok INCLUDE java.lang.thread.p4
        ok jCurrentThread DUP jThreadName TYPE SPACE jDeleteLocalRef

- - - 
#### jCurrentThread
( -- `thread` )  
Return a reference to the current Java Thread.

- - -
#### jDumpStack
( -- )  
Dump the current Thread's stack.

- - -
#### jMilliSleep
( `ms` -- )  
Sleep the current thread for `ms` milliseconds using `java.lang.Thread.sleep(long)`.

- - -
#### jNanoSleep
( `ns` `ms` -- )  
Sleep the current thread for `ms` milliseconds then `ns` nanoseconds [0, 999999] using `java.lang.Thread.sleep(long, int)`.

- - -
#### jThreadId
( `thread` -- `n` )  
Given a `thread` reference, return the thread's ID number `n`.

- - -
#### jThreadName
( `thread` -- `caddr` `u` )  
Given a `thread` reference, return the thread's name via a transient string buffer.
