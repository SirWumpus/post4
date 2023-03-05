Post4 (Post-Forth)
==================

Copyright 2007, 2023 Anthony Howe.  All rights reserved.


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

By default no block file is opened.  Use `block_file` to open a block file at start-up; otherwise see [BLOCK-OPEN](../doc/block.md) and [BLOCK-CLOSE](../doc/block.md) words.

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
### Java Words

These words provide a low level interface that allows Post4 to query Java object fields and call methods.  They essentially wrap the [JNI](https://docs.oracle.com/en/java/javase/17/docs/specs/jni/index.html).  It is still a work in progress and subject to change.

- - -
#### jBoxString
( caddr u -- jstr )  

- - -
#### jCall
( x*i obj method m signature s -- y | )  

- - -
#### jDeleteLocalRef
( obj -- class )  

- - -
#### jField
( obj field f signature s -- x )  

- - -
#### jFindClass
( name u -- class )  

- - -
#### jObjectClass
( obj -- class )  

- - -
#### jPushLocalFrame
( n -- )  

- - -
#### jPopLocalFrame
( -- )  

- - -
#### jSetField
( x obj field f signature s -- )  

- - -
#### jSetLocalCapacity
( n -- )  

- - -
#### jUnboxString
( jstr -- caddr u )  

- - -
### java.lang.System Words

        ok INCLUDE java.lang.System.p4
        ok S" Bonjour le monde! " jprints

- - -
#### jprints
( caddr u -- )  

- - -
#### jprintn
( n -- )  

- - -
#### jprintd
( F: f  -- )  

- - -
### java.lang.Thread Words

        ok INCLUDE java.lang.thread.p4
        ok jCurrentThread DUP jThreadName TYPE SPACE jDeleteLocalRef

- - - 
#### jCurrentThread
( -- thread )  

- - -
#### jDumpStack
( -- )  

- - -
#### jMilliSleep
( ms -- )  

- - -
#### jNanoSleep
( ms ns -- )  

- - -
#### jThreadId
( thread -- n )  

- - -
#### jThreadName
( thread -- caddr u )  
