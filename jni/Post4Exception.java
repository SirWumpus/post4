/*
 * Post4Exception.java
 *
 * Copyright 2023, 2024 by Anthony Howe.  All rights reserved.
 */

package post4.jni;

//import java.util.StringTokenizer;

public class Post4Exception extends Exception
{

	public final int code;

	public final static int THROW_OK			= (0);		/* No error. */

	/* -255..-1 reserved by the standard. */

	public final static int THROW_ABORT			= (-1);		/* ABORT */
	public final static int THROW_ABORT_MSG		= (-2);		/* ABORT" */
	public final static int THROW_DS_OVER		= (-3);		/* stack overflow */
	public final static int THROW_DS_UNDER		= (-4);		/* stack underflow */
	public final static int THROW_RS_OVER		= (-5);		/* return stack overflow */
	public final static int THROW_RS_UNDER		= (-6);		/* return stack underflow */
	public final static int THROW_LOOP_DEPTH	= (-7);		/* do-loops nested too deeply during execution */
	public final static int THROW_DICT_OVER		= (-8);		/* dictionary overflow */
	public final static int THROW_SIGSEGV		= (-9);		/* invalid memory address */
	public final static int THROW_DIV_ZERO		= (-10);	/* division by zero */
	public final static int THROW_ERANGE		= (-11);	/* result out of range */
	public final static int THROW_EINVAL		= (-12);	/* argument type mismatch */
	public final static int THROW_UNDEFINED		= (-13);	/* undefined word */
	public final static int THROW_COMPILE_ONLY	= (-14);	/* interpreting a compile-only word */
	public final static int THROW__15			= (-15);	/* invalid FORGET */
	public final static int THROW_EMPTY_NAME	= (-16);	/* attempt to use zero-length string as a name */
	public final static int THROW_PIC_OVER		= (-17);	/* pictured numeric output string overflow */
	public final static int THROW_PARSE_OVER	= (-18);	/* parsed string overflow */
	public final static int THROW_NAME_TOO_LONG	= (-19);	/* definition name too long */
	public final static int THROW__20			= (-20);	/* write to a read-only location */
	public final static int THROW_UNSUPPORTED	= (-21);	/* unsupported operation (e.g., AT-XY on a too-dumb terminal) */
	public final static int THROW_BAD_CONTROL	= (-22);	/* control structure mismatch */
	public final static int THROW_SIGBUS		= (-23);	/* address alignment exception */
	public final static int THROW_BAD_NUMBER	= (-24);	/* invalid numeric argument */
	public final static int THROW_RS_IMBALANCE	= (-25);	/* return stack imbalance */
	public final static int THROW__26			= (-26);	/* loop parameters unavailable */
	public final static int THROW__27			= (-27);	/* invalid recursion */
	public final static int THROW_SIGINT		= (-28);	/* user interrupt */
	public final static int THROW_COMPILING		= (-29);	/* compiler nesting */
	public final static int THROW__30			= (-30);	/* obsolescent feature */
	public final static int THROW_NOT_CREATED	= (-31);	/* word not defined by CREATE */
	public final static int THROW_BAD_NAME		= (-32);	/* invalid name argument (e.g., TO xxx) */
	public final static int THROW_BLOCK_RD		= (-33);	/* block read exception */
	public final static int THROW_BLOCK_WR		= (-34);	/* block write exception */
	public final static int THROW_BLOCK_BAD		= (-35);	/* invalid block number */
	public final static int THROW__36			= (-36);	/* invalid file position */
	public final static int THROW_EIO			= (-37);	/* file I/O exception */
	public final static int THROW_ENOENT		= (-38);	/* non-existent file */
	public final static int THROW__39			= (-39);	/* unexpected end of file */
	public final static int THROW_BAD_BASE		= (-40);	/* invalid BASE for floating point conversion */
	public final static int THROW__41			= (-41);	/* loss of precision */
	public final static int THROW__42			= (-42);	/* floating-point divide by zero */
	public final static int THROW__43			= (-43);	/* floating-point result out of range */
	public final static int THROW_FS_OVER		= (-44);	/* floating-point stack overflow */
	public final static int THROW_FS_UNDER		= (-45);	/* floating-point stack underflow */
	public final static int THROW__46			= (-46);	/* floating-point invalid argument */
	public final static int THROW__47			= (-47);	/* compilation word list deleted */
	public final static int THROW__48			= (-48);	/* invalid POSTPONE */
	public final static int THROW__49			= (-49);	/* search-order overflow */
	public final static int THROW__50			= (-50);	/* search-order underflow */
	public final static int THROW__51			= (-51);	/* compilation word list changed */
	public final static int THROW__52			= (-52);	/* control-flow stack overflow */
	public final static int THROW__53			= (-53);	/* exception stack overflow */
	public final static int THROW__54			= (-54);	/* floating-point underflow */
	public final static int THROW_SIGFPE		= (-55);	/* floating-point unidentified fault */
	public final static int THROW_QUIT			= (-56);	/* QUIT */
	public final static int THROW__57			= (-57);	/* exception in sending or receiving a character */
	public final static int THROW__58			= (-58);	/* [IF], [ELSE], or [THEN] exception */
	public final static int THROW_ALLOCATE		= (-59);	/* ALLOCATE */
	public final static int THROW__60			= (-60);	/* FREE */
	public final static int THROW_RESIZE		= (-61);	/* RESIZE */
	public final static int THROW__62			= (-62);	/* CLOSE-FILE */
	public final static int THROW__63			= (-63);	/* CREATE-FILE */
	public final static int THROW__64			= (-64);	/* DELETE-FILE */
	public final static int THROW__65			= (-65);	/* FILE-POSITION */
	public final static int THROW__66			= (-66);	/* FILE-SIZE */
	public final static int THROW__67			= (-67);	/* FILE-STATUS */
	public final static int THROW__68			= (-68);	/* FLUSH-FILE */
	public final static int THROW__69			= (-69);	/* OPEN-FILE */
	public final static int THROW__70			= (-70);	/* READ-FILE */
	public final static int THROW__71			= (-71);	/* READ-LINE */
	public final static int THROW__72			= (-72);	/* RENAME-FILE */
	public final static int THROW__73			= (-73);	/* REPOSITION-FILE */
	public final static int THROW__74			= (-74);	/* RESIZE-FILE */
	public final static int THROW__75			= (-75);	/* WRITE-FILE */
	public final static int THROW__76			= (-76);	/* WRITE-LINE */
	public final static int THROW__77			= (-77);	/* Malformed xchar */
	public final static int THROW__78			= (-78);	/* SUBSTITUTE */
	public final static int THROW__79			= (-79);	/* REPLACES */
	public final static int THROW_future		= (-80);	/* -80 .. -4096 reserved for future assignment */

	/* -4095..-256 reserved for the system (that's us). */

	public final static int THROW_GENERIC		= (-4095);	/* Unknown, generic, WTF. */

	private final static String[] messages = {
		"ok",
		"ABORT",
		"ABORT\"",
		"stack overflow",
		"stack underflow",
		"return stack overflow",
		"return stack underflow",
		"do-loops nested too deeply during execution",
		"dictionary overflow",
		"invalid memory address",
		"division by zero",
		"result out of range",
		"argument type mismatch",
		"undefined word",
		"interpreting a compile-only word",
		"invalid FORGET",
		"attempt to use zero-length string as a name",
		"pictured numeric output string overflow",
		"parsed string overflow",
		"definition name too long",
		"write to a read-only location",
		"unsupported operation",
		"control structure mismatch",
		"address alignment exception",
		"invalid numeric argument",
		"return stack imbalance",
		"loop parameters unavailable",
		"invalid recursion",
		"user interrupt",
		"compiler nesting",
		"obsolescent feature",
		"word not defined by CREATE",
		"invalid name argument",
		"block read exception",
		"block write exception",
		"invalid block number",
		"invalid file position",
		"file I/O exception",
		"non-existent file",
		"unexpected end of file",
		"invalid BASE for floating point conversion",
		"loss of precision",
		"floating-point divide by zero",
		"floating-point result out of range",
		"floating-point stack overflow",
		"floating-point stack underflow",
		"floating-point invalid argument",
		"compilation word list deleted",
		"invalid POSTPONE",
		"search-order overflow",
		"search-order underflow",
		"compilation word list changed",
		"control-flow stack overflow",
		"exception: stack overflow",
		"floating-point underflow",
		"floating-point unidentified fault",
		"QUIT",
		"exception: in sending or receiving a character",
		"[IF], [ELSE], or [THEN] exception",
		"ALLOCATE",
		"FREE",
		"ALLOT or RESIZE",
		"CLOSE-FILE",
		"CREATE-FILE",
		"DELETE-FILE",
		"FILE-POSITION",
		"FILE-SIZE",
		"FILE-STATUS",
		"FLUSH-FILE",
		"OPEN-FILE",
		"READ-FILE",
		"READ-LINE",
		"RENAME-FILE",
		"REPOSITION-FILE",
		"RESIZE-FILE",
		"WRITE-FILE",
		"WRITE-LINE",
		"Malformed xchar",
		"SUBSTITUTE",
		"REPLACES",
	};

	public Post4Exception()
	{
		this(THROW_GENERIC);
	}

	public Post4Exception(int code)
	{
		super(code+" thrown: "+(code <= 0 ? messages[-code] : "(unknown)"));
		this.code = code;
	}

	public Post4Exception(String message)
	{
		super(message);
		this.code = THROW_GENERIC;
	}

//	protected Post4Exception(String message)
//	{
//		super(message);
//
//		// Assuming message starts with a leading number, eg. "-13 thrown",
//		// extract the throw code.  Code 1 reserved for: unknown, generic, WTF.
//		StringTokenizer st = new StringTokenizer(message);
//		if (st.hasMoreTokens()) {
//			try {
//				this.code = Integer.parseInt(st.nextToken());
//			} catch (NumberFormatException e) {
//				this.code = THROW_GENERIC;
//			}
//		}
//	}
}
