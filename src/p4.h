/*
 * p4.h
 *
 * Copyright 2007, 2010 by Anthony Howe. All rights reserved.
 */

#ifndef __post4_h__
#define __post4_h__	1

#ifdef __cplusplus
extern "C" {
#endif

/***********************************************************************
 ***
 ***********************************************************************/

#ifndef P4_BLOCK_SIZE
#define P4_BLOCK_SIZE			1024		/* in bytes */
#endif

#ifndef P4_STACK_SIZE
# ifdef NDEBUG
#  define P4_STACK_SIZE			64		/* in CELLS */
# else
#  define P4_STACK_SIZE			10
# endif
#endif

/*
 * See ANS-Forth 93 section 3.3.1.2.  Length byte + 31 chars + NUL byte.
 * Note too that the high bit of the name's length byte is used for the
 * immediate flag. See P4_WORD_ macros. The allows for max. name size
 * of 128 bytes.
 */
#ifndef P4_NAME_SIZE
#define P4_NAME_SIZE			32
#endif

#ifndef P4_STRING_SIZE
#define P4_STRING_SIZE			256
#endif

#ifndef P4_INPUT_SIZE
#define P4_INPUT_SIZE			256
#endif

#ifndef P4_SAFE_PATH
#define P4_SAFE_PATH			"/bin:/usr/bin"
#endif

/***********************************************************************
 *** No configuration below this point.
 ***********************************************************************/

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <setjmp.h>

#ifdef HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif

#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#if HAVE_INTTYPES_H
# include <inttypes.h>
#else
# if HAVE_STDINT_H
# include <stdint.h>
# endif
#endif

#ifdef HAVE_TERMIOS_H
# include <termios.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

/***********************************************************************
 ***
 ***********************************************************************/

#define QUOTE(x)	QUOTE_THIS(x)
#define QUOTE_THIS(x)	#x

/***********************************************************************
 *** Types
 ***********************************************************************/

#ifndef HAVE_GID_T
typedef int gid_t;
#endif
#ifndef HAVE_MODE_T
typedef int mode_t;
#endif
#ifndef HAVE_OFF_T
typedef long off_t;
#endif
#ifndef HAVE_PID_T
# ifdef __MINGW32__
typedef HANDLE pid_t;
# else
typedef int pid_t;
# endif
#endif
#ifndef HAVE_UID_T
typedef int uid_t;
#endif
#ifndef HAVE_SIZE_T
typedef unsigned size_t;
#endif
#ifndef HAVE_SSIZE_T
typedef int ssize_t;
#endif
#ifndef HAVE_TIME_T
typedef unsigned long time_t;
#endif

/* Base C types. */
typedef void *P4_Pointer;
#define P4_POINTER_FMT "0x%.8lx"

typedef unsigned char P4_Byte;
#define P4_BYTE_FMT "%u"

typedef unsigned long P4_Unsigned;
#define P4_UNSIGNED_FMT "%lu"

typedef signed long P4_Signed;
#define P4_SIGNED_FMT "%ld"

typedef size_t P4_Size;
#define P4_SIZE_FMT "%lu"

typedef union p4_cell P4_Cell;
typedef struct p4_data P4_Data;
typedef struct p4_word P4_Word;
typedef struct p4_context P4_Context;
typedef void (*P4_Func)(P4_Context *);

typedef struct {
	P4_Unsigned	length;		/* Length on string less NUL byte. */
	P4_Byte * 	string;		/* Pointer to content plus terminating NUL byte. */
} P4_String;

typedef struct {
	P4_Size		size;		/* Size of table in cells. */
	P4_Cell *	base;		/* Base of array; might be reallocated. */
	P4_Cell *	top;		/* Last element in the array. */
} P4_Array;

typedef struct {
	FILE *		fp;		/* stdin or an open file, never NULL. */
	P4_Signed	fd;		/* -1=string 0=stdin otherwise file descriptor */
	P4_Unsigned	blk;
	P4_Unsigned	offset;		/* Offset of unconsumed input. */
	P4_Unsigned	length;		/* Length of input in buffer. */
	P4_Byte	*	buffer;
} P4_Input;

typedef struct p4_xt {
	P4_Func		code;		/* The "code field" is a C function. */
	P4_Data *	data;		/* The "data field", can be NULL */
} * P4_Exec_Token;

/*
 * See ANS-Forth 1994 section E.4.1.
 */
struct p4_word {
	/* Using the high-bit of the name.length used in traditional
	 * CPU oriented Forths seemed archane in a portable C version
	 * on a modern processor; clarity before compactness (OK, for
	 * an IOCCC winner that might be an odd concept). Maybe short
	 * cuts come later in version 2 after proof of concept.
	 */
	P4_Unsigned	bits;

#define P4_BIT_IMM			0x00000001
#define P4_BIT_ALIAS			0x00000002
#define P4_BIT_COMPILE_ONLY		0x00000004
#define P4_WORD_IS_IMM(w)		((w)->bits & P4_BIT_IMM)
#define P4_WORD_SET_IMM(w)		((w)->bits |= P4_BIT_IMM)
#define P4_WORD_CLEAR_IMM(w)		((w)->bits &= ~P4_BIT_IMM)

	P4_String	name;		/* Allocated string, never moves. */
	P4_Word *	prev;		/* Next word definition. */
	P4_Exec_Token	xt;
};

#define P4_WORD_DO(name) \
	p4_do_ ## name (ctx)

#define P4_WORD_XT(name) \
	&p4_xt_ ## name

#define P4_WORD_COMPILE(ctx, name) \
	P4_PUSH((ctx)->ds).xt = P4_WORD_XT(name); \
	P4_WORD_DO(COMMA);

#define P4_WORD_DEFINE(name) \
	void p4_do_ ## name (P4_Context *ctx)

#define P4_WORD_DECL(scope, name) \
	scope struct p4_xt p4_xt_ ## name ; \
	scope void p4_do_ ## name (P4_Context *)

#define P4_WORD_NAME(name, prev, bits) \
	struct p4_xt p4_xt_ ## name = { p4_do_ ## name, NULL }; \
	P4_Word p4_word_ ## name = { bits, { sizeof (#name)-1, #name }, &p4_word_ ## prev, &p4_xt_ ## name }

#define P4_WORD_TEXT(name, prev, bits, text) \
	struct p4_xt p4_xt_ ## name = { p4_do_ ## name, NULL }; \
	P4_Word p4_word_ ## name = { bits, { sizeof (text)-1, text   }, &p4_word_ ## prev, &p4_xt_ ## name }

#define P4_WORD_ALIAS(alias, prev) \
	P4_Word p4_word_ ## alias = { P4_BIT_ALIAS, { sizeof (#alias)-1, #alias }, &p4_word_ ## prev, &p4_xt_ ## prev }

union p4_cell {
	P4_Cell *	a;
	P4_Word *	w;
	P4_Byte *	s;
	P4_Signed	n;
	P4_Unsigned	u;
	P4_Pointer	p;
	P4_Exec_Token	xt;
};

#define P4_ADDRESS_UNIT	(sizeof (P4_Byte))
#define P4_CELL		(sizeof (P4_Cell))

struct p4_data {
	P4_Size		size;
	P4_Size		length;
	P4_Byte 	base[P4_CELL];
};

struct p4_context{
	P4_Exec_Token *	ip;		/* Pointer to next xt. */
	P4_Array	rs;		/* Return stack rs->base = ctx->size - rs->size */
	P4_Array	ds;		/* Data stack ds->base = ctx->size - rs->size - ds->size */
	P4_Array	noname;		/* Stack of noname xt and associated data-space. */
	P4_Exec_Token	xt;		/* Current xt being compiled */
	P4_Word *	word;		/* Current word being compiled. */
	P4_Word *	words;		/* Head of the dictionary word list. */
	P4_Signed	ibase;		/* Input radix */
	P4_Signed	obase;		/* Output radix */
	P4_Signed	state;		/* 0=interpret, 1=compile */
	P4_Byte		console[P4_INPUT_SIZE];
	P4_Input	input;
	P4_Unsigned	jmp_set;

#define P4_JMP_ABORT			0x00000001
#define P4_JMP_QUIT			0x00000002
#define P4_JMP_THROW			0x00000004

	JMP_BUF		on_throw;
	JMP_BUF		on_abort;
	JMP_BUF		on_quit;
	P4_Signed	sig_int;
	struct termios	saved_tty;
};

extern P4_Cell p4_null_cell;

/* These stack operation grow from the base upwards. */
#define P4_TOP(table)			(*(table).top)
#define P4_POP(table)			(*(table).top--)
#define P4_PUSH(table)			(*++(table).top)
#define P4_POP_SAFE(table)		(P4_IS_EMPTY(table) ? p4_null_cell : P4_POP(table))
#define P4_PUSH_SAFE(table)		p4ArrayGrow(&table, 1, P4_STACK_SIZE), (*++(table).top)
#define P4_PEEK(table, offset)		(table).top[(offset)]
#define P4_POKE(table, offset)		(table).top[(offset)]
#define P4_IS_EMPTY(table)		((table).top < table.base)
#define P4_LENGTH(table)		((table).top - (table).base + 1)
#define P4_IS_FULL(table)		((table).size <= P4_LENGTH(table))
#define P4_SET_DEPTH(table, depth)	((table).top = &(table).base[(depth)-1])
#define P4_RESET(table)			P4_SET_DEPTH(table, 0)

/* These stack operation grow from the base upwards. */
#define P4_TOP_(table)			(*(table)->top)
#define P4_POP_(table)			(*(table)->top--)
#define P4_PUSH_(table)			(*++(table)->top)
#define P4_POP_SAFE_(table)		(P4_IS_EMPTY(table) ? p4_null_cell : P4_POP(table))
#define P4_PUSH_SAFE_(table)		p4ArrayGrow(table, 1, P4_STACK_SIZE), (*++(table)->top)
#define P4_PEEK_(table, offset)		(table)->top[(offset)]
#define P4_POKE_(table, offset)		(table)->top[(offset)]
#define P4_IS_EMPTY_(table)		((table)->top < table->base)
#define P4_LENGTH_(table)		((table)->top - (table)->base + 1)
#define P4_IS_FULL_(table)		((table)->size <= P4_LENGTH_(table))
#define P4_SET_DEPTH_(table, depth)	((table)->top = &(table)->base[(depth)-1])
#define P4_RESET_(table)		P4_SET_DEPTH_(table, 0)

#define P4_IS_COMPILING(ctx)		((ctx)->state != 0)
#define P4_IS_INTERPRETING(ctx)		((ctx)->state == 0)

#define P4_CELL_ALIGN(nbytes)		((((nbytes) - 1)  | (sizeof (P4_Cell)-1)) + 1)

#define P4_SETJMP_PUSH(ctx, this_jb) \
	{ JMP_BUF jb; P4_Unsigned jmp_set = (ctx)->jmp_set; memcpy(&jb, this_jb, sizeof (jb))

#define P4_SETJMP_POP(ctx, this_jb) \
	memcpy(this_jb, &jb, sizeof (jb)); (ctx)->jmp_set = jmp_set; }

#define P4_INPUT_PUSH(this_input)	{ P4_Input old_input = *this_input;
#define P4_INPUT_POP(this_input)	*this_input = old_input; }

/***********************************************************************
 *** Array API
 ***********************************************************************/

/**
 * @param table
 *	A pointer to a static P4_Array.
 *
 * @param size
 *	The size of the initial array in cells.
 *
 * @return
 *	Zero on success, otherwise -1 on error. It is the caller's
 *	responsibility to p4ArrayRelease() this array when done.
 */
extern int p4ArrayAssign(P4_Array *table, size_t size);

/**
 * @param table
 *	A pointer to a static P4_Array to clean-up.
 *
 * @see
 *	p4ArrayAssign()
 */
extern void p4ArrayRelease(P4_Array *table);

/**
 * @param size
 *	The size of the initial dynamic array in cells.
 *
 * @return
 *	A pointer to a dynamic P4_Array, otherwise NULL on error.
 *	It is the caller's responsibility to p4ArrayFree this
 *	pointer when done.
 */
extern P4_Array *p4ArrayCreate(size_t size);

/**
 * @param _table
 *	A pointer to a dynamic P4_Array to free.
 *
 * @see
 *	p4ArrayCreate()
 */
extern void p4ArrayFree(void *_table);

/**
 * @param table
 *	Pointer to a P4_Array structure.
 *
 * @param count
 *	Number of table cells required.
 *
 * @param extra
 *	Number of addtitional table cells to append to the table when
 *	there is insufficient space remaining to satisfy the count
 *	required.
 */
extern void p4ArrayGrow(P4_Array *table, size_t count, size_t extra);

#define p4GrowDS(ctx, require)		p4ArrayGrow(&(ctx)->ds, (require), P4_STACK_SIZE)
#define p4GrowRS(ctx, require)		p4ArrayGrow(&(ctx)->rs, (require), P4_STACK_SIZE)

extern void p4ArrayPick(P4_Array *table, P4_Unsigned index);
extern void p4ArrayRoll(P4_Array *table, P4_Unsigned count);
extern void p4ArrayDump(P4_Array *table, FILE *fp);

/***********************************************************************
 *** Conversion API
 ***********************************************************************/

/**
 * @param ch
 *	A backslash escaped character.
 *
 * @return
 *	The value that the backslash escape character represents.
 *
 *	\a	bell
 *	\b	backspace
 *	\e	escape
 *	\f	formfeed
 *	\n	linefeed
 *	\r	carriage-return
 *	\s	space
 *	\t	tab
 *	\v	vertical tab
 *	\?	delete
 *
 *	Other characters remain as themselves.
 */
extern int p4CharLiteral(int ch);

/**
 * @param number
 *	A number.
 *
 * @param base
 *	The conversion radix between 2 and 36 inclusive.
 *
 * @param buffer
 *	A pointer to a buffer in which to store the string
 *	representation of the number.
 *
 * @param size
 *	The size of the buffer.
 *
 * @return
 *	A number.
 */
extern P4_Size p4SignedToString(P4_Signed number, P4_Unsigned base, P4_Byte *buffer, P4_Size size);
extern P4_Size p4UnsignedToString(P4_Unsigned number, P4_Unsigned base, P4_Byte *buffer, P4_Size size);

/**
 * @param s
 *	A pointer to a C string representing a number.
 *
 * @param stop
 *	If not NULL, then a pointer within the C string
 *	of the first invalid character is stored here.
 *
 * @param base
 *	The conversion radix between 2 and 36 inclusive.
 *
 * @return
 *	A number.
 */
extern P4_Signed p4StringToSigned(const P4_Byte *s, P4_Byte **stop, P4_Unsigned base);

/**
 * @param s
 *	A pointer to a C string to convert in place to upper case.
 *
 * @param length
 *	The length of the C string.
 */
extern void p4StringToUpper(P4_Byte *s, P4_Size length);

/**
 * @param s
 *	A pointer to a C string to reverse in place.
 *
 * @param length
 *	The length of the C string.
 */
extern void p4StringReverse(P4_Byte *s, P4_Size length);

/***********************************************************************
 *** Kernel API
 ***********************************************************************/

/**
 * Initialise the global environment.
 */
extern void p4Init(void);

/**
 * Create a new interpreter context.
 *
 * @return
 *	A pointer to an allocated P4_Context structure.
 */
extern P4_Context *p4Create(void);

/**
 * @param _ctx
 *	A pointer to an allocated P4_Context structure to free.
 */
extern void p4Free(void *_ctx);

/**
 * @param ctx
 *	A pointer to an allocated P4_Context structure.
 *
 * @param fd
 *	A open file descriptor
 *
 * @return
 *	Zero on success, 1 on BYE, otherwise -1 on file error or abort.
 */
extern int p4EvalFd(P4_Context *ctx, P4_Signed fd);

/**
 * @param ctx
 *	A pointer to an allocated P4_Context structure.
 *
 * @param filepath
 *	A C string of a file path name to interpret.
 *	If NULL, then standard input will be read.
 *
 * @return
 *	Zero on success, 1 on BYE, otherwise -1 on file error or abort.
 */
extern int p4EvalFile(P4_Context *ctx, const char *filepath);

/**
 * @param ctx
 *	A pointer to an allocated P4_Context structure.
 *
 * @param string
 *	A C string to interpret.
 *
 * @return
 *	Zero on success, 1 on BYE, otherwise -1 on abort.
 */
extern int p4EvalString(P4_Context *ctx, const char *string);

/***********************************************************************
 *** Convenience Functions
 ***********************************************************************/

extern P4_Signed p4GetC(P4_Context *ctx);
extern P4_Unsigned p4GetLine(P4_Context *ctx, P4_Byte *buffer, P4_Size size);
extern P4_Unsigned p4InputLine(FILE *fp, P4_Byte *buffer, P4_Size size);

/**
 * Handles parsing of "ccc<char>".
 *
 * @param input
 *	A pointer to an already filled input buffer.
 *
 * @param delim
 *	Input is read until this delimiter is seen. If delimiter
 *	is space ( ), then treat all white space as a delimiter.
 *
 * @param escape
 *	When true, allow for backslash escaped characters.
 *
 * @return
 *	A P4_String structure containing a pointer within the
 *	input buffer and the length of the string upto the delim
 *	character.
 *
 * @note
 *	Backslash escaped characters are converted to a literal and
 *	the input buffer reduced in size.
 *
 * @standard ANS-Forth 1994, extended
 */
extern P4_String p4Parse(P4_Input *input, P4_Unsigned delim, P4_Unsigned escape);

/**
 * Handles parsing of "ccc<char>", skipping leading occurences of <char>.
 *
 * @param ctx
 *	A pointer to an allocated P4_Context structure.
 *
 * @param delim
 *	Skip leading occurences of this delimiter character, after which
 *	input continues until the delimiter is seen again or the buffer
 *	is filled. If delimiter is -1, then treat all white space as a
 *	delimiter.
 *
 * @param literal
 *	True if the input should be treated literally, ignoring any
 *	backslash escape sequences.
 *
 * @param buffer
 *	A buffer to hold a counted string read from standard input. The
 *	first octet of the buffer will hold the string's length and the
 *	buffer will always be null terminated. Thus the maximum possible
 *	string length is P4_NAME_SIZE-2 bytes.
 *
 * @param size
 *	Size of the buffer.
 *
 * @return
 *	The length of the parsed input; otherwise 0 on EOF or error.
 */
extern P4_String p4ParseWord(P4_Input *input);

extern P4_Word *p4FindWord(P4_Context *ctx, P4_Byte *caddr, P4_Size length);
extern P4_Word *p4FindXt(P4_Context *ctx, P4_Exec_Token xt);
extern P4_Signed p4IsNoname(P4_Context *ctx, P4_Exec_Token xt);
extern void p4Dump(FILE *fp, P4_Byte *addr, P4_Size length);

/***********************************************************************
 *** Core Words & Actions
 ***********************************************************************/

extern void *p4_program_end;

/***********************************************************************
 *** END
 ***********************************************************************/

#ifdef  __cplusplus
}
#endif

#endif /* __post4_h__ */
