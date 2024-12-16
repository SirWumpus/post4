/*
 * post4.c
 *
 * Copyright 2007, 2024 by Anthony Howe. All rights reserved.
 */

#include "post4.h"
#include "aline.h"

/***********************************************************************
 *** Globals
 ***********************************************************************/

const char p4_commit[] = P4_COMMIT;

/* Expected Forth defined words. */
P4_Word *p4_hook_call, *p4_throw, *p4_flit, *p4_2lit;

static P4_Word *p4_builtin_words;

#define P4_INTERACTIVE(ctx)	(ctx->state == P4_STATE_INTERPRET && is_tty && P4_INPUT_IS_TERM(ctx->input))

#ifdef USE_EXCEPTION_STRINGS
/*
 * See P4_THROW_*.
 *
 * Wastes a lot of space with too many (unused) errors.  The Forth
 * standard goes too far in granularity of errors compared to C and
 * POSIX.  If you really want pretty descriptive error reporting:
 *
 *	CFLAGS="-DUSE_EXCEPTION_STRINGS" ./configure
 *	make clean build
 */
const char *p4_exceptions[] = {
	"",
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
	"invalid argument / type mismatch",
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
	NULL
};
#endif

static const char crlf[] = "\r\n";

/***********************************************************************
 *** Context
 ***********************************************************************/

/**
 */
void
p4Init(P4_Options *opts)
{
	alineInit(opts->hist_size);
}

P4_String
p4FindFilePath(const char *path_list, size_t plen, const char *file, size_t flen)
{
	struct stat sb;
	char *paths, *path, *next;
	P4_String str = { 0, NULL };
	errno = 0;
	if (file == NULL || *file == '\0') {
		errno = EINVAL;
		goto error0;
	}
	if ((str.string = calloc(1, PATH_MAX)) == NULL) {
		goto error0;
	}
	/* Try the given file relative the current working directory. */
	str.length = snprintf(str.string, PATH_MAX, "%.*s", (int)flen, file);
	if (stat(str.string, &sb) == 0) {
		return str;
	}
	/* Path list supplied or use the default? */
	if (path_list == NULL || *path_list == '\0') {
		plen = STRLEN(P4_CORE_PATH);
		path_list = P4_CORE_PATH;
	}
	/* Need a duplicate because strtok modifies the string with NULs. */
	if ((paths = strndup(path_list, plen)) == NULL) {
		goto error1;
	}
	/* Search "dir0:dir1:...:dirN" string. */
	for (next = paths; (path = strtok(next, ":")) != NULL; next = NULL) {
		str.length = snprintf(str.string, PATH_MAX, "%s/%s", path, file);
		if (stat(str.string, &sb) == 0) {
			errno = 0;
			return str;
		}
	}
	free(str.string);
	str.string = NULL;
	str.length = 0;
error1:
	free(paths);
error0:
	return str;
}

/***********************************************************************
 *** Conversion API
 ***********************************************************************/

static const char escape_map[] = "s a\ab\be\033f\fn\nr\rt\tv\v?\177\"\"\\\\z\x00";

/**
 * @param ch
 *	A C-style backslash escaped character.
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
 *	\z	nul
 *	\?	delete
 *
 *	Other characters remain as themselves.
 */
int
p4CharLiteral(int ch)
{
	for (const char *map = escape_map; *map != '\0'; map += 2) {
		if (ch == map[0]) {
			return map[1];
		}
	}
	return ch;
}

int
p4Base36(int digit)
{
	if ('0' <= digit && digit <= '9') {
		return digit - '0';
	}
	digit = toupper(digit);
	if ('A' <= digit && digit <= 'Z') {
		return digit - 'A' + 10;
	}
	return 127;
}

/***********************************************************************
 *** Utility
 ***********************************************************************/

P4_String
p4Parse(P4_Input *input, int delim, int escape)
{
	int ch;
	P4_Uint offset;
	P4_String parsed;

	parsed.length = 0;
	parsed.string = &input->buffer[input->offset];

	for (offset = input->offset; offset < input->length; offset++) {
		ch = input->buffer[offset];

		if (escape && ch == '\\' && offset+1 < input->length) {
			/* Remove backslash escape from input buffer. */
			memmove(input->buffer+offset, input->buffer+offset+1, input->length-offset+1);
			/* Convert the escaped character. */
			input->buffer[offset] = p4CharLiteral(input->buffer[offset]);
			/* The input buffer is less one byte, ie. the backslash. */
			input->length--;
			continue;
		}

		/* Treat a space as indicating a space or any control character.
		 * See 3.4.1.1 Delimiters
		 */
		if (ch == delim || (delim == ' ' && iscntrl(ch))) {
			/* Do NOT terminate the parsed string, since the
			 * source might be a block buffer or some read-only
			 * string, which cannot be modified.
			 */
			break;
		}
	}

	/* Length of parsed string less the delimiter. */
	parsed.length = offset - input->offset;
	/* Input offset just after delimiter or end of buffer. */
	input->offset = offset + (offset < input->length);

	return parsed;
}

P4_String
p4ParseName(P4_Input *input)
{
	/* Skip leading white space. */
	for ( ; input->offset < input->length; input->offset++) {
		if (!isspace(input->buffer[input->offset])) {
			break;
		}
	}

	return p4Parse(input, ' ', 0);
}

/*
 * A nanosecond 1000000000L
 */
void
p4Nap(P4_Uint seconds, P4_Uint nanoseconds)
{
#if defined(__WIN32__)
	Sleep(seconds * 1000 + nanoseconds / 1000000);
#elif defined (HAVE_NANOSLEEP)
{
	struct timespec ts0, ts1, *sleep_time, *unslept_time, *tmp;

	sleep_time = &ts0;
	unslept_time = &ts1;
	ts0.tv_sec = seconds;
	ts0.tv_nsec = nanoseconds;

	while (nanosleep(sleep_time, unslept_time)) {
		tmp = sleep_time;
		sleep_time = unslept_time;
		unslept_time = tmp;
	}
}
#else
{
	unsigned unslept;

	while (0 < (unslept = sleep(seconds))) {
		seconds = unslept;
	}
}
#endif
}

void
p4StackDump(FILE *fp, P4_Cell *base, int length)
{
	P4_Cell *cell;
	unsigned count;

	if (length < 0 || 1024 <= length) {
		(void) fprintf(fp, "stack under or over flow, depth=%d\r\n", length);
		return;
	}
	for (count = 0, cell = base; 0 < length--; cell++) {
		if ((count & 3) == 0) {
			(void) fprintf(fp, "top-%.2u ", (unsigned) length);
		}
		(void) fprintf(fp, P4_H0X_FMT" ", cell->u);
		if ((++count & 3) == 0) {
			(void) fprintf(fp, crlf);
		}
	}
	if ((count & 3) != 0) {
		(void) fprintf(fp, crlf);
	}
}

/***********************************************************************
 *** Double Cell Math
 ***********************************************************************/

// Affected core words:
//
//	constants: MAX-D MAX-UD
//	intermediate results: */ */MOD
//	explicit arguments: M* UM* UM/MOD SM/REM FM/MOD S>D <# # #S #> >NUMBER
//

/*
 * https://stackoverflow.com/questions/22845801/32-bit-signed-integer-multiplication-without-using-64-bit-data-type
 *
 * 0x7fffffffffffffff 0x7fffffffffffffff UM*	u1 u2
 * 0x0000000000000001 0x3fffffffffffffff	lo hi
 *
 * 0xffffffffffffffff 0xffffffffffffffff UM*	u1 u2
 * 0x0000000000000001 0xfffffffffffffffe	lo hi
 *
 * 0xdeadbeefdeadbeef 0xbeefdeadbeefdead UM*	u1 u2
 * 0x3a522ca1ca1e4983 0xa615999d16497cbb	lo hi
 */
void
p4Mulu(P4_Uint a, P4_Uint b, P4_Uint *c0, P4_Uint *c1)
{
	/* Word halves */
	P4_Uint al = (P4_Uint_Half) a;
	P4_Uint ah = a >> P4_HALF_SHIFT;
	P4_Uint bl = (P4_Uint_Half) b;
	P4_Uint bh = b >> P4_HALF_SHIFT;

	/* Partial products. */
	P4_Uint al_bl = al * bl;
	P4_Uint al_bh = al * bh;
	P4_Uint ah_bl = ah * bl;
	P4_Uint ah_bh = ah * bh;

	/* Sum partial products. */
	*c0 = (al_bh << P4_HALF_SHIFT) + al_bl;
	P4_Uint carry = *c0 < al_bl;
	*c0 += (ah_bl << P4_HALF_SHIFT);
	carry += *c0 < (ah_bl << P4_HALF_SHIFT);
	*c1 = ah_bh + (ah_bl >> P4_HALF_SHIFT) + (al_bh >> P4_HALF_SHIFT) + carry;
}

/*
 * https://stackoverflow.com/questions/22845801/32-bit-signed-integer-multiplication-without-using-64-bit-data-type
 *
 * +ve * +ve = +ve
 * 0x7fffffffffffffff 0x7fffffffffffffff M*	n1 n2
 * 0x0000000000000001 0x3fffffffffffffff	lo hi
 *
 * -ve * -ve = +ve
 * 0x8000000000000000 0x8000000000000000 M*	n1 n2
 * 0x0000000000000000 0x4000000000000000	lo hi
 *
 * -ve * -ve = +ve
 * 0xffffffffffffffff 0xffffffffffffffff M*	n1 n2
 * 0x0000000000000001 0x0000000000000000	lo hi
 *
 * -ve * -ve = +ve
 * 0xdeadbeefdeadbeef 0xbeefdeadbeefdead M*	n1 n2
 * 0x3a522ca1ca1e4983 0x0877fbff78abdf1f	lo hi
 *
 * -ve * -ve = +ve
 * 0xdeadbeefcafebabe 0xbabecafebeefdead M*	n1 n2
 * 0x6ea0c1026f76f666 0x0903a85214a96506	lo hi
 *
 * -ve * +ve = -ve
 * 0xdeadbeefdeadbeef 0x7fffffffffffffff M*	n1 n2
 * 0xa152411021524111 0xef56df77ef56df77	lo hi
 *
 * -ve * +ve = -ve
 * 0xdeadbeefcafebabe 0x7fffffffffffffff M*	n1 n2
 * 0x2152411035014542 0xef56df77e57f5d5f	lo hi
 *
 * -ve * +ve = -ve
 * 0xdeadbeefcafebabe 0x7ee3cafebeefdead M*	n1 n2
 * 0xe416c1026f76f666 0xef7bdd9e44bcc2d0	lo hi
 *
 * (I love my HP 16C!)
 */
void
p4Muls(P4_Int a, P4_Int b, P4_Int *c0, P4_Int *c1)
{
	P4_Int sign = a ^ b;
	p4Mulu(a < 0 ? -a : a, b < 0 ? -b : b, (P4_Uint *)c0, (P4_Uint *)c1);
	if (sign < 0) {
		/* Double cell negate. */
		*c1 = ~*c1 + ((*c0 = -*c0) == 0);
	}
}

/*
 * https://andrewlock.net/counting-the-leading-zeroes-in-a-binary-number/
 */
unsigned
p4LeadZeroBits(P4_Uint x)
{
	/* Smear */
	x |= x >> 1;
	x |= x >> 2;
	x |= x >> 4;
	x |= x >> 8;
# if P4_UINT_BITS >= 32
	x |= x >> 16;
#  if P4_UINT_BITS >= 64
	x |= x >> 32;
#  endif
# endif
	/* Count the ones */
	x -= (x >> 1) & (P4_Uint)0x5555555555555555L;
	x = ((x >> 2) & (P4_Uint)0x3333333333333333L) + (x & (P4_Uint)0x3333333333333333L);
	x = ((x >> 4) + x) & (P4_Uint)0x0f0f0f0f0f0f0f0fL;
	x += x >> 8;
# if P4_UINT_BITS >= 32
	x += x >> 16;
#  if P4_UINT_BITS >= 64
	x += x >> 32;
#  endif
# endif
	return P4_UINT_BITS - (x & 0x7f);
}

/**
 * https://skanthak.homepage.t-online.de/division.html#ansi_c
 *
 * Based on Donald Knuth’s Algorithm D.  This is a 2/1 digit division,
 * eg. 128b/64b or 64b/32b depending on P4_Uint definition.
 *
 * > This further simplified and optimised implementation of "Algorithm D"
 * > for unsigned 128/64-bit division on 32-bit machines is based on a
 * > 64/32-bit division returning a 64-bit quotient and a 32-bit remainder,
 * > trivially implemented per "long" (alias "schoolbook") division using
 * > a "narrowing" 64/32-bit division returning a 32-bit quotient and a
 * > 32-bit remainder.
 *
 * @param dend0, dend1
 * 	Dividend as a pair low and high words.
 *
 * @param dsor
 *	Divisor as a single P4_Uint.
 *
 * @param rem
 *	Pointer in which to pass back the remainder.  Can be NULL
 *	to ignore.
 *
 * @return
 *	Return the quotient.
 */
P4_Uint
p4Divu(P4_Uint dend0, P4_Uint dend1, P4_Uint dsor, P4_Uint *rem)
{
	size_t shift;			// Shift amount for norm.
	P4_Uint qhat;			// A quotient.
	P4_Uint rhat;			// A remainder.
	P4_Uint uhat;			// A dividend digit pair.
	P4_Uint_Half q0, q1;		// Quotient digits.

	if (dsor == 0) {
		(void) LONGJMP(sig_break_glass, P4_THROW_SIGFPE);
	}
	if (dend1 >= dsor) {		// If overflow, set rem.
		if (rem != NULL) {	// to an impossible value,
			*rem = ~0;	// and return the largest
		}
		return ~0;		// possible quotient.
	}

	shift = p4LeadZeroBits(dsor);	// 0 <= shift <= 63.
	if (shift > 0) {
		dsor <<= shift;		// Normalize divisor.
		dend1 <<= shift;	// Shift dividend left.
		dend1 |= dend0 >> (P4_UINT_BITS - shift);
		dend0 <<= shift;
	}

	// Compute high quotient digit.
	qhat = dend1 / (P4_Uint_Half)(dsor >> P4_HALF_SHIFT);
	rhat = dend1 % (P4_Uint_Half)(dsor >> P4_HALF_SHIFT);

	while (
		(P4_Uint_Half)(qhat >> P4_HALF_SHIFT) != 0 ||
		// Both qhat and rhat are less 2**P4_HALF_SHIFT here!
		(qhat & P4_LOWER_MASK) * (P4_Uint_Half)(dsor & P4_LOWER_MASK) >
		((rhat << P4_HALF_SHIFT) | (P4_Uint_Half)(dend0 >> P4_HALF_SHIFT))
	) {
		qhat -= 1;
		rhat += (dsor >> P4_HALF_SHIFT);
		if ((rhat >> P4_HALF_SHIFT) != 0) {
			break;
		}
	}

	// Multiply and subtract.
	q1 = (P4_Uint_Half)(qhat & P4_LOWER_MASK);
	uhat = ((dend1 << P4_HALF_SHIFT) | (dend0 >> P4_HALF_SHIFT)) - q1 * dsor;

	// Compute low quotient digit.
	qhat = uhat / (P4_Uint_Half)(dsor >> P4_HALF_SHIFT);
	rhat = uhat % (P4_Uint_Half)(dsor >> P4_HALF_SHIFT);

	while (
		(P4_Uint_Half)(qhat >> P4_HALF_SHIFT) != 0 ||
		// Both qhat and rhat are less 2**P4_HALF_SHIFT here!
		(qhat & P4_LOWER_MASK) * (P4_Uint_Half)(dsor & P4_LOWER_MASK) >
		((rhat << P4_HALF_SHIFT) | (P4_Uint_Half)(dend0 & ~0))
	) {
		qhat -= 1;
		rhat += (dsor >> P4_HALF_SHIFT);
		if ((rhat >> P4_HALF_SHIFT) != 0) {
			break;
		}
	}

	q0 = (P4_Uint_Half)(qhat & P4_LOWER_MASK);

	if (rem != NULL) {
		*rem =  (((uhat << P4_HALF_SHIFT) | (P4_Uint_Half)(dend0 & P4_LOWER_MASK)) - q0 * dsor) >> shift;
	}

	return ((P4_Uint) q1 << P4_HALF_SHIFT) | q0;
}

P4_Int
p4Divs(P4_Int dend0, P4_Int dend1, P4_Int dsor, P4_Int *rem)
{
	P4_Int quot;
	int neg_rem = (dend1 < 0);
	P4_Int sign = dend1 ^ dsor;
	if (dend1 < 0) {
		/* Double cell negate. */
		dend1 = ~dend1 + ((dend0 = -dend0) == 0);
	}
	quot = (P4_Int) p4Divu(dend0, dend1, dsor < 0 ? -dsor : dsor, (P4_Uint *)rem);
	if (sign < 0) {
		quot = -quot;
	}
	if (neg_rem) {
		*rem = -*rem;
	}
	return quot;
}

void
p4Dadd(P4_Uint a[2], P4_Uint b[2], P4_Uint c[2])
{
	c[0] = a[0] + b[0];
	c[1] = a[1] + b[1] + (c[0] < b[0]);
}

void
p4Dsub(P4_Uint a[2], P4_Uint b[2], P4_Uint c[2])
{
	c[0] = a[0] - b[0];
	c[1] = a[1] - b[1] - (a[0] < b[0]);
}

void
p4Dneg(P4_Uint b[2], P4_Uint c[2])
{
	P4_Uint a[2] = { 0, 0 };
	p4Dsub(a, b, c);
}

#ifdef NOT_USED
p4UTstar(P4_Uint a[2], P4_Uint b, P4_Uint c[3])
{
	P4_Uint d[4];
	p4Mulu(a[0], b, c,   d)
	p4Mulu(a[1], b, d+1, d+2);
	d[3] = 0; p4Dadd(d, d+2, c+1);
}
#endif

/***********************************************************************
 *** Core
 ***********************************************************************/

int
p4StrNum(P4_String str, unsigned base, P4_Cell out[2], int *is_float, int *is_double)
{
	size_t offset = 0;
	*is_float = 0;
	*is_double = 0;
	if (str.length == 0) {
		return -1;
	}
	if (str.string[0] == '\'') {
		if (str.length == 3 && str.string[2] == '\'') {
			out[0].u = str.string[1];
			return 0;
		}
		if (str.length == 4 && str.string[1] == '\\' && str.string[3] == '\'') {
			out[0].u = p4CharLiteral(str.string[2]);
			return 0;
		}
	}
	static char prefix[] = { '$', 16, '#', 10, '%', 2, 0 };
	for (char *p = prefix; *p != 0; p += 2) {
		if (*p == str.string[0]) {
			base = p[1];
			offset++;
			break;
		}
	}
	int negate = 0;
	if (str.string[offset] == '-') {
		negate = 1;
		offset++;
	} else 	if (str.string[offset] == '+') {
		offset++;
	}
	P4_Uint a[2], b[2];
	for (out[0] = out[1] = (P4_Cell) 0L; offset < str.length; offset++) {
		b[0] = p4Base36(str.string[offset]);
		if (base <= b[0]) {
			/* Support small integer double notation 123. => 123 0 */
			if (offset + 1 == str.length && str.string[offset] == '.') {
				*is_double = 1;
				break;
			}
#ifdef HAVE_MATH_H
			char *stop;
			/* Note that 1E 0E 123E may not accepted depending
			 * on the version of strtod().  0.0, .0, 0E0, 123.,
			 * 123.456 work fine.
			 */
			out[0].f = strtod(str.string, &stop);
			if (str.string + str.length == stop) {
				*is_float = 1;
				return 0;
			}
#endif
			return -1;
		}
		p4Mulu(out[1].u, base, a, a+1);
		b[1] = a[0];
		p4Mulu(out[0].u, base, a, a+1);
		p4Dadd(a, b, (P4_Uint *)out);
	}
	if (negate) {
		p4Dneg((P4_Uint *)out, a);
		out[0].u = a[0]; out[1].u = a[1];
	}
	return 0;
}

P4_Int
p4Refill(P4_Input *input)
{
	int n;
	if (P4_INPUT_IS_EVAL(input)
	|| (n = alineInput(input->fp, "", input->buffer, P4_INPUT_SIZE)) < 0) {
		return P4_FALSE;
	}
	input->length = n;
	input->offset = 0;
	return P4_TRUE;
}

void
p4WordFree(P4_Word *word)
{
	if (word != NULL) {
		free(word->name);
		free(word);
	}
}

void *
p4Allot(P4_Ctx *ctx, P4_Int n)
{
	if (ctx->end <= ctx->here + n) {
		/* Attempt to reserve more data space than available. */
		return NULL;
	}
	if (*ctx->active != NULL) {
		if (ctx->here + n < (P4_Char *)(*ctx->active)->data) {
			/* Attempt to release data space below the most recently
			 * created word.
			 */
			return NULL;
		}
		(*ctx->active)->ndata += n;
	}
	void *start = ctx->here;
	ctx->here += n;
	return start;
}

P4_Word *
p4WordCreate(P4_Ctx *ctx, const char *name, size_t length, P4_Code code)
{
	P4_Word *word;

	if ((word = calloc(1, sizeof (*word))) == NULL) {
		goto error0;
	}
	if ((word->name = strndup(name, length)) == NULL) {
		goto error1;
	}
	word->length = length;

	/* Make sure new word starts with aligned data. */
	ctx->here = (P4_Char *) P4_CELL_ALIGN(ctx->here);
	word->data = (P4_Cell *) ctx->here;
	word->code = code;

	word->prev = *ctx->active;
	*ctx->active = word;

	return word;
error1:
	free(word);
error0:
	LONGJMP(ctx->longjmp, P4_THROW_ALLOCATE);
}

void
p4WordAppend(P4_Ctx *ctx, P4_Cell data)
{
	(void) p4Allot(ctx, P4_ALIGN_BY(ctx->here));
	*(P4_Cell *)p4Allot(ctx, sizeof (data)) = data;
}

P4_Nt
p4FindNameIn(P4_Ctx *ctx, const char *caddr, P4_Size length, int wid)
{
	if (wid < 0 || P4_WORDLISTS < wid) {
		LONGJMP(ctx->longjmp, P4_THROW_EINVAL);
	}
	for (P4_Word *word = ctx->lists[wid-1]; word != NULL; word = word->prev) {
		if (!P4_WORD_IS_HIDDEN(word)
		&& word->length > 0 && word->length == length
		&& strncasecmp(word->name, caddr, length) == 0) {
			return word;
		}
	}

	return NULL;
}

P4_Nt
p4FindName(P4_Ctx *ctx, const char *caddr, P4_Size length)
{
	/* Start from zero, LOCALS always included first. */
	for (unsigned i = 0; i < ctx->norder; i++) {
		P4_Nt nt = p4FindNameIn(ctx, caddr, length, ctx->order[i]);
		if (nt != NULL) {
			return nt;
		}
	}
	return NULL;
}

int
p4IsNtIn(P4_Ctx *ctx, P4_Nt nt, int wid)
{
	if (wid < 0 || P4_WORDLISTS < wid) {
		LONGJMP(ctx->longjmp, P4_THROW_EINVAL);
	}
	for (P4_Word *word = ctx->lists[wid-1]; word != NULL; word = word->prev) {
		if (nt == word) {
			return 1;
		}
	}
	return 0;
}

int
p4IsNt(P4_Ctx *ctx, P4_Nt nt)
{
	for (unsigned i = 0; i < ctx->norder; i++) {
		if (p4IsNtIn(ctx, nt, ctx->order[i])) {
			return 1;
		}
	}
	return 0;
}

static void
p4FreeWords(P4_Word *words)
{
	P4_Word *word, *prev;
	for (word = words; word != NULL && p4_builtin_words != word; word = prev) {
		prev = word->prev;
		p4WordFree(word);
	}
}

void
p4Free(P4_Ctx *ctx)
{
	if (ctx != NULL) {
		for (int i = 0; i < P4_WORDLISTS; i++) {
			p4FreeWords(ctx->lists[i]);
		}
		if (ctx->block_fd != NULL) {
			(void) fclose(ctx->block_fd);
		}
		free(ctx->ds.base - P4_GUARD_CELLS/2);
		free(ctx->fs.base - P4_GUARD_CELLS/2);
		free(ctx->rs.base - P4_GUARD_CELLS/2);
		free(ctx->input);
		free(ctx->block);
		free(ctx);
	}
}

void
p4ResetInput(P4_Ctx *ctx, FILE *fp)
{
	ctx->input->fp = fp;
	ctx->input->length = 0;
	ctx->input->offset = 0;
	ctx->input->blk = 0;
}

void
p4AllocStack(P4_Ctx *ctx, P4_Stack *stk, unsigned need)
{
	int depth = 0;
	P4_Cell *base = NULL;
	if (stk->base != NULL) {
		depth = P4_PLENGTH(stk);
		if (depth+need <= stk->size) {
			return;
		}
		base = stk->base - P4_GUARD_CELLS/2;
		need = P4_ALIGN_SIZE(depth + need, P4_STACK_EXTRA);
	}
	if ((base = realloc(base, (need + P4_GUARD_CELLS) * sizeof (*stk->base))) == NULL) {
		LONGJMP(ctx->longjmp, P4_THROW_ALLOCATE);
	}
	/* Adjust base for underflow guard. */
	stk->base = base + P4_GUARD_CELLS/2;
	stk->base[need].u = P4_SENTINEL;
	stk->base[-1].u = P4_SENTINEL;
	stk->base[need+1].u = 0;
	stk->base[-2].u = 0;
	stk->size = need;
	P4_PSET(stk, depth);
}

static P4_Input *
p4CreateInput(void)
{
	P4_Input *input;
	if ((input = calloc(1, sizeof (*input))) == NULL) {
		return NULL;
	}
	MEMSET(input->data, BYTE_ME, sizeof (input->data));
	/* A separate pointer to the buffer data allows the pointer
	 * to be temporarily replaced and then easily restored, ie.
	 * input strings.
	 */
	input->buffer = input->data;
	return input;
}

P4_Ctx *
p4Create(P4_Options *opts)
{
	P4_Ctx *ctx;

	/* GH-5 Clear initial memory space to placate Valgrind. */
	if ((ctx = calloc(opts->mem_size, 1024)) == NULL) {
		goto error0;
	}
	ctx->end = (P4_Char *)ctx + opts->mem_size * 1024;
	ctx->here = (P4_Char*)(ctx+1);
	/* GH-5 Setting memory to something other than zero can
	 * help debug possible memory use before initialising.
	 */
	MEMSET(ctx->here, BYTE_ME, ctx->end - ctx->here);

	ctx->radix = 10;
	ctx->unkey = EOF;
	ctx->options = opts;
	ctx->state = P4_STATE_INTERPRET;
	ctx->trace = opts->trace;

	p4AllocStack(ctx, &ctx->ds, P4_DATA_STACK_SIZE);
	p4AllocStack(ctx, &ctx->rs, P4_RETURN_STACK_SIZE);
#ifdef HAVE_MATH_H
	p4AllocStack(ctx, &ctx->fs, P4_FLOAT_STACK_SIZE);
	ctx->precision = 6;
#endif
	if ((ctx->input = p4CreateInput()) == NULL) {
		goto error0;
	}
	ctx->input->path = "/dev/stdin";
	p4ResetInput(ctx, stdin);

	if ((ctx->block = calloc(1, sizeof (*ctx->block))) == NULL) {
		goto error0;
	}
	if (opts->block_file != NULL && *opts->block_file != '\0'	/* Block file name? */
	&& (ctx->block_fd = fopen(opts->block_file, "rb+")) == NULL	/* File exists? */
	&& (ctx->block_fd = fopen(opts->block_file, "wb+")) == NULL) {	/* Else create file. */
		(void) fprintf(STDERR, "post4: %s: %s\r\n", opts->block_file, strerror(errno));
	}
	ctx->norder = 1;
	ctx->order[0] = 1;
	ctx->active = &ctx->lists[0];
	return ctx;
error0:
	p4Free(ctx);
	return NULL;
}

#ifdef P4_TRACE
static void
p4Bp(P4_Ctx *ctx)
{
	P4_Input *input = ctx->input;
	int has_nl = input->buffer[input->length-(0 < input->length)] == '\n';
	(void) fprintf(STDERR, "\r\n>> ");
	for (unsigned i = 0; i < input->length-has_nl; i++) {
		(void) fputc(input->buffer[i] == '\t' ? ' ' : input->buffer[i], STDERR);
	}
	(void) fprintf(STDERR, "\r\n>> %*c\r\n", (int)input->offset, '^' );
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
static void
p4TraceStack(P4_Ctx *ctx, P4_Stack *stk, int u, const char *prefix)
{
	P4_Cell w;
	int is_small;

	(void) fprintf(STDERR, "%s%s", prefix, 0 < u ? "" : "-");
	for ( ; 0 < u; u--) {
		w = stk->top[1-u];
		is_small = -65536 < w.n && w.n < 65536;
		(void) fprintf(STDERR, is_small ? P4_INT_FMT"%s" : P4_HEX_FMT"%s", w.n, 1 < u ? " " : "");
	}
}
#pragma GCC diagnostic pop

static void
p4Trace(P4_Ctx *ctx, P4_Xt xt, P4_Cell *ip)
{
	if (ctx->trace) {
#ifdef HAVE_MATH_H
		(void) fprintf(
			STDERR, "ds=%-2d fs=%-2d rs=%-2d %*s%s ",
			(int)P4_LENGTH(ctx->ds), (int)P4_LENGTH(ctx->fs), (int)P4_LENGTH(ctx->rs),
			2 * (int)ctx->level, "", 0 < xt->length ? (char *)xt->name : ":NONAME"
		);
#else
		(void) fprintf(
			STDERR, "ds=%-2d rs=%-2d %*s%s ",
			(int)P4_LENGTH(ctx->ds), (int)P4_LENGTH(ctx->rs),
			2 * (int)ctx->level, "", 0 < xt->length ? (char *)xt->name : ":NONAME"
		);
#endif
		for (int i = P4_WD_LIT(xt); 0 < i--; ip++) {
			int is_small = -65536 < ip->n && ip->n < 65536;
			(void) fprintf(STDERR, is_small ? P4_INT_FMT" " : P4_HEX_FMT" ", ip->n);
		}
		if (xt->poppush & 0xF0F0F0) {
			p4TraceStack(ctx, &ctx->ds, P4_DS_CAN_POP(xt), "\t");
#ifdef HAVE_MATH_H
			p4TraceStack(ctx, &ctx->fs, P4_FS_CAN_POP(xt), " ; ");
#endif
			p4TraceStack(ctx, &ctx->rs, P4_RS_CAN_POP(xt), " ; ");
		}
		(void) fputs(crlf, STDERR);
	}
}
#else
# define p4Bp(ctx)
# define p4Cp(ctx)
# define p4Trace(ctx, xt, ip)
#endif

/* When compiled with debugging add more selective and frequent stack checks. */
#ifdef NDEBUG
# define P4BP(ctx)
# define P4STACKGUARD(ctx, stk, over, under)
# define P4STACKGUARDS(ctx)
# define P4STACKISEMPTY(ctx, stk, under)
# define P4STACKISFULL(ctx, stk, over)
#else
# define P4BP(ctx)				p4Bp(ctx)
# define P4STACKGUARD(ctx, stk, over, under) 	p4StackGuard(ctx, stk, over, under)
# define P4STACKGUARDS(ctx)			p4StackGuards(ctx)
# define P4STACKISEMPTY(ctx, stk, under)	p4StackIsEmpty(ctx, stk, under)
# define P4STACKISFULL(ctx, stk, over)		p4StackIsFull(ctx, stk, over)
#endif

/*
 * Stack checks before operation.
 */
static void
p4StackIsEmpty(P4_Ctx *ctx, P4_Stack *stack, int under)
{
	if (P4_PLENGTH(stack) <= 0) {
		LONGJMP(ctx->longjmp, under);
	}
}

#ifdef DEAD
static void
p4StackIsFull(P4_Ctx *ctx, P4_Stack *stack, int over)
{
	if (stack->size <= P4_PLENGTH(stack)) {
		LONGJMP(ctx->longjmp, over);
	}
}
#endif

/*
 * Stack checks after operation.
 */
static void
p4StackGuard(P4_Ctx *ctx, P4_Stack *stack, int over, int under)
{
	ptrdiff_t length = P4_PLENGTH(stack);
	if (length < 0 || stack->base[-1].u != P4_SENTINEL) {
		stack->base[-1].u = P4_SENTINEL;
		LONGJMP(ctx->longjmp, under);
	}
	if (stack->size < length || stack->base[stack->size].u != P4_SENTINEL) {
		stack->base[stack->size].u = P4_SENTINEL;
		LONGJMP(ctx->longjmp, over);
	}
}

static void
p4StackGuards(P4_Ctx *ctx)
{
	p4StackGuard(ctx, &ctx->ds, P4_THROW_DS_OVER, P4_THROW_DS_UNDER);
	p4StackGuard(ctx, &ctx->rs, P4_THROW_RS_OVER, P4_THROW_RS_UNDER);
#ifdef HAVE_MATH_H
	p4StackGuard(ctx, &ctx->fs, P4_THROW_FS_OVER, P4_THROW_FS_UNDER);
#endif
}

int
p4Repl(P4_Ctx *ctx, volatile int thrown)
{
	int rc;
	P4_String str;
	P4_Cell w, x, y, *ip;

#pragma GCC diagnostic push
/* Ignore pedantic warning about "address of a label", required extension. */
#pragma GCC diagnostic ignored "-Wpedantic"
	static P4_Word words[] = {
		P4_WORD("_nop",		&&_nop,		0, 0x00),	//_p4
#define w_nop		words[0]
		P4_WORD("LIT",		&&_lit,		0, 0x01000001),	// historic
#define w_lit		words[1]
		P4_WORD("_;",		&&_exit,	0, 0x0100),	// _seext
#define w_semi		words[2]
		P4_WORD("_abort",	&&_abort,	0, 0x00),	// p4
#define w_abort		words[3]
		P4_WORD("QUIT",		&&_quit,	0, 0x00),
#define w_quit		words[4]
		P4_WORD("_interpret",	&&_interpret,	0, 0x00),	// p4
#define w_interpret	words[5]
		P4_WORD("REFILL",	&&_refill,	0, 0x01),
#define w_refill	words[6]
		P4_WORD("_branchnz",	&&_branchnz,	0, 0x01000010),	// p4
#define w_branchnz	words[7]
#ifdef HAVE_HOOKS
		P4_WORD("_hook_call",	&&_hook_call,	0, 0x00),	// p4
#endif
		/* Exposed even when no float support; see CATCH THROW. */
		P4_WORD("_fs",		&&_fs,		0, 0x03),	// p4
#ifdef HAVE_MATH_H
# if defined(FLT_EVAL_METHOD) == 0
#  define MAX_FLOAT	((float) FLT_MAX)
#  define MIN_FLOAT	((float) FLT_MIN)
# else
#  define MAX_FLOAT	((double) DBL_MAX)
#  define MIN_FLOAT	((double) DBL_MIN)
# endif
//		P4_FVAL("min-float",	MIN_FLOAT),			// p4
		P4_FVAL("max-float",	MAX_FLOAT),			// p4
		P4_WORD(">FLOAT",	&&_to_float,	0, 0x010021),
		P4_WORD("FROUND",	&&_f_round,	0, 0x110000),
		P4_WORD("FTRUNC",	&&_f_trunc,	0, 0x110000),
		P4_WORD("FLOOR",	&&_f_floor,	0, 0x110000),
		P4_WORD("FSQRT",	&&_f_sqr,	0, 0x110000),
		P4_WORD("FATAN2",	&&_f_atan2,	0, 0x210000),
		P4_WORD("FACOSH",	&&_f_acosh,	0, 0x110000),
		P4_WORD("FASINH",	&&_f_asinh,	0, 0x110000),
		P4_WORD("FATANH",	&&_f_atanh,	0, 0x110000),
		P4_WORD("FACOS",	&&_f_acos,	0, 0x110000),
		P4_WORD("FASIN",	&&_f_asin,	0, 0x110000),
		P4_WORD("FATAN",	&&_f_atan,	0, 0x110000),
		P4_WORD("FCOSH",	&&_f_cosh,	0, 0x110000),
		P4_WORD("FSINH",	&&_f_sinh,	0, 0x110000),
		P4_WORD("FTANH",	&&_f_tanh,	0, 0x110000),
		P4_WORD("FCOS",		&&_f_cos,	0, 0x110000),
		P4_WORD("FSIN",		&&_f_sin,	0, 0x110000),
		P4_WORD("FTAN",		&&_f_tan,	0, 0x110000),
		P4_WORD("FLN",		&&_f_ln,	0, 0x110000),
		P4_WORD("FLOG",		&&_f_log,	0, 0x110000),
		P4_WORD("FEXP",		&&_f_exp,	0, 0x110000),
		P4_WORD("FMAX",		&&_f_max,	0, 0x210000),
		P4_WORD("FMIN",		&&_f_min,	0, 0x210000),
		P4_WORD("F**",		&&_f_pow,	0, 0x210000),
		P4_WORD("F!",		&&_f_store,	0, 0x100010),
		P4_WORD("F@",		&&_f_fetch,	0, 0x010010),
		P4_WORD("F+",		&&_f_add,	0, 0x210000),
		P4_WORD("F-",		&&_f_sub,	0, 0x210000),
		P4_WORD("F*",		&&_f_mul,	0, 0x210000),
		P4_WORD("F/",		&&_f_div,	0, 0x210000),
		P4_WORD("F0<",		&&_f_lt0,	0, 0x110000),
		P4_WORD("F0=",		&&_f_eq0,	0, 0x110000),
		P4_WORD("FS.",		&&_f_sdot,	0, 0x100000),
		P4_WORD("F.",		&&_f_dot,	0, 0x100000),
		P4_WORD("REPRESENT",	&&_f_represent,	0, 0x100023),
		P4_WORD("F>S",		&&_f_to_s,	0, 0x100001),
		P4_WORD("S>F",		&&_s_to_f,	0, 0x010010),
		P4_WORD("f>r",		&&_fs_to_rs,	0, 0x100100),	// p4
		P4_WORD("fr>",		&&_rs_to_fs,	0, 0x011000),	// p4
#endif
		P4_WORD("stdin",                &&_fa_stdin,    0, 0x01),       // p4
		P4_WORD("stdout",               &&_fa_stdout,   0, 0x01),       // p4
		P4_WORD("BIN",			&&_fa_bin,	0, 0x01),
		P4_WORD("CLOSE-FILE",		&&_fa_close,	0, 0x11),
		P4_WORD("CREATE-FILE",		&&_fa_create,	0, 0x22),
		P4_WORD("DELETE-FILE",		&&_fa_delete,	0, 0x21),
		P4_WORD("FILE-POSITION",	&&_fa_tell,	0, 0x13),
		P4_WORD("FILE-SIZE",		&&_fa_fsize,	0, 0x13),
		P4_WORD("FILE-STATUS",		&&_fa_status,	0, 0x22),
		P4_WORD("FLUSH-FILE",		&&_fa_flush,	0, 0x11),
		P4_WORD("_eval_file",		&&_eval_file,	0, 0x10),	// p4
		P4_WORD("find-file-path",	&&_fa_find_path,0, 0x42),	// p4
		P4_WORD("OPEN-FILE",		&&_fa_open,	0, 0x32),
		P4_WORD("READ-FILE",		&&_fa_read,	0, 0x32),
		P4_WORD("READ-LINE",		&&_fa_rline,	0, 0x33),
		P4_WORD("REPOSITION-FILE",	&&_fa_seek,	0, 0x21),
		P4_WORD("WRITE-FILE",		&&_fa_write,	0, 0x31),

		/* Constants. */
		P4_VAL("R/O",			0),
		P4_VAL("R/W",			1),
		P4_VAL("/pad",			P4_PAD_SIZE),		// p4
		P4_VAL("address-unit-bits",	P4_CHAR_BIT),		// p4
		P4_VAL("WORDLISTS",		P4_WORDLISTS),
		P4_WORD("post4-path",	&&_post4_path,	0, 0x02),	// p4
		P4_WORD("post4-commit", &&_post4_commit,0, 0x02),	// p4

		/* Internal support. */
		P4_WORD("_bp",		&&_bp,		0, 0x00),	// p4
		P4_WORD("_branch",	&&_branch,	P4_BIT_COMPILE, 0x01000000),	// p4
		P4_WORD("_branchz",	&&_branchz,	P4_BIT_COMPILE, 0x01000010),	// p4
		P4_WORD("_call",	&&_call,	P4_BIT_COMPILE, 0x01000100),	// p4
		P4_WORD("_ds",		&&_ds,		0, 0x03),	// p4
		P4_WORD("_longjmp",	&&_longjmp,	0, 0x10),	// p4
		P4_WORD("_rs",		&&_rs,		0, 0x03),	// p4
		P4_WORD("_pp!",		&&_pp_put,	P4_BIT_IMM, 0x10), // p4
		P4_WORD("_stack_check", &&_stack_check, 0, 0x00),	// p4
		P4_WORD("_stack_dump",	&&_stack_dump,	0, 0x20),	// p4
		P4_WORD("_window",	&&_window,	0, 0x02),	// p4

		/* Compiling Words */
		P4_WORD("compile-only",	&&_compile_only,0, 0x00),	//p4
		P4_WORD(":NONAME",	&&_noname,	0, 0x00),
		P4_WORD(":",		&&_colon,	0, 0x00),
		P4_WORD(";",		&&_semicolon,	P4_BIT_IMM|P4_BIT_COMPILE, 0x00),
		P4_WORD(">BODY",	&&_body,	0, 0x01),
		P4_WORD("_created",	&&_created,	0, 0x20),
		P4_WORD("CREATE",	&&_create,	0, 0x00),
		P4_WORD("DOES>",	&&_does,	P4_BIT_COMPILE, 0x1000),
		P4_WORD("EXECUTE",	&&_execute,	0, 0x10),
		P4_WORD("EXIT",		&&_exit,	P4_BIT_COMPILE, 0x1000),
		P4_WORD("IMMEDIATE",	&&_immediate,	0, 0x00),

		/* Data Space - Alignment */
		P4_WORD("CELLS",	&&_cells,	0, 0x11),
		P4_WORD("ALLOT",	&&_allot,	0, 0x10),
		P4_WORD(">here",	&&_here_offset,	0, 0x01),	// p4

		/* Data Space - Access */
		P4_WORD("_ctx",		&&_ctx,		0, 0x01),	// p4
		P4_WORD("!",		&&_store,	0, 0x20),
		P4_WORD(">R",		&&_to_rs,	0, 0x0110),
		P4_WORD("@",		&&_fetch,	0, 0x11),
		P4_WORD("C!",		&&_cstore,	0, 0x20),
		P4_WORD("C@",		&&_cfetch,	0, 0x11),
		P4_WORD("DROP",		&&_drop,	0, 0x10),
		P4_WORD("DUP",		&&_dup,		0, 0x12),
		P4_WORD("MOVE",		&&_move,	0, 0x30),
		P4_WORD("PICK",		&&_pick,	0, 0x11),
		P4_WORD("R>",		&&_from_rs,	0, 0x1001),
		P4_WORD("ROLL",		&&_roll,	0, 0x10),
		P4_WORD("SWAP",		&&_swap,	0, 0x22),

		/* Dynamic Memory */
		P4_WORD("ALLOCATE",	&&_allocate,	0, 0x12),
		P4_WORD("FREE",		&&_free,	0, 0x11),
		P4_WORD("RESIZE",	&&_resize,	0, 0x22),

		/* Operators */
		P4_WORD("*",		&&_mul,		0, 0x21),
		P4_WORD("+",		&&_add,		0, 0x21),
		P4_WORD("-",		&&_sub,		0, 0x21),
		P4_WORD("/",		&&_div,		0, 0x21),
		P4_WORD("AND",		&&_and,		0, 0x21),
		P4_WORD("INVERT",	&&_not,		0, 0x11),
		P4_WORD("LSHIFT",	&&_lshift,	0, 0x21),
		P4_WORD("M*",		&&_mstar,	0, 0x22),
		P4_WORD("MOD",		&&_mod,		0, 0x21),
		P4_WORD("OR",		&&_or,		0, 0x21),
		P4_WORD("RSHIFT",	&&_rshift,	0, 0x21),
		P4_WORD("SM/REM",	&&_sm_div_rem,	0, 0x21),
		P4_WORD("UM*",		&&_umstar,	0, 0x22),
		P4_WORD("UM/MOD",	&&_um_div_mod,	0, 0x21),
		P4_WORD("XOR",		&&_xor,		0, 0x21),

		/* Comparisons */
		P4_WORD("0=",		&&_eq0,		0, 0x11),
		P4_WORD("0<",		&&_lt0,		0, 0x11),
		P4_WORD("U<",		&&_u_lt,	0, 0x21),
		P4_WORD("<",		&&_lt,		0, 0x21),

		/* Tools*/
		P4_WORD("alias",	&&_alias,	0, 0x10),	// p4
		P4_WORD("bye-status",	&&_bye_code,	0, 0x10),	// p4

		/* I/O */
		P4_WORD("ACCEPT",	&&_accept,	0, 0x21),
		P4_WORD("TYPE",		&&_type,	0, 0x20),
		P4_WORD("epoch-seconds", &&_epoch_seconds, 0, 0x01),	// p4
		P4_WORD("FIND-NAME-IN",	&&_find_name_in, 0, 0x31),
		P4_WORD("FIND-NAME",	&&_find_name,	0, 0x21),
		P4_WORD("KEY",		&&_key,		0, 0x01),
		P4_WORD("KEY?",		&&_key_ready,	0, 0x01),
		P4_WORD("MS",		&&_ms,		0, 0x10),
		P4_WORD("_parse",	&&_parse,	0, 0x22),	// p4
		P4_WORD("PARSE-NAME",	&&_parse_name,	0, 0x02),

		P4_WORD(NULL,		NULL,		0, 0),
	};
#pragma GCC diagnostic pop

	if (p4_builtin_words == NULL) {
		/* Link up the base dictionary. */
		for (w.nt = words; w.nt->code != NULL; w.nt++) {
			w.nt[1].prev = w.nt;
		}
		p4_builtin_words = w.nt->prev;
		*ctx->active = p4_builtin_words;
#ifdef HAVE_HOOKS
		/* Find _hook_call and install any hooked words, eg. SH SHELL. */
		p4_hook_call = p4FindName(ctx, "_hook_call", STRLEN("_hook_call"));
		p4HookInit(ctx, p4_hooks);
#endif
		if ((rc = p4EvalFile(ctx, ctx->options->core_file)) == P4_THROW_OK) {
			/* Find THROW to aid with throwing exceptions from C to Forth. */
			p4_throw = p4FindName(ctx, "THROW", STRLEN("THROW"));
			p4_2lit = p4FindName(ctx, "2lit", STRLEN("2lit"));
#ifdef HAVE_MATH_H
			p4_flit = p4FindName(ctx, "flit", STRLEN("flit"));
#endif
		}
	}

#define NEXT		goto _next
#define THROWHARD(e)	{ rc = (e); goto _thrown; }
#define THROW(e)	{ if (p4_throw != NULL) { x.nt = p4_throw; \
				P4_PUSH(ctx->ds, (P4_Int)(e)); \
				/* Reset any previous exception. */ \
				rc = P4_THROW_OK; goto _forth; \
			} THROWHARD(e); }

#pragma GCC diagnostic push
/* Ignore pedantic warning about "address of a label", required extension. */
#pragma GCC diagnostic ignored "-Wpedantic"
	static const P4_Word w_inter_loop = P4_WORD("_inter_loop", &&_inter_loop, P4_BIT_HIDDEN, 0x00);
	static const P4_Word w_halt = P4_WORD("_halt", &&_halt, P4_BIT_HIDDEN, 0x00);
	static const P4_Cell repl[] = { {.cw = &w_interpret}, {.cw = &w_halt} };

	/* When the REPL executes a word, it puts the XT of the word here
	 * and executes the word with the IP pointed to exec[].  When the
	 * word completes the next XT transitions from threaded code back
	 * into the C driven REPL.
	 */
	static P4_Cell exec[] = { { 0 }, {.cw = &w_inter_loop} };
#pragma GCC diagnostic pop

	SETJMP_PUSH(ctx->longjmp);
	rc = SETJMP(ctx->longjmp);

	if (thrown != P4_THROW_OK) {
		/* Signal thrown overrides context. */
		rc = thrown;
		/* Only report once. */
		thrown = P4_THROW_OK;
	}
	if (rc != P4_THROW_OK && ctx->frame != NULL) {
		/* Throw might be caught, can't fall through. */
		THROW(rc);
	}
_thrown:
	switch (rc) {
	default:
		p4Bp(ctx);
		THROW_MSG(rc);
		/* Cannot not rely on ctx->state for compilation state, since
		 * its possible to temporarily change states in the middle of
		 * compiling a word, eg `: word [ 123 ;`  Use the fact that
		 * while compiling the word is hidden from use.
		 */
		if (P4_WORD_IS_HIDDEN(*ctx->active)) {
			/* A thrown error while compiling a word leaves the
			 * definition in an incomplete state; discard it.
			 */
			w.nt = *ctx->active;
			(void) fprintf(STDERR, " compiling %s",
				w.nt->length == 0 ? ":NONAME" : (char *)w.nt->name
			);
			*ctx->active = w.nt->prev;
			/* Rewind HERE, does not free ALLOCATE data. */
			ctx->here = (P4_Char *) w.nt->data;
			p4WordFree(w.nt);
		} else {
			/* Cannot rely on ip pointing to the xt after the error. */
			(void) fprintf(STDERR, crlf);
			for (x.p = ctx->rs.top; ctx->rs.base <= x.p; x.p--) {
				w = (*x.p).p[-1];
				y.s = p4IsNt(ctx, w.nt) ? w.nt->name : "";
				(void) fprintf(STDERR, P4_H0X_FMT"  %s\r\n", (long) w.nt, y.s);
			}
		}
		/*@fallthrough@*/
	case P4_THROW_SIGINT:
	case P4_THROW_ABORT_MSG:
		/* Ensure ABORT" and other messages print newline.*/
		(void) fprintf(STDERR, crlf);
		/*@fallthrough@*/
	case P4_THROW_ABORT:
		/* Historically no message, simply return to REPL. */
_abort:		P4_RESET(ctx->ds);
#ifdef HAVE_MATH_H
		P4_RESET(ctx->fs);
#endif
		if (!P4_INPUT_IS_TERM(ctx->input)) {
			return rc;
		}
		/*@fallthrough@*/
	case P4_THROW_QUIT:
_quit:		P4_RESET(ctx->rs);
		(void) fflush(STDERR);
		p4ResetInput(ctx, stdin);
		ctx->state = P4_STATE_INTERPRET;
		ctx->frame = 0;
		/* Reset level, else next trace the indentation might be skewed. */
		ctx->level = 0;
		/*@fallthrough@*/
	case P4_THROW_OK:
		;
	}
	ip = (P4_Cell *)(repl+1);
	// (S: -- )
_interpret:
	p4AllocStack(ctx, &ctx->rs, 1);
	P4_PUSH(ctx->rs, ip);
	do {
		p4StackGuards(ctx);
		/* The input buffer might have been primed (EVALUATE, LOAD),
		 * so try to parse it first before reading more input.
		 */
_inter_loop:	while (ctx->input->offset < ctx->input->length) {
			str = p4ParseName(ctx->input);
			if (str.length == 0) {
				break;
			}
			x.nt = p4FindName(ctx, str.string, str.length);
			if (x.nt == NULL) {
				P4_Cell num[2];
				int is_float, is_double;
				if (p4StrNum(str, ctx->radix, num, &is_float, &is_double)) {
					/* Not a word, not a number. */
					THROW(P4_THROW_UNDEFINED);
				}
#ifdef HAVE_MATH_H
				if (is_float) {
					if (ctx->state == P4_STATE_COMPILE) {
						if (p4_flit == NULL) {
							THROW(P4_THROW_UNDEFINED);
						}
						p4WordAppend(ctx, (P4_Cell) p4_flit);
						p4WordAppend(ctx, num[0]);
					} else {
						p4AllocStack(ctx, &ctx->P4_FLOAT_STACK, 1);
						P4_PUSH(ctx->P4_FLOAT_STACK, num[0]);
					}
				} else
#endif
				if (ctx->state == P4_STATE_COMPILE) {
					if (is_double && p4_2lit != NULL) {
						p4WordAppend(ctx, (P4_Cell) p4_2lit);
						p4WordAppend(ctx, num[0]);
						p4WordAppend(ctx, num[1]);
					} else {
						p4WordAppend(ctx, (P4_Cell) &w_lit);
						p4WordAppend(ctx, num[0]);
						if (is_double) {
							p4WordAppend(ctx, (P4_Cell) &w_lit);
							p4WordAppend(ctx, num[1]);
						}
					}
				} else {
					p4AllocStack(ctx, &ctx->ds, 1+is_double);
					P4_PUSH(ctx->ds, num[0]);
					if (is_double) {
						P4_PUSH(ctx->ds, num[1]);
					}
				}
			} else if (ctx->state == P4_STATE_INTERPRET && P4_WORD_IS(x.nt, P4_BIT_COMPILE)) {
				THROW(P4_THROW_COMPILE_ONLY);
			} else if (ctx->state == P4_STATE_COMPILE && !P4_WORD_IS_IMM(x.nt)) {
				p4WordAppend(ctx, (P4_Cell) x.nt);
			} else {
_forth:				exec[0].xt = x.nt;
				ip = exec;
				NEXT;
			}
		}
		if (P4_INTERACTIVE(ctx)) {
			(void) printf(ANSI_CYAN"ok "ANSI_NORMAL);
			(void) fflush(stdout);
		}
	} while (p4Refill(ctx->input));
	p4StackIsEmpty(ctx, &ctx->rs, P4_THROW_RS_UNDER);
	ip = P4_POP(ctx->rs).p;
	NEXT;

	// (  -- )
_halt:	SETJMP_POP(ctx->longjmp);
	return rc;

		// ( -- )
_bp:		p4Bp(ctx);
		/*@fallthrough@*/

_nop:
_next:		w = *ip++;
		/* Pre-load top for some words. */
		x = P4_TOP(ctx->ds);
		p4Trace(ctx, w.xt, ip);
		goto *w.xt->code;

		// ( xt -- )
_execute:	w = P4_POP(ctx->ds);
		/* Pre-load top for some words. */
		x = P4_TOP(ctx->ds);
		p4Trace(ctx, w.xt, ip);
		goto *w.xt->code;

		// ( i*x -- j*y )(R: -- ip)
_enter:		p4AllocStack(ctx, &ctx->rs, 1);
		P4_PUSH(ctx->rs, ip);
		// w contains xt loaded by _next or _execute.
		ip = w.xt->data;
		ctx->level++;
		NEXT;

		// ( i*x -- i*x )(R:ip -- )
_exit:		P4STACKGUARDS(ctx);
		ip = P4_POP(ctx->rs).p;
		ctx->level--;
		NEXT;

		// ( ex_code -- )
_bye_code:	exit((int) x.n);

		// ( -- aaddr )
_ctx:		p4AllocStack(ctx, &ctx->ds, 1);
		P4_PUSH(ctx->ds, (P4_Cell *) ctx);
		NEXT;

		// ( -- )
_call:		w = *ip;
		p4AllocStack(ctx, &ctx->rs, 1);
		P4_PUSH(ctx->rs, ip + 1);
		ip = (P4_Cell *)((P4_Char *) ip + w.n);
		NEXT;

		// ( -- )
_branch:	w = *ip;
		ip = (P4_Cell *)((P4_Char *) ip + w.n);
		NEXT;

		// ( flag -- )
_branchz:	w = *ip;
		P4_DROP(ctx->ds, 1);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"
		ip = (P4_Cell *)((P4_Char *) ip + (x.u == 0 ? w.n : P4_CELL));
#pragma GCC diagnostic pop
		NEXT;

		// ( flag -- )
_branchnz:	w = *ip;
		P4_DROP(ctx->ds, 1);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"
		ip = (P4_Cell *)((P4_Char *) ip + (x.u != 0 ? w.n : P4_CELL));
#pragma GCC diagnostic pop
		NEXT;

#ifdef HAVE_HOOKS
		// ( i*x -- j*y )
_hook_call:	x = w.xt->data[0];
		(*(void (*)(P4_Ctx *)) x.p)(ctx);
		NEXT;
#endif
		// ( n -- )
_longjmp:	P4_DROP(ctx->ds, 1);
		THROWHARD((int) x.n);

		// ( -- x )
		// : lit r> dup cell+ >r @ ;
_lit:		p4AllocStack(ctx, &ctx->ds, 1);
		w = *ip++;
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( -- x )
_doconst:	p4AllocStack(ctx, &ctx->ds, 1);
		P4_PUSH(ctx->ds, w.xt->ndata);
		NEXT;

		// ( n1 -- n2 )
_cells:		P4_TOP(ctx->ds).n *= P4_CELL;
		NEXT;

		/*
		 * Defining words.
		 */
		// ( -- xt colon )
_noname:	str.string = "";
		str.length = 0;
		goto _do_colon;

_colon:		str = p4ParseName(ctx->input);
		goto _do_colon;

		// (C: -- colon) (R: -- ip)
		// Save the current lengths so we can check for imbalance.
_do_colon:	ctx->state = P4_STATE_COMPILE;
		if (ctx->trace) {
			(void) printf("%*s%.*s\r\n", 19+2*(int)ctx->level, "", (int)str.length, str.string);
		}
		x.nt = p4WordCreate(ctx, str.string, str.length, &&_enter);
		p4AllocStack(ctx, &ctx->ds, 1+(x.nt->length == 0));
		if (x.nt->length == 0) {
			/* :NONAME leaves xt on stack. */
			P4_PUSH(ctx->ds, x.nt);
		}
		/* Save sentinel for control imbalance test below. */
		P4_PUSH(ctx->ds, (P4_Uint) P4_MARKER);
		/* Keep new word hidden while compiling. */
		P4_WORD_SET_HIDDEN(x.nt);
		NEXT;

		// (C: colon -- ) (R: ip -- )
_semicolon:	ctx->state = P4_STATE_INTERPRET;
		if (P4_POP(ctx->ds).u != P4_MARKER) {
			/* Control structure imbalance.  Did we match
			 * all the IF-THEN, BEGIN-REPEAT, etc.
			 */
			THROW(P4_THROW_BAD_CONTROL);
		}
		p4WordAppend(ctx, (P4_Cell) &w_semi);
		P4_WORD_CLEAR_HIDDEN(*ctx->active);
		NEXT;

		// ( -- )
_compile_only:	P4_WORD_SET_COMPILE(*ctx->active);
		NEXT;

		// ( -- )
_immediate:	P4_WORD_SET_IMM(*ctx->active);
		NEXT;

		// ( u -- )
_pp_put:	P4_DROP(ctx->ds, 1);
		(*ctx->active)->poppush = x.u;
		NEXT;

		// ( i*x fd -- j*y )
_eval_file:	P4_DROP(ctx->ds, 1);
		p4ResetInput(ctx, x.v);
		goto _interpret;

		/* CREATE DOES> is bit of a mind fuck.  Their purpose is to define
		 * words that in turn define new words.  Best to look at a simple
		 * example like CONSTANT defined as:
		 *
		 *	: CONSTANT CREATE , DOES> @ ;
		 *
		 * Then using CONSTANT to create a new word MONACO:
		 *
		 *	377 CONSTANT MONACO
		 *
		 * The simplest way I've come to understand CREATE and DOES> is:
		 *
		 *	: word1 CREATE ... add data to word2 ...
		 *              DOES>  ... word2 executes this code with data
		 *      ;
		 *
		 * This page http://forth.org/svfig/Len/definwds.htm explains it well.
		 *
		 * CREATE can also be used on its own to assign buffers or initialised
		 * data.
		 *
		 *	CREATE PAD /PAD CHARS ALLOT
		 *	CREATE stuff 1 , 2 , 3 , 4 ,
		 */
		// ( <spaces>name -- )
_create:	str = p4ParseName(ctx->input);
		x.nt = p4WordCreate(ctx, str.string, str.length, &&_data_field);
		// Reserve the 1st data cell for possible DOES>; wasted otherwise.
		p4WordAppend(ctx, (P4_Cell)(P4_Int) 0),
		P4_WORD_SET(x.nt, P4_BIT_CREATED);
		NEXT;

		// ( caddr u -- )
_created:	P4_DROP(ctx->ds, 1);
		w = P4_POP(ctx->ds);
		x.nt = p4WordCreate(ctx, w.s, x.z, &&_nop);
		P4_WORD_SET(x.nt, P4_BIT_CREATED);
		NEXT;

		// DOES>
_does:		w.nt = *ctx->active;
		if (!P4_WORD_IS(w.nt, P4_BIT_CREATED)) {
			THROW(P4_THROW_NOT_CREATED);
		}
		w.nt->code = &&_do_does;
		/*** If we change (again) how a P4_Word and data are
		 *** stored in memory, then most likely need to fix
		 *** this and _seext.
		 ***/
		// Save defining word's xt for _seext.
		x = P4_TOP(ctx->rs);
		p4WordAppend(ctx, *--x.p);
		// Append the IP of the words following DOES> of the defining
		// word after the data of the current word being defined.
		//
		//	: word CREATE ( store data) DOES> ( words) ;
		//	                                  ^--- IP
		w.nt->data[0].p = ip;
		goto _exit;

		// ( -- aaddr)
_do_does:	p4AllocStack(ctx, &ctx->ds, 1);
		P4_PUSH(ctx->ds, w.xt->data + 1);
		// Remember who called us.
		p4AllocStack(ctx, &ctx->rs, 1);
		P4_PUSH(ctx->rs, ip);
		// Continue execution just after DOES> of the defining word.
		ip = w.xt->data[0].p;
		ctx->level++;
		NEXT;

		// ( xt -- addr )
_body:		w = P4_POP(ctx->ds);
		if (!P4_WORD_IS(w.nt, P4_BIT_CREATED)) {
			THROW(P4_THROW_NOT_CREATED);
		}
		/*@fallthrough@*/

		// ( -- addr )
		// w contains xt loaded by _next or _execute.;
_data_field:	p4AllocStack(ctx, &ctx->ds, 1);
		P4_PUSH(ctx->ds, w.xt->data + 1);
		NEXT;

		// ( n -- )
_allot:		P4_DROP(ctx->ds, 1);
		if (p4Allot(ctx, x.n) == NULL) {
			THROW(P4_THROW_ALLOCATE);
		}
		NEXT;

		// ( xt <spaces>name -- )
_alias:		P4_DROP(ctx->ds, 1);
		str = p4ParseName(ctx->input);
		w.nt = p4WordCreate(ctx, str.string, str.length, x.nt->code);
		w.nt->ndata = x.nt->ndata;
		w.nt->data = x.nt->data;
		w.nt->bits = x.nt->bits;
		NEXT;

		// ( -- rows cols )
_window:	p4AllocStack(ctx, &ctx->ds, 2);
		P4_PUSH(ctx->ds, (P4_Uint) window.ws_row);
		P4_PUSH(ctx->ds, (P4_Uint) window.ws_col);
		NEXT;

		/*
		 * Memory access.
		 */
		// ( caddr -- char )
_cfetch:	P4_TOP(ctx->ds).u = *x.s;
		NEXT;

		// ( char caddr -- )
_cstore:	P4_DROP(ctx->ds, 1);
		*x.s = P4_POP(ctx->ds).u;
		NEXT;

		// ( aaddr -- x )
_fetch:		P4_TOP(ctx->ds) = *x.p;
		NEXT;

		// ( x aaddr -- )
_store:		P4_DROP(ctx->ds, 1);
		*x.p = P4_POP(ctx->ds);
		NEXT;

		// ( src dst u -- )
_move:		w = P4_POP(ctx->ds);
		x = P4_POP(ctx->ds);
		/* Using strncpy would allow for propagation, like CMOVE:
		 *	char src[] = "A    ";
		 *	strncpy(src+1, src, 4);
		 *	strcmp(src, "AAAAA") == 0;
		 * Not necessarily as efficent, plus the C standard says
		 * that the behaviour of strncpy with overlapping strings
		 * is undefined (which seems wrong when tested).
		 */
		(void) memmove(x.s, P4_POP(ctx->ds).s, w.z);
		NEXT;

		// ( -- u )
_here_offset:	P4_PUSH(ctx->ds, (P4_Size)(ctx->here - (P4_Char *) (*ctx->active)->data));
		NEXT;

		/*
		 * Dynamic Memory
		 */
		// ( aaddr -- ior )
_free:		free(x.s);
		P4_TOP(ctx->ds).n = 0;
		NEXT;

		// ( u -- aaddr ior )
_allocate:	w.s = NULL;
		p4AllocStack(ctx, &ctx->ds, 1);
		goto _resize_null;

		// ( aaddr1 u -- aaddr2 ior )
_resize:	w = P4_DROPTOP(ctx->ds);
		/*@fallthrough@*/

		/* GH-5 Check for possibly negative size.  A size_t is a positive
		 * value so -1 would be 0xFFFF...FFFF and technically allowed
		 * but so large as to be impractical and possibly a type error,
		 * conversion error, or some size miscalculation.  So -1024..-1
		 * is reserved for trapping this possible error.  Not perfect,
		 * but should be handle most cases.
		 */
_resize_null:	if (x.n < 0 && -1024 <= x.n) {
			P4_TOP(ctx->ds) = w;
			P4_PUSH(ctx->ds, (P4_Int) ENOMEM);
			NEXT;
		}
		errno = 0;
		x.s = realloc(w.s, (size_t) x.u);
		P4_TOP(ctx->ds) = x.s == NULL ? w : x;
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

		/*
		 * Stack manipulation.
		 */
		// ( x -- )
_drop:		p4StackIsEmpty(ctx, &ctx->ds, P4_THROW_DS_UNDER);
		P4_DROP(ctx->ds, 1);
		NEXT;

		// ( x -- x x )
_dup:		p4StackIsEmpty(ctx, &ctx->ds, P4_THROW_DS_UNDER);
		p4AllocStack(ctx, &ctx->ds, 1);
		P4_PUSH(ctx->ds, x);
		NEXT;

		// ( xu ... x1 x0 u -- xu ... x1 x0 xu )
		// : PICK >R _DS DROP 1 - R> - CELLS + @ ;
		// 0 PICK == DUP, 1 PICK == OVER
_pick:		P4_DROP(ctx->ds, 1);
		w = P4_PICK(ctx->ds, x.u);
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( x y -- y x ): pp _ds drop 2 - rot - cells + @ ;
		// 1 ROLL == SWAP
_swap:		P4_DROP(ctx->ds, 1);
		w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds) = x;
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( xu xu-1 ... x0 u –– xu-1 ... x0 xu )
		// 0 ROLL == noop, 1 ROLL == SWAP, 2 ROLL == ROT
_roll:		P4_DROP(ctx->ds, 1);
		w = P4_PICK(ctx->ds, x.n);
		for ( ; 0 < x.u; x.u--) {
			P4_PICK(ctx->ds, x.n) = P4_PICK(ctx->ds, x.n-1);
		}
		P4_TOP(ctx->ds) = w;
		NEXT;

		// (x -- )(R: -- x )
_to_rs:		p4StackIsEmpty(ctx, &ctx->ds, P4_THROW_DS_UNDER);
		P4_DROP(ctx->ds, 1);
		p4AllocStack(ctx, &ctx->rs, 1);
		P4_PUSH(ctx->rs, x);
		P4STACKGUARDS(ctx);
		NEXT;

		// (R: x -- )
_from_rs:	p4StackIsEmpty(ctx, &ctx->rs, P4_THROW_RS_UNDER);
		w = P4_POP(ctx->rs);
		p4AllocStack(ctx, &ctx->ds, 1);
		P4_PUSH(ctx->ds, w);
		P4STACKGUARDS(ctx);
		NEXT;

		/*
		 * Operators
		 */
		// ( n1 n2 -- n3 )
_add:		P4_DROPTOP(ctx->ds).n += x.n;
		NEXT;

		// ( n1 n2 -- n3 )
_sub:		P4_DROPTOP(ctx->ds).n -= x.n;
		NEXT;

		// ( n1 n2 -- n3 )
_mul:		P4_DROPTOP(ctx->ds).n *= x.n;
		NEXT;

		// ( n1 n2 -- n3 )
_div:		P4_DROP(ctx->ds, 1);
		if (x.n == 0) {
			THROW(P4_THROW_DIV_ZERO);
		}
		P4_TOP(ctx->ds).n /= x.n;
		NEXT;

		// n1 n2 -- d (lo hi)
		P4_Cell c0, c1;
_mstar:		w = P4_DROPTOP(ctx->ds);
		p4Muls(x.n, w.n, &c0.n, &c1.n);
		P4_TOP(ctx->ds).u = c0.n;
		P4_PUSH(ctx->ds, c1.n);
		NEXT;

		// n1 n2 -- ud (lo hi)
_umstar:	w = P4_DROPTOP(ctx->ds);
		p4Mulu(x.u, w.u, &c0.u, &c1.u);
		P4_TOP(ctx->ds).u = c0.u;
		P4_PUSH(ctx->ds, c1.u);
		NEXT;

		// ( d dsor -- rem quot )
		// C99+ specifies symmetric division.
		// Dividend Divisor Remainder Quotient
		//       10       7         3        1
		//      -10       7        -3       -1
		//       10      -7         3       -1
		//      -10      -7        -3        1
		//
_sm_div_rem:	y = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		if (y.n == 0) {
			THROW(P4_THROW_DIV_ZERO);
		}
		w.n = p4Divs(x.n, w.n, y.n, &x.n);
		P4_TOP(ctx->ds).n = x.n;
		P4_PUSH(ctx->ds, w.n);
		NEXT;

		// ( ud dsor -- mod quot )
_um_div_mod:	y = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		if (y.n == 0) {
			THROW(P4_THROW_DIV_ZERO);
		}
		w.u = p4Divu(x.u, w.u, y.u, &x.u);
		P4_TOP(ctx->ds).u = x.u;
		P4_PUSH(ctx->ds, w.u);
		NEXT;

		// ( n1 n2 -- n3 )
_mod:		if (x.n == 0) {
			THROW(P4_THROW_DIV_ZERO);
		}
		P4_DROPTOP(ctx->ds).n %= x.n;
		NEXT;

		// ( x1 x2 -- x3 )
_and:		P4_DROPTOP(ctx->ds).u &= x.u;
		NEXT;

		// ( x1 x2 -- x3 )
_or:		P4_DROPTOP(ctx->ds).u |= x.u;
		NEXT;

		// ( x1 x2 -- x3 )
_xor:		P4_DROPTOP(ctx->ds).u ^= x.u;
		NEXT;

		// ( n1 -- n2 )
_not:		P4_TOP(ctx->ds).u = ~x.u;
		NEXT;

		// ( x1 u -- x2 )
_lshift:	P4_DROPTOP(ctx->ds).u <<= x.u;
		NEXT;

		// ( x1 u -- x2 )
_rshift:	P4_DROPTOP(ctx->ds).u >>= x.u;
		NEXT;

		/*
		 * Comparision
		 */
		// ( x -- flag )
_eq0:		P4_TOP(ctx->ds).u = P4_BOOL(x.u == 0);
		NEXT;

		// ( x -- flag )
_lt0:		P4_TOP(ctx->ds).u = P4_BOOL(x.n < 0);
		NEXT;

		// ( u1 u2 -- )
_u_lt:		w = P4_DROPTOP(ctx->ds);
		P4_TOP(ctx->ds).u = P4_BOOL(w.u < x.u);
		NEXT;

		// ( n1 n2 -- )
_lt:		w = P4_DROPTOP(ctx->ds);
		P4_TOP(ctx->ds).u = P4_BOOL(w.n < x.n);
		NEXT;

		/*
		 * I/O
		 */
		// ( caddr +n1 -- +n2 )
_accept:	w = P4_DROPTOP(ctx->ds);
		if ((x.n = alineInput(stdin, "", w.s, x.z)) < 0) {
			THROW(P4_THROW_BAD_EOF);
		}
		P4_TOP(ctx->ds) = x;
		NEXT;

		// ( -- flag)
_refill:	w.n = p4Refill(ctx->input);
		p4AllocStack(ctx, &ctx->ds, 1);
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( -- n )
_key:		(void) fflush(stdout);
		if (ctx->unkey == EOF) {
			(void) alineSetMode(ALINE_RAW);
			x.n = alineReadByte();
		} else {
			x.n = ctx->unkey;
			ctx->unkey = EOF;
		}
		p4AllocStack(ctx, &ctx->ds, 1);
		P4_PUSH(ctx->ds, x.n);
		NEXT;

		// ( -- flag )
_key_ready:	(void) fflush(stdout);
		if (ctx->unkey == EOF) {
			(void) alineSetMode(ALINE_RAW_NB);
			ctx->unkey = alineReadByte();
		}
		p4AllocStack(ctx, &ctx->ds, 1);
		P4_PUSH(ctx->ds, (P4_Uint) P4_BOOL(ctx->unkey != EOF));
		NEXT;

		// ( caddr u -- )
_type:		P4_DROP(ctx->ds, 1);
		w = P4_POP(ctx->ds);
		(void) fwrite(w.s, sizeof (*w.s), x.z, stdout);
		NEXT;

		// ( char bool -- c-addr u )
_parse:		w = P4_DROPTOP(ctx->ds);
		str = p4Parse(ctx->input, w.u, x.u);
		P4_TOP(ctx->ds).s = str.string;
		P4_PUSH(ctx->ds, str.length);
		NEXT;

		// ( -- c-addr u )
_parse_name:	str = p4ParseName(ctx->input);
		p4AllocStack(ctx, &ctx->ds, 2);
		P4_PUSH(ctx->ds, str.string);
		P4_PUSH(ctx->ds, str.length);
		NEXT;

		// ( caddr u -- nt | 0 )
_find_name:	w = P4_DROPTOP(ctx->ds);
		P4_TOP(ctx->ds).nt = p4FindName(ctx, w.s, x.z);
		NEXT;

		// ( caddr u wid -- nt | 0 )
_find_name_in:	y = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).nt = p4FindNameIn(ctx, x.s, w.z, y.u);
		NEXT;

		// ( ms -- )
_ms:		P4_DROP(ctx->ds, 1);
		p4Nap(x.u / 1000L, (x.u % 1000L) * 1000000L);
		NEXT;

_epoch_seconds:	(void) time(&w.t);
		p4AllocStack(ctx, &ctx->ds, 1);
		P4_PUSH(ctx->ds, w);
		NEXT;

		/*
		 * Tools
		 */
		// ( -- aaddr n s )
_ds:		w.n = P4_LENGTH(ctx->ds);
		P4_PUSH(ctx->ds, ctx->ds.base);
		P4_PUSH(ctx->ds, w);
		P4_PUSH(ctx->ds, ctx->ds.size);
		NEXT;

		// ( -- aaddr n s )
_rs:		w.n = P4_LENGTH(ctx->rs);
		P4_PUSH(ctx->ds, ctx->rs.base);
		P4_PUSH(ctx->ds, w);
		P4_PUSH(ctx->ds, ctx->rs.size);
		NEXT;

		// ( -- )
_stack_check:	p4StackGuards(ctx);
		NEXT;

		// ( addr u -- )
_stack_dump:	P4_DROP(ctx->ds, 1);
		w = P4_POP(ctx->ds);
		p4StackDump(stdout, w.p, x.n);
		NEXT;

		// ( -- caddr u )
_post4_path:	if ((w.s = getenv("POST4_PATH")) == NULL) {
			w.s = P4_CORE_PATH;
		}
		x.z = strlen(w.s);
		P4_PUSH(ctx->ds, w);
		P4_PUSH(ctx->ds, x);
		NEXT;

		// ( -- caddr u )
_post4_commit:	P4_PUSH(ctx->ds, (char *) p4_commit);
		P4_PUSH(ctx->ds, sizeof (p4_commit)-1);
		NEXT;

		FILE *fp;
		struct stat sb;

		// ( -- fd )
_fa_stdin:	P4_PUSH(ctx->ds, (void *) stdin);
		NEXT;

		// ( -- fd )
_fa_stdout:	P4_PUSH(ctx->ds, (void *) stdout);
		NEXT;

		// ( fam1 -- fam2 )
_fa_bin:	P4_TOP(ctx->ds).u = x.u | 2;
		NEXT;

		// ( fd -- ior )
_fa_close:	errno = 0;
		(void) fclose(x.v);
		P4_TOP(ctx->ds).n = errno;
		NEXT;

		// ( caddr u -- ior )
_fa_delete:	errno = 0;
		P4_DROP(ctx->ds, 1);
		w = P4_TOP(ctx->ds);
		x.n = unlink(w.s);
		P4_TOP(ctx->ds).n = errno;
		NEXT;

		// ( caddr u fam -- fd ior )
		static char *fmodes[] = {
			"r", "r+", "rb", "rb+",		/* open */
			"w", "w+", "wb", "wb+"		/* create */
		};

_fa_create:	P4_TOP(ctx->ds).u = x.u | 4;
		/*@fallthrough@*/

		// ( caddr u fam -- fd ior )
_fa_open:	errno = 0;
		x = P4_POP(ctx->ds);
		y = P4_POP(ctx->ds);
		w = P4_TOP(ctx->ds);
		w.s = strndup(w.s, y.u);
		fp = fopen(w.s, fmodes[x.u]);
		free(w.s);
		P4_TOP(ctx->ds).v = fp;
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

		// ( sd.paths sd.file -- sd.path ior )
		P4_Cell z;
_fa_find_path:	x = P4_POP(ctx->ds);
		y = P4_POP(ctx->ds);
		z = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		str = p4FindFilePath(w.s, z.u, y.s, x.u);
		P4_PUSH(ctx->ds, str.string);
		P4_PUSH(ctx->ds, str.length);
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

		// ( caddr u1 fd -- u2 ior )
_fa_read:	errno = 0;
		fp = P4_POP(ctx->ds).v;
		x = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		w.u = fread(w.s, sizeof (*w.s), x.z, fp);
		P4_PUSH(ctx->ds, w);
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

		// ( caddr u1 fd -- u2 bool ior )
_fa_rline:	errno = 0;
		fp = P4_POP(ctx->ds).v;
		x = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		int eof = feof(fp);
		(void) fgets(w.s, (int) x.n, fp);
		x.z = strlen(w.s);
		if (0 < x.z && w.s[x.z-1] == '\n') {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsequence-point"
			x.z -= (0 < --x.z && w.s[x.z-1] == '\r');
#pragma GCC diagnostic pop
		}
		P4_PUSH(ctx->ds, x.z);
		P4_PUSH(ctx->ds, P4_BOOL(!eof));
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

		// ( fd -- ior )
_fa_flush:	errno = 0;
		(void) fflush(x.v);
		P4_TOP(ctx->ds).n = errno;
		NEXT;

		// ( fd -- ud ior )
_fa_fsize:	errno = 0;
		MEMSET(&sb, 0, sizeof (sb));
		(void) fstat(fileno(x.v), &sb);
		P4_TOP(ctx->ds).n = sb.st_size;
		p4AllocStack(ctx, &ctx->ds, 2);
		P4_PUSH(ctx->ds, (P4_Uint) 0);
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

		// ( caddr u -- x ior )
_fa_status:	errno = 0;
		w = P4_DROPTOP(ctx->ds);
		w.s = strndup(w.s, x.z);
		MEMSET(&sb, 0, sizeof (sb));
		(void) stat(w.s, &sb);
		free(w.s);
		P4_TOP(ctx->ds).u = sb.st_mode;
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

		// ( caddr u fd -- ior )
_fa_write:	errno = 0;
		fp = P4_POP(ctx->ds).v;
		x = P4_POP(ctx->ds);
		w = P4_TOP(ctx->ds);
		w.u = fwrite(w.s, sizeof (*w.s), x.z, fp);
		P4_TOP(ctx->ds).n = errno;
		NEXT;

		// ( ud fd -- ior )
_fa_seek:	errno = 0;
		fp = P4_POP(ctx->ds).v;
		P4_DROP(ctx->ds, 1);
		x = P4_POP(ctx->ds);
		(void) fseek(fp, x.z, SEEK_SET);
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

		// ( fd -- ud ior )
_fa_tell:	errno = 0;
		w.u = ftell(x.v);
		P4_TOP(ctx->ds) = w;
		p4AllocStack(ctx, &ctx->ds, 2);
		P4_PUSH(ctx->ds, (P4_Uint) 0);
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

		// ( -- aaddr n s )
_fs:		w.n = P4_LENGTH(ctx->fs);
		P4_PUSH(ctx->ds, ctx->fs.base);
		P4_PUSH(ctx->ds, w);
		P4_PUSH(ctx->ds, ctx->fs.size);
		NEXT;

#ifdef HAVE_MATH_H
_dofloat:	p4AllocStack(ctx, &ctx->P4_FLOAT_STACK, 1);
		P4_PUSH(ctx->P4_FLOAT_STACK, (P4_Float)w.xt->ndata);
		NEXT;

		// ( aaddr -- ) (F: -- f )
_f_fetch:	P4_DROP(ctx->ds, 1);
		p4AllocStack(ctx, &ctx->P4_FLOAT_STACK, 1);
		P4_PUSH(ctx->P4_FLOAT_STACK, *x.p);
		NEXT;

		// ( aaddr -- ) (F: f -- )
_f_store:	P4_DROP(ctx->ds, 1);
		w = P4_POP(ctx->P4_FLOAT_STACK);
		*x.p = w;
		NEXT;

		// (x -- )(R: -- x )
_fs_to_rs:	p4StackIsEmpty(ctx, &ctx->fs,P4_THROW_FS_UNDER);
		w = P4_POP(ctx->P4_FLOAT_STACK);
		p4AllocStack(ctx, &ctx->rs, 1);
		P4_PUSH(ctx->rs, w);
		P4STACKGUARDS(ctx);
		NEXT;

		// (F: -- f ; R: f -- )
_rs_to_fs:	p4StackIsEmpty(ctx, &ctx->rs, P4_THROW_RS_UNDER);
		w = P4_POP(ctx->rs);
		p4AllocStack(ctx, &ctx->P4_FLOAT_STACK, 1);
		P4_PUSH(ctx->P4_FLOAT_STACK, w);
		P4STACKGUARDS(ctx);
		NEXT;

		// (F: -- f )( caddr u -- bool )
		char *stop;
_to_float:	errno = 0;
		w = P4_DROPTOP(ctx->ds);
		y.f = strtod((const char *)w.s, &stop);
		P4_TOP(ctx->ds).u = P4_BOOL(errno == 0 && stop - (char *)w.s == x.n);
		if (P4_TOP(ctx->ds).n == P4_TRUE) {
			p4AllocStack(ctx, &ctx->P4_FLOAT_STACK, 1);
			P4_PUSH(ctx->P4_FLOAT_STACK, y);
		}
		NEXT;

		// (F: f -- )
_f_dot:		p4StackIsEmpty(ctx, &ctx->fs, P4_THROW_FS_UNDER);
		w = P4_POP(ctx->P4_FLOAT_STACK);
		(void) printf(P4_FLT_PRE_FMT" ", (int) ctx->precision, w.f);
		NEXT;

		// (F: f -- )
_f_sdot:	p4StackIsEmpty(ctx, &ctx->fs, P4_THROW_FS_UNDER);
		w = P4_POP(ctx->P4_FLOAT_STACK);
		(void) printf(P4_SCI_PRE_FMT" ", (int) ctx->precision, w.f);
		NEXT;

		// (F: f -- )(S: caddr u -- n sign ok )
		//
		char num[DECIMAL_DIG+STRLEN(".e-999")+1];
_f_represent:	P4_DROP(ctx->ds, 1);
		w = P4_POP(ctx->ds);
		y = P4_POP(ctx->P4_FLOAT_STACK);
		(void) snprintf(num, sizeof (num), P4_SCI_PRE_FMT, (int) x.n, fabs(y.f));
		int E = strcspn(num, "eE");
		num[E] = '\0';
		E = (int) strtol(num+E+1, NULL, 10);
		/* 12.6.1.2143 REPRESENT
		 * ... The character string shall consist of the u most
		 * significant digits of the significand represented as
		 * a decimal fraction with the IMPLIED DECIMAL POINT TO
		 * THE LEFT OF THE FIRST DIGIT, and the first digit zero
		 * only if all digits are zero.
		 *
		 * So not 1.2345e02, but 0.12345e03 => 123450 n=3 0 -1
		 */
		char *fraction = num+2;
		if (isdigit(*num) && *num != '0') {
			/* Shuffle leading integer digit, eg. 1.2345 -> .12345 */
			fraction = num+1;
			num[1] = num[0];
			E++;
		}
		(void) memmove(w.s, fraction, x.z);
		p4AllocStack(ctx, &ctx->ds, 1);
		P4_PUSH(ctx->ds, (P4_Int) E);
		P4_PUSH(ctx->ds, P4_BOOL(y.n < 0));
		P4_PUSH(ctx->ds, P4_BOOL(isdigit(*num) != 0));
		NEXT;

		// (F: f1 f2 -- f3 )
_f_add:		w = P4_POP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f += w.f;
		NEXT;

		// (F: f1 f2 -- f3 )
_f_sub:		w = P4_POP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f -= w.f;
		NEXT;

		// (F: f1 f2 -- f3 )
_f_mul:		w = P4_POP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f *= w.f;
		NEXT;

		// (F: f1 f2 -- f3 )
_f_div:		w = P4_POP(ctx->P4_FLOAT_STACK);
// With floating point, divide by zero doesn't generate SIGFPE.
//		if (w.f == 0) {
//			THROW(P4_THROW_DIV_ZERO);
//		}
		P4_TOP(ctx->P4_FLOAT_STACK).f /= w.f;
		NEXT;

		// (F: f -- )( -- bool )
_f_eq0:		w = P4_POP(ctx->P4_FLOAT_STACK);
		p4AllocStack(ctx, &ctx->ds, 1);
		P4_PUSH(ctx->ds, P4_BOOL(w.f == 0.0));
		NEXT;

		// (F: f -- )( -- bool )
_f_lt0:		w = P4_POP(ctx->P4_FLOAT_STACK);
		p4AllocStack(ctx, &ctx->ds, 1);
		P4_PUSH(ctx->ds, P4_BOOL(w.f < 0.0));
		NEXT;

		// (F: f1 -- f2 )
_f_sqr:		w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = sqrt(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_cos:		w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = cos(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_sin:		w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = sin(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_tan:		w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = tan(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_cosh:	w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = cosh(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_sinh:	w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = sinh(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_tanh:	w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = tanh(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_acos:	w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = acos(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_asin:	w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = asin(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_atan:	w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = atan(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_acosh:	w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = acosh(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_asinh:	w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = asinh(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_atanh:	w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = atanh(w.f);
		NEXT;

		// (F: w x -- rad )
_f_atan2:	x = P4_POP(ctx->P4_FLOAT_STACK);
		w = P4_TOP(ctx->P4_FLOAT_STACK);
		/* Return arctan w/x in the interval [−pi , +pi ] radians. */
		P4_TOP(ctx->P4_FLOAT_STACK).f = atan2(w.f, x.f);
		NEXT;

		// (F: f1 -- f2 )
_f_exp:		w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = exp(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_ln:		w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = log(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_log:		w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = log10(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_round:	w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = round(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_trunc:	w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = trunc(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_floor:	w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = floor(w.f);
		NEXT;

/* This doesn't work entirely as expected for all large values of n (MAX-N).
 *
 *	MAX-N DUP S>F F>S .s \ don't match
 *
 * Largest value appears to be:
 *
 *	0x001fffffffffffff DUP S>F F>S .s
 *
 * This is an artifact of moving between 64b int to 64-bit double; too small
 * to hold all values of "long int".  Using "long double" allows full range
 * of a "long int", but some older versions of gcc don't appear to support
 * all the "long double" math functions (NetBSD 9.3 gcc 7.5).
 *
 * Implementing D>F and F>D is another problem.
 */
		// (S: n -- ; F: -- f )
		// : S>F S>D D>F ;
_s_to_f:	P4_DROP(ctx->ds, 1);
		p4AllocStack(ctx, &ctx->P4_FLOAT_STACK, 1);
		P4_PUSH(ctx->P4_FLOAT_STACK, (P4_Float) x.n);
		NEXT;

		// (S: -- n ; F: f -- )
		// : F>S F>D D>S ;
_f_to_s:	w = P4_POP(ctx->P4_FLOAT_STACK);
		p4AllocStack(ctx, &ctx->ds, 1);
		P4_PUSH(ctx->ds, (P4_Int) w.f);
		NEXT;

		// (F: f1 f2 -- f3 )
_f_max:		w = P4_POP(ctx->P4_FLOAT_STACK);
		x = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = fmax(x.f, w.f);
		NEXT;

		// (F: f1 f2 -- f3 )
_f_min:		w = P4_POP(ctx->P4_FLOAT_STACK);
		x = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = fmin(x.f, w.f);
		NEXT;

		// (F: f1 f2 -- f3 )
_f_pow:		w = P4_POP(ctx->P4_FLOAT_STACK);
		x = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = pow(x.f, w.f);
		NEXT;
#endif
}

int
p4EvalFile(P4_Ctx *ctx, const char *file)
{
	FILE *fp;
	char *p4_path;
	P4_String str;
	int rc = P4_THROW_ENOENT;
	if (file == NULL || *file == '\0') {
		goto error0;
	}
	if ((p4_path = getenv("POST4_PATH")) == NULL) {
		p4_path = P4_CORE_PATH;
	}
	P4_INPUT_PUSH(ctx->input);
	str = p4FindFilePath(p4_path, strlen(p4_path), file, strlen(file));
	if (0 < str.length && (fp = fopen(str.string, "r")) != NULL) {
		p4ResetInput(ctx, fp);
		ctx->input->path = file;
		rc = p4Repl(ctx, P4_THROW_OK);
		(void) fclose(fp);
		free(str.string);
	}
	P4_INPUT_POP(ctx->input);
error0:
	return rc;
}

int
p4EvalString(P4_Ctx *ctx, const char *str, size_t len)
{
	int rc;
	P4_Input *input;

	/* Do not save STATE, see A.6.1.2250 STATE. */
	P4_INPUT_PUSH(ctx->input);
	input = ctx->input;
	input->fp = (FILE *) -1;
	input->buffer = (char *)str;
	input->length = len;
	input->offset = 0;
	input->blk = 0;
	input->path = "about:input/string";
	rc = p4Repl(ctx, P4_THROW_OK);
	P4_INPUT_POP(ctx->input);
	return rc;
}

JMP_BUF sig_break_glass;
static void sig_int(int);
static void sig_exit(int);

typedef struct {
	int signal;
	int exception;
	void (*new_handler)(int);
	void (*old_handler)(int);
} sig_map;

static sig_map signalmap[] = {
	/* User interrupt remain within the process. */
	{ SIGINT, P4_THROW_SIGINT, sig_int, NULL },
	{ SIGSEGV, P4_THROW_SIGSEGV, sig_int, NULL },
	{ SIGTERM, P4_THROW_SIGTERM, sig_exit, NULL },
#ifdef NDEBUG
	/* User clean exit without generating a core file. */
	{ SIGQUIT, P4_THROW_QUIT, sig_exit, NULL },
#endif
	{ 0, P4_THROW_OK, NULL, NULL }
};

static void
sig_int(int signum)
{
	for (sig_map *map = signalmap; map->signal != 0; map++) {
		if (signum == map->signal) {
			signum = map->exception;
			break;
		}
	}
	/*** Typically longjmp() from a signal handler is unsafe. ***/
	LONGJMP(sig_break_glass, signum);
}

static void
sig_exit(int signum)
{
	(void) fputs(crlf, stdout);
	exit(P4_EXIT_SIGNAL(signum));
}

void
sig_init(void)
{
	for (sig_map *map = signalmap; map->signal != 0; map++) {
		map->old_handler = signal(map->signal, map->new_handler);
	}
}

void
sig_fini(void)
{
	for (sig_map *map = signalmap; map->signal != 0; map++) {
		if (map->old_handler != NULL) {
			(void) signal(map->signal, map->old_handler);
			map->old_handler = NULL;
		}
	}
}
