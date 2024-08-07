/*
 * post4.c
 *
 * Copyright 2007, 2024 by Anthony Howe. All rights reserved.
 */

#include "post4.h"

/***********************************************************************
 *** Globals
 ***********************************************************************/

static P4_Word *p4_builtin_words;
static P4_Ctx * volatile signal_ctx;

static int is_tty;
#ifdef HAVE_TCGETATTR
static int tty_fd = -1;
static struct termios tty_raw;
static struct termios tty_raw_nb;
static struct termios tty_saved;
static struct termios *tty_mode;
#endif

static struct winsize window = {
	.ws_row = 24,
	.ws_col = 80,
};

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
static const char *p4_exceptions[] = {
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
	NULL
};
#endif

static const char crlf[] = "\r\n";

/***********************************************************************
 *** Context
 ***********************************************************************/

static void
sig_winch(int signum)
{
#if defined(HAVE_TCGETWINSIZE)
	if (is_tty) {
		(void) tcgetwinsize(0, &window);
	}
#elif defined(TIOCGWINSZ)
	if (is_tty) {
		(void) ioctl(0, TIOCGWINSZ, &window);
	}
#endif
}

static void
p4Fini(void)
{
#ifdef HAVE_TCSETATTR
	if (0 <= tty_fd) {
		(void) tcsetattr(tty_fd, TCSADRAIN, &tty_saved);
	}
#endif
}

/**
 */
void
p4Init(void)
{
	char *p;
	unsigned i;

	(void) atexit(p4Fini);

	signal(SIGWINCH, sig_winch);

	is_tty = isatty(fileno(stdin));
#ifdef ASSERT_LINE_BUFFERING
	setvbuf(stdout, NULL, _IOLBF, 0);
	setvbuf(stderr, NULL, _IOLBF, 0);
#endif

#ifdef HAVE_TCGETATTR
# ifdef HAVE_CTERMID
	tty_fd = open(ctermid(NULL), O_RDWR, S_IRWXU|S_IRWXG|S_IRWXO);
# else
	tty_fd = fileno(stdin);
# endif
	if (tty_fd != -1) {
		(void) tcgetattr(tty_fd, &tty_saved);
		tty_mode = &tty_saved;

		tty_raw = tty_saved;

		/* Non-canonical blocking input. */
		tty_raw.c_cc[VMIN] = 1;
		tty_raw.c_cc[VTIME] = 0;
		tty_raw.c_lflag |= ISIG;
		tty_raw.c_lflag &= ~(ICANON|ECHO|ECHONL|ECHOCTL);

		/* Non-canonical non-blocking input. */
		tty_raw_nb = tty_raw;
		tty_raw_nb.c_cc[VMIN] = 0;

		/* Disable ECHO now. This allows KEY and KEY? to
		 * function correctly with interactive scripts;
		 * see life.p4, pressing a key skips to the next
		 * set of patterns. ECHO is turned back on for
		 * line input like REFILL or ACCEPT.
		 */
//		(void) tcsetattr(tty_fd, TCSADRAIN, &tty_raw);
	}
#endif /* HAVE_TCGETATTR */

	sig_winch(SIGWINCH);
}

int
p4LoadFile(P4_Ctx *ctx, const char *file)
{
	struct stat sb;
	int rc = -1, cwd;
	char *path_copy, *path, *next;

	if (file == NULL || *file == '\0' || (cwd = open(".", O_RDONLY)) < 0) {
		goto error0;
	}
	if ((path_copy = getenv("POST4_PATH")) == NULL || *path_copy == '\0') {
		path_copy = P4_CORE_PATH;
	}
	if ((path_copy = strdup(path_copy)) == NULL) {
		goto error1;
	}
	for (next = path_copy; (path = strtok(next, ":")) != NULL; next = NULL) {
		if (stat(path, &sb) || !S_ISDIR(sb.st_mode) || chdir(path)) {
			continue;
		}
		if (stat(file, &sb) == 0 && S_ISREG(sb.st_mode)) {
			break;
		}
	}
	if (path == NULL) {
		warn("%s", file);
	} else {
		rc = p4EvalFile(ctx, file);
	}
error2:
	free(path_copy);
error1:
	(void) fchdir(cwd);
	(void) close(cwd);
error0:
	return rc;
}

/***********************************************************************
 *** Conversion API
 ***********************************************************************/

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
	switch (ch) {
	case 'a': return '\a';		/* bell */
	case 'b': return '\b';		/* backspace */
	case 'e': return '\033';	/* escape */
	case 'f': return '\f';		/* formfeed */
	case 'n': return '\n';		/* linefeed */
	case 'r': return '\r';		/* carriage-return */
	case 's': return ' ';		/* space */
	case 't': return '\t';		/* tab */
	case 'v': return '\v';		/* vertical tab */
	case 'z': return '\0';		/* nul */
	case '0': return '\0';		/* nul */
	case '?': return '\177';	/* delete */
	}
	return ch;			/* identity */
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

int
p4IsPrintable(const char *str, size_t u)
{
	for ( ; u < 0; u--) {
		if (!isprint(*str++)) {
			return 0;
		}
	}
	return 1;
}

int
p4StrNum(P4_String str, P4_Uint base, P4_Cell *out, int *is_float)
{
	P4_Cell num;
	int negate = 0;
	int offset = 0;

	out->n = 0;
	*is_float = 0;

	if (str.length == 0) {
		return 0;
	}

	switch (str.string[0]) {
	case '$':	/* $F9 hex */
		base = 16;
		offset++;
		break;
	case '#':	/* #99 decimal */
		base = 10;
		offset++;
		break;
	case '%':	/* %1011 binary */
		base = 2;
		offset++;
		break;
	case '0':	/* 0377 octal or 0xFF hex */
		if (2 < str.length && tolower(str.string[1]) == 'x') {
			offset += 2;
			base = 16;
		} else if (1 < str.length && (isdigit(str.string[1]) || str.string[1] == '-')) {
			offset++;
			base = 8;
		}
		break;
	case '\'':	/* 'c' and '\x' escaped characters */
		if (str.length == 3 && str.string[2] == '\'') {
			out->n = (P4_Int) str.string[1];
			return str.length;
		}
		/* Extension C style backslash literals */
		if (str.length == 4 && str.string[1] == '\\' && str.string[3] == '\'') {
			out->n = (P4_Int) p4CharLiteral(str.string[2]);
			return str.length;
		}
		/* Nothing parsed. */
		return 0;
	}

	if (offset < str.length) {
		switch (str.string[offset]) {
		case '-':
			negate = 1;
			/*@fallthrough@*/
		case '+':
			offset++;
		}
	}
	for (num.n = 0; offset < str.length; offset++) {
		int digit = p4Base36(str.string[offset]);
		if (base <= digit) {
#ifdef HAVE_MATH_H
			if (str.string[offset] == '.' || toupper(str.string[offset]) == 'E') {
				if (base != 10) {
					raise(SIGFPE);
				}
				*is_float = 1;
				unsigned char *stop;
				/* Note that 1E 0E 123E not accepted.  strtod expects a
				 * number after 'E'.  0.0, .0, 0E0, 123., 123.456 work fine.
				 */
				out->f = strtod(str.string, (char **)&stop);
				return stop - str.string;
			}
#endif
			break;
		}
		num.n = num.n * base + digit;
	}
	if (negate) {
		num.n = -num.n;
	}

	out->n = num.n;
	return offset;
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

	while (0 < (unslept = sleep(seconds)))
		seconds = unslept;
}
#endif
}

void
p4StackDump(FILE *fp, P4_Cell *base, P4_Uint length)
{
	P4_Cell *cell;
	unsigned count;

	for (count = 0, cell = base; 0 < length--; cell++) {
		if ((count & 3) == 0) {
			(void) fprintf(fp, "top-%.2u ", (unsigned) length);
		}
		(void) fprintf(fp, P4_HEX_FMT" ", cell->u);
		if ((++count & 3) == 0) {
			(void) fprintf(fp, crlf);
		}
	}
	if ((count & 3) != 0) {
		(void) fprintf(fp, crlf);
	}
}

void
p4MemDump(FILE *fp, P4_Char *addr, P4_Size length)
{
	P4_Char *s;
	unsigned count;

	s = addr;
	for (count = 0; count < length; addr++) {
		if ((count & 0xF) == 0) {
			/* Format with fixed width hex string, instead
			 * of as pointer to maintain the dump layout.
			 */
			(void) fprintf(fp, P4_HEX_FMT"  ", (long)addr);
			s = addr;
		}
		(void) fprintf(fp, "%.2x", (unsigned char) *addr);
		if ((++count & 0x1) == 0) {
			(void) fputc(' ', fp);
		}
		if ((count & 0xF) == 0) {
			(void) fputc(' ', fp);
			for ( ; s <= addr; s++) {
				(void) fputc(isprint(*s) ? *s : '.', fp);
			}
			(void) fprintf(fp, crlf);
		}
	}
	if ((count & 0xF) != 0) {
		do {
			(void) fprintf(fp, "  ");
			if ((++count & 0x1) == 0) {
				(void) fputc(' ', fp);
			}
		} while ((count & 0xF) != 0);
		(void) fputc(' ', fp);
		for ( ; s < addr; s++) {
			(void) fputc(isprint(*s) ? *s : '.', fp);
		}
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
#define CARRY_V2
#ifdef CARRY_V2
	*c0 = (al_bh << P4_HALF_SHIFT) + al_bl;
	P4_Uint carry = *c0 < al_bl;
	*c0 += (ah_bl << P4_HALF_SHIFT);
	carry += *c0 < (ah_bl << P4_HALF_SHIFT);
	*c1 = ah_bh + (ah_bl >> P4_HALF_SHIFT) + (al_bh >> P4_HALF_SHIFT) + carry;
#else
	P4_Uint carry = ((al_bl >> P4_HALF_SHIFT) + (P4_Uint_Half) al_bh + (P4_Uint_Half) ah_bl) >> P4_HALF_SHIFT;
	*c1 = ah_bh + (ah_bl >> P4_HALF_SHIFT) + (al_bh >> P4_HALF_SHIFT) + carry;
	*c0 = (ah_bl << P4_HALF_SHIFT) + (al_bh << P4_HALF_SHIFT) + al_bl;
#endif
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
	p4Mulu(a < 0 ? -a : a, b < 0 ? -b : b, c0, c1);
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
	x -= x >> 1 & (P4_Uint)0x5555555555555555L;
	x = (x >> 2 & (P4_Uint)0x3333333333333333L) + (x & (P4_Uint)0x3333333333333333L);
	x = (x >> 4) + x & (P4_Uint)0x0f0f0f0f0f0f0f0fL;
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
		(void) raise(SIGFPE);
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
	quot = (P4_Int) p4Divu(dend0, dend1, dsor < 0 ? -dsor : dsor, rem);
	if (sign < 0) {
		quot = -quot;
	}
	if (neg_rem) {
		*rem = -*rem;
	}
	return quot;
}

/***********************************************************************
 *** Input / Ouput
 ***********************************************************************/

#ifndef HAVE_TCSETATTR
int
p4SetNonBlocking(int fd, int flag)
{
	unsigned long flags;

	flags = (unsigned long) fcntl(fd, F_GETFL);

	if (flag)
		flags |= O_NONBLOCK;
	else
		flags &= ~O_NONBLOCK;

	return fcntl(fd, F_SETFL, flags);
}
#endif

P4_Int
p4ReadByte(int fd)
{
	unsigned char ch;

	if (read(fd, &ch, sizeof (ch)) != sizeof (ch)) {
		return EOF;
	}

	return ch;
}

P4_Int
p4GetC(P4_Input *input)
{
	if (input->fp == (FILE *) -1) {
		return input->offset < input->length ? input->buffer[input->offset++] : EOF;
	}
	return fgetc(input->fp == NULL ? stdin : input->fp);
}

P4_Int
p4Accept(P4_Input *input, P4_Char *buf, P4_Size size)
{
	int ch;
	P4_Char *ptr;

	if (input->fp == (FILE *) -1 || size-- <= 1) {
		return 0;
	}
#ifdef HAVE_TCSETATTR
	/* For a terminal restore original line input and echo settings. */
	if (is_tty && tty_mode != &tty_saved) {
		(void) tcsetattr(tty_fd, TCSADRAIN, &tty_saved);
		tty_mode = &tty_saved;
	}
#endif
	for (ptr = buf; ptr - buf < size; ) {
		if ((ch = p4GetC(input)) == EOF) {
			if (ptr - buf == 0) {
				return EOF;
			}
			break;
		}
		*ptr++ = (P4_Char) ch;
		if (ch == '\n') {
			break;
		}
	}

	return ptr - buf;
}

P4_Int
p4Refill(P4_Ctx *ctx, P4_Input *input)
{
	P4_Int n;

	if (P4_INPUT_IS_STR(ctx->input)) {
		return P4_FALSE;
	}
	if ((n = p4Accept(&ctx->input, ctx->input.buffer, ctx->input.size)) < 0) {
		return P4_FALSE;
	}
	input->length = n;
	input->offset = 0;

	return P4_TRUE;
}

/***********************************************************************
 *** Block I/O
 ***********************************************************************/

int
p4BlockGrow(int fd, P4_Uint block)
{
	size_t n;
	struct stat sb;
	P4_Char blanks[P4_BLOCK_SIZE];

	if (fstat(fd, &sb)) {
		return -1;
	}
	/* Is the file large enough to contain the requested block? */
	if (sb.st_size < block * P4_BLOCK_SIZE) {
		if (lseek(fd, 0, SEEK_END) == (off_t) -1) {
			return -1;
		}
		(void) memset(blanks, ' ', sizeof (blanks));

		/* P4_BLOCK_SIZE is a power of 2. */
		if ((n = (sb.st_size & (P4_BLOCK_SIZE-1))) != 0) {
			/* Extend the file to a multiple of block size. */
			if (write(fd, blanks, P4_BLOCK_SIZE - n) != P4_BLOCK_SIZE - n) {
				return -1;
			}
			sb.st_size = sb.st_size - n + P4_BLOCK_SIZE;
		}

		/* Extend the file with blank blocks. */
		for (n = sb.st_size / P4_BLOCK_SIZE; n < block; n++) {
			if (write(fd, blanks, P4_BLOCK_SIZE) != P4_BLOCK_SIZE) {
				return -1;
			}
		}
	}
	if (lseek(fd, (block - 1) * P4_BLOCK_SIZE, SEEK_SET) == (off_t) -1) {
		return -1;
	}

	return 0;
}

int
p4BlockRead(int fd, P4_Uint blk_num, P4_Block *block)
{
	if (fd <= 0 || blk_num == 0 || block == NULL) {
		return -1;
	}
	if (lseek(fd, (blk_num - 1) * P4_BLOCK_SIZE, SEEK_SET) == (off_t) -1) {
		return -1;
	}
	if (read(fd, block->buffer, P4_BLOCK_SIZE) != P4_BLOCK_SIZE) {
		return -1;
	}
	block->state = P4_BLOCK_CLEAN;
	block->number = blk_num;

	return 0;
}

int
p4BlockWrite(int fd, P4_Block *block)
{
	if (fd <= 0 || block == NULL) {
		return -1;
	}
	if (p4BlockGrow(fd, block->number)) {
		return -1;
	}
	if (write(fd, block->buffer, P4_BLOCK_SIZE) != P4_BLOCK_SIZE) {
		return -1;
	}
	block->state = P4_BLOCK_CLEAN;

	return 0;
}

int
p4BlockOpen(const char *file)
{
	int fd;

	if (file == NULL || *file == '\0') {
		return -1;
	}
	if ((fd = open(file, O_CREAT|O_RDWR, S_IRWXU|S_IRWXG|S_IRWXO)) < 0 || flock(fd, LOCK_EX|LOCK_NB)) {
		warn("%s", file);
	}

	return fd;
}

int
p4BlockClose(int fd, P4_Block *block)
{
	if (fd < 0 || block->state == P4_BLOCK_DIRTY && p4BlockWrite(fd, block)) {
		return -1;
	}
	block->state = P4_BLOCK_FREE;
	block->number = 0;
	return close(fd);
}

int
p4BlockBuffer(P4_Ctx *ctx, P4_Uint blk_num, int with_read)
{
	if (ctx->block_fd <= 0) {
		return P4_THROW_EIO;
	}
	if (blk_num == 0) {
		return P4_THROW_BLOCK_BAD;
	}
	if (blk_num == ctx->block.number) {
		return P4_THROW_OK;
	}
	/* Current there is no block buffer assignment strategy beyond
	 * a single buffer per context.  Might add one day.
	 */
	if (ctx->block.state == P4_BLOCK_DIRTY && p4BlockWrite(ctx->block_fd, &ctx->block)) {
		return P4_THROW_BLOCK_WR;
	}
	if (with_read && p4BlockRead(ctx->block_fd, blk_num, &ctx->block)) {
		return P4_THROW_BLOCK_RD;
	}
	ctx->block.state = P4_BLOCK_CLEAN;
	ctx->block.number = blk_num;
	return P4_THROW_OK;
}

/***********************************************************************
 *** Core
 ***********************************************************************/

void
p4WordFree(P4_Word *word)
{
	if (word != NULL) {
		free(word->name.string);
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
	if (ctx->here + n < (P4_Char *) ctx->words->data) {
		/* Attempt to release data space below the most recently
		 * created word.
		 */
		return NULL;
	}
	void *start = ctx->here;
	MEMSET(start, BYTE_ME, n);
	ctx->words->ndata += n;
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
	/* Make sure new word starts with aligned data. */
	(void) p4Allot(ctx, P4_ALIGN_BY((P4_Uint) ctx->here));
	word->data = (P4_Cell *) ctx->here;
	CHECK_ADDR(word->data);

	if ((word->name.string = strndup(name, length)) == NULL) {
		goto error1;
	}
	word->name.length = length;
	word->code = code;

	word->prev = ctx->words;
	ctx->words = word;

	return word;
error1:
	free(word);
error0:
	LONGJMP(ctx->on_throw, P4_THROW_ALLOCATE);
}

void
p4WordAppend(P4_Ctx *ctx, P4_Cell data)
{
	(void) p4Allot(ctx, P4_ALIGN_BY((P4_Uint) ctx->here));
	P4_Cell *here = p4Allot(ctx, sizeof (data));
	CHECK_ADDR(here);
	*here = data;
}

P4_Word *
p4FindName(P4_Ctx *ctx, P4_Char *caddr, P4_Size length)
{
	P4_Word *word;

	for (word = ctx->words; word != NULL; word = word->prev) {
		if (!P4_WORD_IS_HIDDEN(word)
		&& word->name.length > 0 && word->name.length == length
		&& strncasecmp((char *)word->name.string, caddr, length) == 0) {
			return word;
		}
	}

	return NULL;
}

void
p4Free(P4_Ctx *ctx)
{
	P4_Word *word, *prev;

	if (ctx != NULL) {
		for (word = ctx->words; p4_builtin_words != word; word = prev) {
			prev = word->prev;
			p4WordFree(word);
		}
		(void) p4BlockClose(ctx->block_fd, &ctx->block);
#if defined(HAVE_MATH_H)
		free(ctx->fs.base - P4_GUARD_CELLS/2);
#endif
		free(ctx->ds.base - P4_GUARD_CELLS/2);
		free(ctx->rs.base - P4_GUARD_CELLS/2);
		free(ctx->mem);
		free(ctx);
	}
}

static void
p4ResetInput(P4_Ctx *ctx)
{
	ctx->input.size = sizeof (ctx->tty);
	ctx->input.buffer = ctx->tty;
	ctx->input.length = 0;
	ctx->input.offset = 0;
	ctx->input.blk = 0;
}

static int
p4CreateStack(P4_Stack *stk, int size)
{
	if ((stk->base = calloc(size + P4_GUARD_CELLS, sizeof (*stk->base))) == NULL) {
		return -1;
	}
	/* Adjust base for underflow gurard. */
	stk->base += P4_GUARD_CELLS/2;
	stk->base[size].u = P4_SENTINEL;
	stk->base[-1].u = P4_SENTINEL;
	stk->top = stk->base - 1;
	stk->size = size;
	return 0;
}

P4_Ctx *
p4Create(P4_Options *opts)
{
	P4_Ctx *ctx;

	if ((ctx = calloc(1, sizeof (*ctx))) == NULL) {
		goto error0;
	}
	ctx->radix = 10;
	ctx->unkey = EOF;
	ctx->input.fp = stdin;
	p4ResetInput(ctx);
	ctx->argc = opts->argc;
	ctx->argv = opts->argv;
	ctx->state = P4_STATE_INTERPRET;

	/* GH-5 Clear initial memory space to placate Valgrind. */
	if ((ctx->mem = calloc(1, opts->mem_size * 1024)) == NULL) {
		goto error0;
	}
	/* GH-5 Setting memory to something other than zero can
	 * help debug possible memory use before initialising.
	 */
	MEMSET(ctx->mem, BYTE_ME, opts->mem_size * 1024);
	ctx->end = ctx->mem + opts->mem_size * 1024;
	ctx->here = ctx->mem;

#ifdef HAVE_MATH_H
	ctx->precision = 6;
	if (p4CreateStack(&ctx->fs, opts->fs_size)) {
		goto error0;
	}
#endif
	if (p4CreateStack(&ctx->rs, opts->rs_size)) {
		goto error0;
	}
	if (p4CreateStack(&ctx->ds, opts->ds_size)) {
		goto error0;
	}

	ctx->block_fd = p4BlockOpen(opts->block_file);

	if (p4_builtin_words == NULL) {
		/* Link up the base dictionary. */
		(void) p4EvalString(ctx, "", 0);
	}

	(void) p4LoadFile(ctx, opts->core_file);

	return ctx;
error0:
	p4Free(ctx);
	return NULL;
}

#ifdef NDEBUG
# define p4Bp(ctx)
#else
static void
p4Bp(P4_Ctx *ctx)
{
	int has_nl = ctx->input.buffer[ctx->input.length-1] == '\n';
	// Convert tabs to spaces in effort to get ^ to line up.
	for (int i = 0; i < ctx->input.length; i++) {
		if (ctx->input.buffer[i] == '\t') {
			ctx->input.buffer[i] == ' ';
		}
	}
	(void) printf(
		">> %.*s\r\n>> %*c\r\n",
		(int)ctx->input.length - has_nl, ctx->input.buffer,
		(int)ctx->input.offset, '^'
	);
}
#endif


#ifdef P4_TRACE
static void
p4Trace(P4_Ctx *ctx, P4_Xt xt)
{
	if (ctx->trace) {
		P4_Int depth = ctx->ds.top - ctx->ds.base + 1;
		(void) printf("%#lx %.*s\tdepth=%ld\r\n", xt, (int)xt->name.length, xt->name.string, depth);
	}
}
#else
# define p4Trace(c, xt)
#endif

#ifdef USE_STACK_CHECKS
static void
p4StackCanPopPush(P4_Ctx *ctx, P4_Stack *stack, unsigned pop, unsigned push)
{
	int over = P4_THROW_DS_OVER;
	int under = P4_THROW_DS_UNDER;
	unsigned length = (stack->top + 1 - stack->base);

	if (stack == &ctx->rs) {
		over = P4_THROW_RS_OVER;
		under = P4_THROW_RS_UNDER;
# if defined(HAVE_MATH_H)
	} else if (stack == &ctx->fs) {
		over = P4_THROW_FS_OVER;
		under = P4_THROW_FS_UNDER;
# endif
	}

	/* Stack has enough data to pop? */
	if (length < pop) {
		p4Bp(ctx);
		LONGJMP(ctx->on_throw, under);
	}
	/* Stack has enough space to push data? */
	if (stack->size < length + push - pop) {
		p4Bp(ctx);
		LONGJMP(ctx->on_throw, over);
	}
}
#else
# define p4StackCanPopPush(c, s, pop, push)
#endif

int
p4Repl(P4_Ctx *ctx)
{
	int rc;
	P4_Char *cstr;
	P4_Word *word;
	P4_String str;
	P4_Cell w, x, *ip;

	static P4_Word words[] = {
		P4_WORD("_repl",	&&_repl,	0, 0),
		P4_WORD("LIT",		&&_lit,		0, 0x01),		// historic
		P4_WORD(";",		&&_exit,	P4_BIT_HIDDEN, 0), // _seext
#ifdef HAVE_MATH_H
//		P4_WORD("min-float",	&&_min_float,	0, 0x01),	// p4
		P4_WORD("max-float",	&&_max_float,	0, 0x01),	// p4
		P4_WORD("_fs",		&&_fs,		0, 0x03),	// p4
		P4_WORD("_fsp_get",	&&_fsp_get,	0, 0x01),	// p4
		P4_WORD("_fsp_put",	&&_fsp_put,	0, 0x10),	// p4
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
		P4_WORD("F>S",		&&_f_to_s,	0, 0x100001),	// p4
		P4_WORD("S>F",		&&_s_to_f,	0, 0x010010),	// p4
		P4_WORD("fs>rs",	&&_fs_to_rs,	0, 0x100100),	// p4
		P4_WORD("rs>fs",	&&_rs_to_fs,	0, 0x011000),	// p4
#endif
#ifdef P4_FILE_ACCESS
		P4_WORD("BIN",			&&_fa_bin,	0, 0x01),
		P4_WORD("R/O",			&&_fa_ro,	0, 0x01),
		P4_WORD("R/W",			&&_fa_rw,	0, 0x01),
		P4_WORD("W/O",			&&_fa_wo,	0, 0x01),
		P4_WORD("CLOSE-FILE",		&&_fa_close,	0, 0x11),
		P4_WORD("CREATE-FILE",		&&_fa_create,	0, 0x22),
		P4_WORD("DELETE-FILE",		&&_fa_delete,	0, 0x21),
		P4_WORD("FILE-POSITION",	&&_fa_tell,	0, 0x12),
		P4_WORD("FILE-SIZE",		&&_fa_fsize,	0, 0x12),
		P4_WORD("FLUSH-FILE",		&&_fa_flush,	0, 0x11),
		P4_WORD("INCLUDE-FILE",		&&_fa_include,	0, 0x10),
		P4_WORD("OPEN-FILE",		&&_fa_open,	0, 0x22),
		P4_WORD("READ-FILE",		&&_fa_read,	0, 0x32),
		P4_WORD("READ-LINE",		&&_fa_rline,	0, 0x33),
		P4_WORD("REPOSITION-FILE",	&&_fa_seek,	0, 0x21),
		P4_WORD("WRITE-FILE",		&&_fa_write,	0, 0x31),
#endif
#ifdef P4_TRACE
		P4_WORD("TRACE",		&&_trace,	0, 0x01),	// p4
#endif
		/* Constants. */
		P4_WORD("/pad",			&&_pad_size,	0, 0x01),	// p4
		P4_WORD("address-unit-bits",	&&_char_bit,	0, 0x01),	// p4

		/* Internal support. */
		P4_WORD("_bp",		&&_bp,		0, 0),		// p4
		P4_WORD("_branch",	&&_branch,	P4_BIT_COMPILE, 0),	// p4
		P4_WORD("_branchz",	&&_branchz,	P4_BIT_COMPILE, 0x10),	// p4
		P4_WORD("_call",	&&_call,	P4_BIT_COMPILE, 0x0100),// p4
		P4_WORD("_ds",		&&_ds,		0, 0x03),	// p4
		P4_WORD("_dsp@",	&&_dsp_get,	0, 0x01),	// p4
		P4_WORD("_dsp!",	&&_dsp_put,	0, 0x10),	// p4
#ifdef HAVE_HOOKS
		P4_WORD("_hook_add",	&&_hook_add,	0, 0),		// p4
		P4_WORD("_hook_call",	&&_hook_call,	0, 0),		// p4
#endif
		P4_WORD("_longjmp",	&&_longjmp,	0, 0x10),	// p4
		P4_WORD("_rs",		&&_rs,		0, 0x03),	// p4
		P4_WORD("_rsp@",	&&_rsp_get,	0, 0x01),	// p4
		P4_WORD("_rsp!",	&&_rsp_put,	0, 0x10),	// p4
		P4_WORD("_stack_dump",	&&_stack_dump,	0, 0x20),	// p4
		P4_WORD("_window",	&&_window,	0, 0x02),	// p4

		/* Compiling Words */
		P4_WORD("compile-only",	&&_compile_only,0, 0),		// p4
		P4_WORD("compile-only?",&&_is_compile,	P4_BIT_COMPILE, 0),// p4
		P4_WORD(":NONAME",	&&_noname,	0, 0),
		P4_WORD(":",		&&_colon,	0, 0),
		P4_WORD(";",		&&_semicolon,	P4_BIT_IMM|P4_BIT_COMPILE, 0),
		P4_WORD(">BODY",	&&_body,	0, 0x01),
		P4_WORD("CREATE",	&&_create,	0, 0x01),
		P4_WORD("DOES>",	&&_does,	P4_BIT_COMPILE, 0),
		P4_WORD("EVALUATE",	&&_evaluate,	0, 0x20),
		P4_WORD("EXECUTE",	&&_execute,	0, 0x10),
		P4_WORD("EXIT",		&&_exit,	P4_BIT_COMPILE, 0),
		P4_WORD("IMMEDIATE",	&&_immediate,	0, 0),
		P4_WORD("immediate?",	&&_is_immediate,0, 0x01),	// p4
		P4_WORD("MARKER",	&&_marker,	0, 0),
		P4_WORD("STATE",	&&_state,	0, 0x01),

		/* Data Space - Alignment */
		P4_WORD("CELLS",	&&_cells,	0, 0x11),
		P4_WORD("CHARS",	&&_chars,	0, 0x11),
		P4_WORD("ALIGN",	&&_align,	0, 0),
		P4_WORD("ALLOT",	&&_allot,	0, 0x10),
		P4_WORD("HERE", 	&&_here_addr,	0, 0x01),
		P4_WORD(">here",	&&_here_offset,	0, 0x01),	// p4
		P4_WORD("UNUSED",	&&_unused,	0, 0x01),

		/* Data Space - Access */
		P4_WORD("_ctx",		&&_ctx,		0, 0x01),	// p4
		P4_WORD("!",		&&_store,	0, 0x20),
		P4_WORD(">R",		&&_to_rs,	0, 0x0110),	// allow interpret
		P4_WORD("@",		&&_fetch,	0, 0x11),
		P4_WORD("C!",		&&_cstore,	0, 0x20),
		P4_WORD("C@",		&&_cfetch,	0, 0x11),
		P4_WORD("DROP",		&&_drop,	0, 0x10),
		P4_WORD("DUP",		&&_dup,		0, 0x12),
		P4_WORD("MOVE",		&&_move,	0, 0x30),
		P4_WORD("PICK",		&&_pick,	0, 0),
		P4_WORD("R>",		&&_from_rs,	0, 0x1001),	// allow interpret
		P4_WORD("ROLL",		&&_roll,	0, 0),
		P4_WORD("SWAP",		&&_swap,	0, 0x22),
		P4_WORD("BASE",		&&_base,	0, 0x01),

		/* Dynamic Memory */
		P4_WORD("ALLOCATE",	&&_allocate,	0, 0x12),
		P4_WORD("FREE",	&&_free,	0, 0x11),
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
		P4_WORD("args",		&&_args,	0, 0x02),	// p4
		P4_WORD("bye-code",	&&_bye_code,	0, 0x10),	// p4
		P4_WORD("env",		&&_env,		0, 0x22),	// p4
#ifdef HAVE_SEE
		P4_WORD("_seext",	&&_seext,	0, 0x10),	// p4
#endif

		/* I/O */
		P4_WORD(">IN",		&&_input_offset,0, 0x01),
		P4_WORD("ACCEPT",	&&_accept,	0, 0),
		P4_WORD("BLK",		&&_blk,		0, 0),
		P4_WORD("BLOCK",	&&_block,	0, 0),
		P4_WORD("block-open",	&&_block_open,	0, 0),		// p4
		P4_WORD("block-close",	&&_block_close,	0, 0),		// p4
		P4_WORD("blocks",	&&_blocks, 	0, 0),		// p4
		P4_WORD("BUFFER",	&&_buffer,	0, 0),
		P4_WORD("DUMP",		&&_dump,	0, 0x20),
		P4_WORD("EMIT",		&&_emit,	0, 0x10),
		P4_WORD("EMPTY-BUFFERS", &&_empty_buffers, 0, 0),
		P4_WORD("epoch-seconds", &&_epoch_seconds, 0, 0x01),	// p4
		P4_WORD("FIND-NAME",	&&_find_name,	0, 0x21),
		P4_WORD("INCLUDED",	&&_included,	0, 0x20),
		P4_WORD("KEY",		&&_key,		0, 0x01),
		P4_WORD("KEY?",		&&_key_ready,	0, 0x01),
		P4_WORD("MS",		&&_ms,		0, 0x10),
		P4_WORD("_parse",	&&_parse,	0, 0x22),	// p4
		P4_WORD("PARSE-NAME",	&&_parse_name,	0, 0x02),
		P4_WORD("REFILL",	&&_refill,	0, 0x01),
		P4_WORD("SAVE-BUFFERS",	&&_save_buffers, 0, 0),
		P4_WORD("SOURCE",	&&_source,	0, 0x02),
		P4_WORD("SOURCE-ID",	&&_source_id,	0, 0),
		P4_WORD("TIME&DATE",	&&_time_date,	0, 0x06),
		P4_WORD("UPDATE",	&&_update,	0, 0),

		P4_WORD(NULL,		NULL,		0, 0),
	};

#define w_repl	words[0]	// See exec[].
#define w_lit	words[1]
#define w_semi	words[2]	// See _seext.

	/* When the REPL executes a word, it puts the XT of the word here
	 * and starts the machine with the IP pointed to exec[].  When the
	 * word completes the next XT (w_repl) transitions from threaded
	 * code back into the C driven REPL.
	 */
	static P4_Cell exec[] = { { 0 }, { .w = &w_repl } };

	if (p4_builtin_words == NULL) {
		/* Link up the base dictionary. */
		for (word = words; word->code != NULL; word++) {
			word[1].prev = word;
		}
		p4_builtin_words = word->prev;
		ctx->words = p4_builtin_words;
	}

#define NEXT		goto _next
#define THROW(x)	{ rc = (x); goto _thrown; }

	signal_ctx = ctx;
	SETJMP_PUSH(ctx->on_throw);
	if ((rc = SETJMP(ctx->on_throw)) != 0) {
_thrown:
		switch (rc) {
		case P4_THROW_ABORT_MSG:
			/* Displays its own message. */
		case P4_THROW_ABORT:
		case P4_THROW_QUIT:
			/* Historically no message, simply return to REPL. */
			break;
		default:
#ifdef USE_EXCEPTION_STRINGS
			(void) printf("%d thrown: %s", rc, P4_THROW_future <= rc && rc < 0 ? p4_exceptions[-rc] : "?");
#else
			(void) printf("%d thrown", rc);
#endif
		}

		/* Cannot not rely on ctx->state for compilation state, since
		 * its possible to temporarily change states in the middle of
		 * compiling a word, eg : word [ 123 ;
		 */
		if (P4_WORD_IS_HIDDEN(ctx->words)) {
			/* A thrown error while compiling a word leaves the
			 * definition in an incomplete state; discard it.
			 */
			P4_Word *word = ctx->words;
			(void) printf(
				" while compiling \"%s\"",
				word->name.length == 0 ? ":NONAME" : (char *)word->name.string
			);
			ctx->words = word->prev;
			/* Rewind HERE, does not free ALLOCATE data. */
			ctx->here = (P4_Char *) word->data;
			p4WordFree(word);
		}
		switch (rc) {
		case P4_THROW_ABORT_MSG:
			/* Displays its own message. */
		case P4_THROW_ABORT:
		case P4_THROW_QUIT:
			/* Historically no message, simply return to REPL. */
			break;
		default:
			(void) printf(crlf);
			(void) fflush(stdout);
		}

		switch (rc) {
		case P4_THROW_ABORT:
		case P4_THROW_ABORT_MSG:
		case P4_THROW_DS_OVER:
		case P4_THROW_DS_UNDER:
		case P4_THROW_UNDEFINED:
			P4_RESET(ctx->ds);
			/*@fallthrough@*/
#if defined(HAVE_MATH_H)
		case P4_THROW_FS_OVER:
		case P4_THROW_FS_UNDER:
			P4_RESET(ctx->fs);
			/*@fallthrough@*/
#endif
		case P4_THROW_QUIT:
		case P4_THROW_SIGSEGV:		/* Retain data stack. */
		case P4_THROW_RS_OVER:
		case P4_THROW_RS_UNDER:
		case P4_THROW_LOOP_DEPTH:
			P4_RESET(ctx->rs);
			/* Normally at this point one would reset input
			 * to the console, but that has problems.  Wait
			 * for the caller to resolve this by closing
			 * their files and popping the previous input
			 * context and/or re-asserting stdin.
			 *
			 * ctx->input.fp = stdin;
			 */
			/*@fallthrough@*/

		/* See 3.4.4 Possible actions on an ambiguous condition
		 *
		 * - display a message;
		 * - set interpretation state and begin text interpretation;
		 */
		default:
			/* Discard the current input buffer. */
			ctx->input.offset = ctx->input.length = 0;
			ctx->state = P4_STATE_INTERPRET;
		}

		/* Ensure we cleanup before return. */
		goto setjmp_cleanup;
	}
_repl:
	/* The input buffer might have been primed (EVALUATE, LOAD),
	 * so try to parse it first before reading more input.
	 */
	do {
		while (ctx->input.offset < ctx->input.length) {
			str = p4ParseName(&ctx->input);
			if (str.length == 0) {
				break;
			}
			word = p4FindName(ctx, str.string, str.length);
			if (word == NULL) {
				int is_float;
				if (p4StrNum(str, ctx->radix, &x, &is_float) != str.length) {
					/* Not a word, not a number. */
					(void) printf("\"%.*s\" ", (int)str.length, str.string);
					THROW(P4_THROW_UNDEFINED);
				}
				if (ctx->state == P4_STATE_COMPILE) {
					p4WordAppend(ctx, (P4_Cell) &w_lit);
					p4WordAppend(ctx, x);
#ifdef HAVE_MATH_H
				} else if (is_float) {
					p4StackCanPopPush(ctx, &ctx->P4_FLOAT_STACK, 0, 1);
					P4_PUSH(ctx->P4_FLOAT_STACK, x);
#endif
				} else {
					p4StackCanPopPush(ctx, &ctx->ds, 0, 1);
					P4_PUSH(ctx->ds, x);
				}
			} else if (ctx->state == P4_STATE_COMPILE && !P4_WORD_IS_IMM(word)) {
				p4WordAppend(ctx, (P4_Cell) word);
			} else {
				// Setup XT of word found to execute.
				exec[0].w = word;
				ip = exec;
				NEXT;
			}
		}
		if (P4_INTERACTIVE(ctx)) {
			(void) printf("ok ");
			(void) fflush(stdout);
		}
	} while (p4Refill(ctx, &ctx->input));

	if (P4_INTERACTIVE(ctx)) {
		(void) printf(crlf);
	}

setjmp_cleanup:
	SETJMP_POP(ctx->on_throw);
	return rc;

		// ( -- )
_bp:		p4Bp(ctx);
		/*@fallthrough@*/

		// Indirect threading.
_next:		w = *ip++;
		p4Trace(ctx, w.xt);
		/* Check data stack bounds. */
		p4StackCanPopPush(ctx, &ctx->ds, P4_DS_CAN_POP(w.w), P4_DS_CAN_PUSH(w.w));
		p4StackCanPopPush(ctx, &ctx->rs, P4_RS_CAN_POP(w.w), P4_RS_CAN_PUSH(w.w));
#ifdef HAVE_MATH_H
		p4StackCanPopPush(ctx, &ctx->P4_FLOAT_STACK, P4_FS_CAN_POP(w.w), P4_FS_CAN_PUSH(w.w));
#endif
		goto *w.xt->code;

		// ( xt -- )
_execute:	w = P4_POP(ctx->ds);
		p4Trace(ctx, w.xt);
		goto *w.xt->code;

		// ( i*x -- j*y )(R: -- ip)
_enter:		p4StackCanPopPush(ctx, &ctx->rs, 0, 1);
		P4_PUSH(ctx->rs, ip);
		// w contains xt loaded by _next or _execute.
		ip = w.xt->data;
		NEXT;

		// ( i*x -- i*x )(R:ip -- )
_exit:		p4StackCanPopPush(ctx, &ctx->rs, 1, 0);
		ip = P4_POP(ctx->rs).p;
		NEXT;

		// ( ex_code -- )
_bye_code:	w = P4_TOP(ctx->ds);
		exit((int) w.n);

		// ( -- aaddr )
_ctx:		P4_PUSH(ctx->ds, (P4_Cell *) ctx);
		NEXT;

		// ( -- )
_call:		w = *ip;
		P4_PUSH(ctx->rs, ip + 1);
		ip = (P4_Cell *)((P4_Char *) ip + w.n);
		NEXT;

		// ( -- )
_branch:	w = *ip;
		ip = (P4_Cell *)((P4_Char *) ip + w.n);
		NEXT;

		// ( flag -- )
_branchz:	w = *ip;
		x = P4_POP(ctx->ds);
		ip = (P4_Cell *)((P4_Char *) ip + (x.u == 0 ? w.n : P4_CELL));
		NEXT;

#ifdef HAVE_HOOKS
		// ( func `<spaces>name` -- )
_hook_add:	str = p4ParseName(&ctx->input);
		p4WordCreate(ctx, str.string, str.length, &&_hook_call);
		w = P4_POP(ctx->ds);
		p4WordAppend(ctx, w);
		NEXT;

		// ( i*x -- j*y )
_hook_call:	x = w.w->data[0];
		(*(void (*)(P4_Ctx *)) x.p)(ctx);
		NEXT;
#endif

		// ( -- aaddr )
_dsp_get:	w.p = ctx->ds.top;
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( aaddr -- )
_dsp_put:	w = P4_POP(ctx->ds);
		ctx->ds.top = w.p;
		NEXT;

		// ( -- aaddr )
_rsp_get:	w.p = ctx->rs.top;
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( aaddr -- )
_rsp_put:	w = P4_POP(ctx->ds);
		ctx->rs.top = w.p;
		NEXT;

#if defined(HAVE_MATH_H)
		// ( -- aaddr )
_fsp_get:	w.p = ctx->fs.top;
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( aaddr -- )
_fsp_put:	w = P4_POP(ctx->ds);
		ctx->fs.top = w.p;
		NEXT;
#endif

		// ( n -- )
_longjmp:	w = P4_POP(ctx->ds);
		THROW((int) w.n);

		// ( -- x )
		// : lit r> dup cell+ >r @ ;
_lit:		w = *ip++;
		P4_PUSH(ctx->ds, w);
		NEXT;

		/*
		 * Environment Constants.
		 */
		// ( -- u )
_char_bit:	P4_PUSH(ctx->ds, (P4_Uint) P4_CHAR_BIT);
		NEXT;

		// ( n1 -- n2 )
_chars:		P4_TOP(ctx->ds).n *= sizeof (P4_Char);
		NEXT;

		// ( n1 -- n2 )
_cells:		P4_TOP(ctx->ds).n *= P4_CELL;
		NEXT;

		/*
		 * Defining words.
		 */
_noname:	str.string = "";
		str.length = 0;
		goto _do_colon;

_colon:		str = p4ParseName(&ctx->input);
		goto _do_colon;

		// (C: -- colon) (R: -- ip)
		// Save the current lengths so we can check for imbalance.
_do_colon:	ctx->state = P4_STATE_COMPILE;
#ifdef USE_STACK_CHECKS
		/* Save stack lengths for control imbalance test below. */
		w.u = P4_LENGTH(ctx->ds);
		P4_PUSH(ctx->ds, w);
#endif
		word = p4WordCreate(ctx, str.string, str.length, &&_enter);
		/* Keep new word hidden while compiling. */
		P4_WORD_SET_HIDDEN(word);
		NEXT;

		// (C: colon -- ) (R: ip -- )
_semicolon:	ctx->state = P4_STATE_INTERPRET;
#ifdef USE_STACK_CHECKS
		w = P4_POP(ctx->ds);
		if (w.u != P4_LENGTH(ctx->ds)) {
			/* Control structure imbalance.  Did we match
			 * all the IF-THEN, BEGIN-REPEAT, etc.
			 */
			THROW(P4_THROW_BAD_CONTROL);
		}
#endif
		p4WordAppend(ctx, (P4_Cell) &w_semi);
		if (ctx->words->name.length == 0) {
			/* :NONAME leaves xt on stack. */
			P4_PUSH(ctx->ds, ctx->words);
		} else {
			P4_WORD_CLEAR_HIDDEN(ctx->words);
		}
		NEXT;

		// ( -- )
_compile_only:	P4_WORD_SET_COMPILE(ctx->words);
		NEXT;

		// ( xt -- bool )
_is_compile:	w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).n = P4_BOOL(P4_WORD_IS_COMPILE(w.xt));
		NEXT;

		// ( -- )
_immediate:	P4_WORD_SET_IMM(ctx->words);
		NEXT;

		// ( xt -- bool )
_is_immediate:	w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).n = P4_BOOL(P4_WORD_IS_IMM(w.xt));
		NEXT;

_marker:	str = p4ParseName(&ctx->input);
		(void) p4WordCreate(ctx, str.string, str.length, &&_rm_marker);
		NEXT;

_rm_marker:	x.w = w.xt;
		for (word = ctx->words; word != x.w; word = w.w) {
			w.w = word->prev;
			p4WordFree(word);
		}
		ctx->words = word->prev;
		/* Rewind HERE, does not free ALLOCATE data. */
		ctx->here = (P4_Char *) word->data;
		p4WordFree(word);
		NEXT;

		// ( i*x caddr u -- j*x )
_evaluate:	w = P4_POP(ctx->ds);
		x = P4_POP(ctx->ds);
		(void) p4EvalString(ctx, x.s, w.u);
		NEXT;


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
		// ( -- addr )
_create:	str = p4ParseName(&ctx->input);
		word = p4WordCreate(ctx, str.string, str.length, &&_data_field);
		// Reserve the 1st data cell for possible DOES>; wasted otherwise.
		p4WordAppend(ctx, (P4_Cell)(P4_Int) 0),
		P4_WORD_SET_CREATED(word);
		NEXT;

		// DOES>
_does:		word = ctx->words;
		if (!P4_WORD_WAS_CREATED(word)) {
			THROW(P4_THROW_NOT_CREATED);
		}
		word->code = &&_do_does;
#ifdef HAVE_SEE
		/*** If we change (again) how a P4_Word and data are
		 *** stored in memory, then most likely need to fix
		 *** this and _seext.
		 ***/
		// Save defining word's xt for _seext.
		x = P4_TOP(ctx->rs);
		p4WordAppend(ctx, *--x.p);
#endif
		// Append the IP of the words following DOES> of the defining
		// word after the data of the current word being defined.
		//
		//	: word CREATE ( store data) DOES> ( code words) ;
		//	                                  ^--- IP
		word->data[0].p = ip;
		goto _exit;

		// ( -- aaddr)
_do_does:	P4_PUSH(ctx->ds, w.xt->data + 1);
		// Remember who called us.
		p4StackCanPopPush(ctx, &ctx->rs, 0, 1);
		P4_PUSH(ctx->rs, ip);
		// Continue execution just after DOES> of the defining word.
		ip = w.xt->data[0].p;
		NEXT;

		// ( xt -- addr )
_body:		w = P4_POP(ctx->ds);
		if (!P4_WORD_WAS_CREATED(w.w)) {
			THROW(P4_THROW_NOT_CREATED);
		}
		/* fallthrough */

		// ( -- addr )
		// w contains xt loaded by _next or _execute.;
_data_field:	P4_PUSH(ctx->ds, w.xt->data + 1);
		NEXT;

		// ( n -- )
_allot:		w = P4_POP(ctx->ds);
		if (p4Allot(ctx, w.n) == NULL) {
			THROW(P4_THROW_ALLOCATE);
		}
		NEXT;

		// ( -- )
_align:		if (p4Allot(ctx, P4_ALIGN_BY((P4_Uint) ctx->here)) == NULL) {
			THROW(P4_THROW_ALLOCATE);
		}
		NEXT;

		/*
		 * Context variables
		 */
		// ( -- argv argc )
_args:		P4_PUSH(ctx->ds, (P4_Cell *) ctx->argv);
		P4_PUSH(ctx->ds, (P4_Int) ctx->argc);
		NEXT;

		// ( key k -- value v )
_env:		P4_DROP(ctx->ds, 1);		// Ignore k, S" NUL terminates.
		w = P4_TOP(ctx->ds);
		x.s = getenv(w.s);
		P4_TOP(ctx->ds) = x;
		P4_PUSH(ctx->ds, (P4_Int)(x.s == NULL ? -1 : strlen(x.s)));
		NEXT;

		// ( -- addr )
_state:		P4_PUSH(ctx->ds, (P4_Cell *) &ctx->state);
		NEXT;

#ifdef P4_TRACE
		// ( -- addr )
_trace:		P4_PUSH(ctx->ds, (P4_Cell *) &ctx->trace);
		NEXT;
#endif

		/*
		 * Numeric formatting
		 */
		// ( -- addr )
_base:		P4_PUSH(ctx->ds, (P4_Cell *) &ctx->radix);
		NEXT;

		// ( -- u )
_pad_size:	P4_PUSH(ctx->ds, (P4_Uint) P4_PAD_SIZE);
		NEXT;

		// ( -- rows cols )
_window:	P4_PUSH(ctx->ds, (P4_Uint) window.ws_row);
		P4_PUSH(ctx->ds, (P4_Uint) window.ws_col);
		NEXT;

		/*
		 * Memory access.
		 */
		// ( caddr -- char )
_cfetch:	w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).u = *w.s;
		NEXT;

		// ( char caddr -- )
_cstore:	w = P4_POP(ctx->ds);
		*w.s = P4_POP(ctx->ds).u;
		NEXT;

		// ( aaddr -- x )
_fetch:		w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds) = *w.p;
		NEXT;

		// ( x aaddr -- )
_store:		w = P4_POP(ctx->ds);
		x = P4_POP(ctx->ds);
		*w.p = x;
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
_here_offset:	P4_PUSH(ctx->ds, (P4_Size)(ctx->here - (P4_Char *) ctx->words->data));
		NEXT;

		// ( -- addr )
_here_addr:	P4_PUSH(ctx->ds, ctx->here);
		NEXT;

		// ( -- u )
_unused:	P4_PUSH(ctx->ds, ctx->end - ctx->here);
		NEXT;

		/*
		 * Dynamic Memory
		 */
		// ( u -- aaddr ior )
_allocate:	w = P4_TOP(ctx->ds);
		/* GH-5 Check for possibly negative size.  A size_t is a positive
		 * value so -1 would be 0xFFFF...FFFF and technically allowed
		 * but so large as to be impractical and possibly a type error,
		 * conversion error, or some size miscalculation.  So -1024..-1
		 * is reserved for trapping this possible error.  Not perfect,
		 * but should be handle most cases.
		 */
		if (w.n < 0 && -1024 <= w.n) {
			P4_TOP(ctx->ds) = w;
			P4_PUSH(ctx->ds, (P4_Int) ENOMEM);
			NEXT;
		}
		errno = 0;
		x.s = malloc((size_t) w.u);
		MEMSET(x.s, BYTE_ME, w.u);
		P4_TOP(ctx->ds) = x;
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

		// ( aaddr -- ior )
_free:		w = P4_TOP(ctx->ds);
		free(w.s);
		P4_TOP(ctx->ds).n = 0;
		NEXT;

		// ( aaddr1 u -- aaddr2 ior )
_resize:	w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		/* GH-5 Check for possibly negative size.  See above. */
		if (w.n < 0 && -1024 <= w.n) {
			P4_TOP(ctx->ds) = x;
			P4_PUSH(ctx->ds, (P4_Int) ENOMEM);
			NEXT;
		}
		errno = 0;
		w.s = realloc(x.s, (size_t) w.u);
		P4_TOP(ctx->ds) = w.s == NULL ? x : w;
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;


		/*
		 * Stack manipulation.
		 */
		// ( x -- )
_drop:		P4_DROP(ctx->ds, 1);
		NEXT;

		// ( x -- x x )
_dup:		w = P4_TOP(ctx->ds);
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( xu ... x1 x0 u -- xu ... x1 x0 xu )
		// Could be implemented in Post4 minus the stack depth check.
		// : PICK >R _ds DROP 1 - CELLS + R> CELLS - @ ;
		// 0 PICK == DUP, 1 PICK == OVER
_pick:		w = P4_POP(ctx->ds);
		/* Check stack depth. */
		p4StackCanPopPush(ctx, &ctx->ds, w.u+1, 0);
		x = P4_PICK(ctx->ds, w.u);
		P4_PUSH(ctx->ds, x);
		NEXT;

		// ( x y -- y x )
		// 1 ROLL == SWAP
_swap:		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds) = w;
		P4_PUSH(ctx->ds, x);
		NEXT;

		// ( xu xu-1 ... x0 u –– xu-1 ... x0 xu )
		// 0 ROLL == noop, 1 ROLL == SWAP, 2 ROLL == ROT
_roll:		w = P4_POP(ctx->ds);
		/* Check stack depth. */
		p4StackCanPopPush(ctx, &ctx->ds, w.n+1, 0);
		x = P4_PICK(ctx->ds, w.n);
		for ( ; 0 < w.u; w.u--) {
			P4_PICK(ctx->ds, w.n) = P4_PICK(ctx->ds, w.n-1);
		}
		P4_TOP(ctx->ds) = x;
		NEXT;

		// (x -- )(R: -- x )
_to_rs:		w = P4_POP(ctx->ds);
		P4_PUSH(ctx->rs, w);
		NEXT;

		// (R: x -- )
_from_rs:	w = P4_POP(ctx->rs);
		P4_PUSH(ctx->ds, w);
		NEXT;

		/*
		 * Operators
		 */
		// ( n1 n2 -- n3 )
_add:		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).n += w.n;
		NEXT;

		// ( n1 n2 -- n3 )
_sub:		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).n -= w.n;
		NEXT;

		// ( n1 n2 -- n3 )
_mul:		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).n *= w.n;
		NEXT;

		// ( n1 n2 -- n3 )
_div:		w = P4_POP(ctx->ds);
		if (w.n == 0) {
			THROW(P4_THROW_DIV_ZERO);
		}
		P4_TOP(ctx->ds).n /= w.n;
		NEXT;

		// n1 n2 -- d (lo hi)
		P4_Cell c0, c1;
_mstar:		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		p4Muls(w.n, x.n, &c0.n, &c1.n);
		P4_TOP(ctx->ds).u = c0.n;
		P4_PUSH(ctx->ds, c1.n);
		NEXT;

		// n1 n2 -- ud (lo hi)
_umstar:	w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		p4Mulu(w.u, x.u, &c0.u, &c1.u);
		P4_TOP(ctx->ds).u = c0.u;
		P4_PUSH(ctx->ds, c1.u);
		NEXT;

	{	// ( d dsor -- rem quot )
		// C99+ specifies symmetric division.
		// Dividend Divisor Remainder Quotient
		//       10       7         3        1
		//      -10       7        -3       -1
		//       10      -7         3       -1
		//      -10      -7        -3        1
		//
		P4_Cell d;
_sm_div_rem:	d = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		if (d.n == 0) {
			THROW(P4_THROW_DIV_ZERO);
		}
		w.n = p4Divs(x.n, w.n, d.n, &x.n);
		P4_TOP(ctx->ds).n = x.n;
		P4_PUSH(ctx->ds, w.n);
		NEXT;

		// ( ud dsor -- mod quot )
_um_div_mod:	d = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		if (d.n == 0) {
			THROW(P4_THROW_DIV_ZERO);
		}
		w.u = p4Divu(x.u, w.u, d.u, &x.u);
		P4_TOP(ctx->ds).u = x.u;
		P4_PUSH(ctx->ds, w.u);
		NEXT;
	}
		// ( n1 n2 -- n3 )
_mod:		w = P4_POP(ctx->ds);
		if (w.n == 0) {
			THROW(P4_THROW_DIV_ZERO);
		}
		P4_TOP(ctx->ds).n %= w.n;
		NEXT;

		// ( x1 x2 -- x3 )
_and:		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).u &= w.u;
		NEXT;

		// ( x1 x2 -- x3 )
_or:		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).u |= w.u;
		NEXT;

		// ( x1 x2 -- x3 )
_xor:		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).u ^= w.u;
		NEXT;

		// ( n1 -- n2 )
_not:		w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).u = ~w.u;
		NEXT;

		// ( x1 u -- x2 )
_lshift:	w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).u <<= w.u;
		NEXT;

		// ( x1 u -- x2 )
_rshift:	w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).u >>= w.u;
		NEXT;


		/*
		 * Comparision
		 */
		// ( x -- flag )
_eq0:		w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).u = P4_BOOL(w.u == 0);
		NEXT;

		// ( x -- flag )
_lt0:		w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).u = P4_BOOL(w.n < 0);
		NEXT;

		// ( u1 u2 -- )
_u_lt:		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).u = P4_BOOL(x.u < w.u);
		NEXT;

		// ( n1 n2 -- )
_lt:		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).u = P4_BOOL(x.n < w.n);
		NEXT;

		/*
		 * I/O
		 */
		// ( -- u )
_input_offset:	P4_PUSH(ctx->ds, (P4_Cell *) &ctx->input.offset);
		NEXT;

		// ( -- caddr u )
_source:	P4_PUSH(ctx->ds, ctx->input.buffer);
		P4_PUSH(ctx->ds, ctx->input.length);
		NEXT;

		// ( -- -1 | 0 | fp )
		// Alias FILE *stdin to NULL.
_source_id:	P4_PUSH(ctx->ds, (P4_Cell *)(ctx->input.fp == stdin ? NULL : ctx->input.fp));
		NEXT;

		// ( caddr +n1 -- +n2 )
_accept:	w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		w.u = p4Accept(&ctx->input, x.s, w.u);
		P4_TOP(ctx->ds) = w;
		NEXT;

		// ( -- flag)
_refill:	w.n = p4Refill(ctx, &ctx->input);
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( -- n )
_key:		(void) fflush(stdout);
		if (ctx->unkey == EOF) {
#ifdef HAVE_TCSETATTR
			if (is_tty && tty_mode != &tty_raw) {
				(void) tcsetattr(tty_fd, TCSANOW, &tty_raw);
				tty_mode = &tty_raw;
			}
#endif
			x.n = p4ReadByte(tty_fd);
		} else {
			x.n = ctx->unkey;
			ctx->unkey = EOF;
		}
		P4_PUSH(ctx->ds, x.n);
		NEXT;

		// ( -- flag )
_key_ready:	(void) fflush(stdout);
		if (ctx->unkey == EOF) {
#ifdef HAVE_TCSETATTR
			if (is_tty && tty_mode != &tty_raw_nb) {
				(void) tcsetattr(tty_fd, TCSANOW, &tty_raw_nb);
				tty_mode = &tty_raw_nb;
			}
			ctx->unkey = p4ReadByte(tty_fd);
#else
			if (p4SetNonBlocking(tty_fd, 1) == 0) {
				ctx->unkey = p4ReadByte(tty_fd);
				(void) p4SetNonBlocking(tty_fd, 0);
			}
#endif
		}
		P4_PUSH(ctx->ds, (P4_Uint) P4_BOOL(ctx->unkey != EOF));
		NEXT;

		// ( c -- )
_emit:		w = P4_POP(ctx->ds);
		(void) fputc(w.n, stdout);
		NEXT;

		// ( caddr u -- )
_included:	w = P4_POP(ctx->ds);
		x = P4_POP(ctx->ds);
		if ((cstr = strndup(x.s, w.u)) == NULL) {
			THROW(P4_THROW_ALLOCATE);
		}
		(void) p4LoadFile(ctx, cstr);
		free(cstr);
		NEXT;


		/*
		 * Block I/O
		 */
		// ( -- aaddr )
_blk:		P4_PUSH(ctx->ds, (P4_Cell *) &ctx->input.blk);
		NEXT;

		// ( u -- aaddr )
_block:		w = P4_TOP(ctx->ds);
		if ((rc = p4BlockBuffer(ctx, w.u, 1)) != P4_THROW_OK) {
			THROW(rc);
		}
		P4_TOP(ctx->ds).s = ctx->block.buffer;
		NEXT;

		// ( caddr u -- bool )
_block_open:	w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		(void) p4BlockClose(ctx->block_fd, &ctx->block);
		ctx->block_fd = p4BlockOpen(x.s);
		P4_TOP(ctx->ds).n = P4_BOOL(0 < ctx->block_fd);
		NEXT;

		// ( -- )
_block_close:	(void) p4BlockClose(ctx->block_fd, &ctx->block);
		NEXT;

	{	// ( -- u )
		struct stat sb;
_blocks:	if (fstat(ctx->block_fd, &sb) != 0) {
			THROW(P4_THROW_EIO);
		}
		w.u = sb.st_size / P4_BLOCK_SIZE;
		P4_PUSH(ctx->ds, w);
		NEXT;
	}
		// ( u -- aaddr )
_buffer:	w = P4_TOP(ctx->ds);
		if ((rc = p4BlockBuffer(ctx, w.u, 0)) != P4_THROW_OK) {
			THROW(rc);
		}
		P4_TOP(ctx->ds).s = ctx->block.buffer;
		NEXT;

		// ( -- )
_empty_buffers:	ctx->block.state = P4_BLOCK_FREE;
		/*@fallthrough@*/

		// ( -- )
_save_buffers:	if (ctx->block.state == P4_BLOCK_DIRTY && p4BlockWrite(ctx->block_fd, &ctx->block)) {
			THROW(P4_THROW_BLOCK_WR);
		}
		NEXT;

		// ( -- )
_update:	ctx->block.state = P4_BLOCK_DIRTY;
		NEXT;


		/*
		 */
		// ( char bool -- c-addr u )
_parse:		x = P4_POP(ctx->ds);
		w = P4_TOP(ctx->ds);
		str = p4Parse(&ctx->input, w.u, x.u);
		P4_TOP(ctx->ds).s = str.string;
		P4_PUSH(ctx->ds, str.length);
		NEXT;

		// ( -- c-addr u )
_parse_name:	str = p4ParseName(&ctx->input);
		P4_PUSH(ctx->ds, str.string);
		P4_PUSH(ctx->ds, str.length);
		NEXT;

		// ( caddr u -- xt | 0 )
_find_name:	w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).w = p4FindName(ctx, x.s, w.z);
		NEXT;

		// ( ms -- )
_ms:		w = P4_POP(ctx->ds);
		p4Nap(w.u / 1000L, (w.u % 1000L) * 1000000L);
		NEXT;

	{	// ( -- sec min hour day month year )
		struct tm *now;
_time_date:	(void) time(&w.t);
		now = localtime(&w.t);
		P4_PUSH(ctx->ds, (P4_Int) now->tm_sec);
		P4_PUSH(ctx->ds, (P4_Int) now->tm_min);
		P4_PUSH(ctx->ds, (P4_Int) now->tm_hour);
		P4_PUSH(ctx->ds, (P4_Int) now->tm_mday);
		P4_PUSH(ctx->ds, (P4_Int) now->tm_mon+1);
		P4_PUSH(ctx->ds, (P4_Int) now->tm_year+1900);
		NEXT;
	}

_epoch_seconds:	(void) time(&w.t);
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

		// ( addr u -- )
_stack_dump:	x = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		p4StackDump(stdout, w.p, x.u);
		NEXT;

		// ( addr u -- )
_dump:		x = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		p4MemDump(stdout, w.s, x.u);
		NEXT;

#ifdef P4_FILE_ACCESS
	{
		FILE *fp;
		struct stat sb;
		static char *fmodes[] = {
			"a+",  "r", "w", "w+", "ab+", "rb", "wb", "wb+"
		};

_fa_ro:		P4_PUSH(ctx->ds, (P4_Uint) 1);
		NEXT;

_fa_wo:		P4_PUSH(ctx->ds, (P4_Uint) 2);
		NEXT;

_fa_rw:		P4_PUSH(ctx->ds, (P4_Uint) 3);
		NEXT;

_fa_bin:	x = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).u = x.u | 4;
		NEXT;

_fa_close:	errno = 0;
		(void) fclose(P4_TOP(ctx->ds).v);
		P4_TOP(ctx->ds).n = errno;
		NEXT;

_fa_delete:	errno = 0;
		P4_DROP(ctx->ds, 1);
		w = P4_TOP(ctx->ds);
		x.n = unlink(w.s);
		P4_TOP(ctx->ds).n = errno;
		NEXT;

_fa_create:
_fa_open:	errno = 0;
		x = P4_POP(ctx->ds);
		P4_DROP(ctx->ds, 1);
		w = P4_TOP(ctx->ds);
		fp = fopen((const char *) w.s, fmodes[x.u]);
		P4_TOP(ctx->ds).v = fp;
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

_fa_read:	fp = P4_POP(ctx->ds).v;
		x = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		w.u = fread(w.s, sizeof (*w.s), x.u, fp);
		P4_PUSH(ctx->ds, w);
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

_fa_rline:	errno = 0;
		fp = P4_POP(ctx->ds).v;
		x = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		(void) fgets(w.s, (int) x.n, fp);
		x.z = strlen(w.s);
		if (0 < x.z && w.s[x.z-1] == '\n') {
			x.z -= 0 < --x.z && w.s[x.z-1] == '\r';
		}
		P4_PUSH(ctx->ds, x.z);
		P4_PUSH(ctx->ds, P4_BOOL(errno == 0));
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

_fa_flush:	errno = 0;
		(void) fflush(P4_TOP(ctx->ds).v);
		P4_TOP(ctx->ds).n = errno;
		NEXT;

_fa_fsize:	errno = 0;
		(void) fstat(fileno(P4_TOP(ctx->ds).v), &sb);
		P4_TOP(ctx->ds).n = sb.st_size;
		P4_PUSH(ctx->ds, (P4_Uint) 0);
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

_fa_write:	errno = 0;
		fp = P4_POP(ctx->ds).v;
		x = P4_POP(ctx->ds);
		w = P4_TOP(ctx->ds);
		w.u = fwrite(w.s, sizeof (*w.s), x.u, fp);
		P4_TOP(ctx->ds).n = errno;
		NEXT;

_fa_include:	x.n = p4EvalFp(ctx, P4_TOP(ctx->ds).v);
		P4_TOP(ctx->ds) = x;
		NEXT;

_fa_seek:	errno = 0;
		fp = P4_POP(ctx->ds).v;
		P4_DROP(ctx->ds, 1);
		x = P4_POP(ctx->ds);
		(void) fseek(fp, x.n, SEEK_SET);
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;

_fa_tell:	errno = 0;
		x.u = ftell(P4_TOP(ctx->ds).v);
		P4_TOP(ctx->ds) = x;
		P4_PUSH(ctx->ds, (P4_Uint) 0);
		P4_PUSH(ctx->ds, (P4_Int) errno);
		NEXT;
	}
#endif
#ifdef HAVE_SEE
		// ( xt -- )
_seext:		word = P4_POP(ctx->ds).xt;
		/* If xt is bogus address, then possible SIGSEGV here.
		 * Test: 123 _seext
		 */
		if (word->code == &&_enter) {
			/* Test most words, eg. SEE IF SEE ['] SEE \ */
			(void) printf(
				word->name.length == 0 ? ":NONAME " : ": %.*s ",
				(int) word->name.length, word->name.string
			);
			for (w.p = word->data; w.p->xt != &w_semi; w.p++) {
				x = *w.p;
				if (x.w->code == &&_lit) {
					x = *++w.p;
					if (x.w != NULL && words <= x.w
					&& p4IsPrintable(x.w->name.string, x.w->name.length)) {
						(void) printf("[ ' %.*s ] LITERAL ", (int) x.w->name.length, x.w->name.string);
					} else {
						(void) printf("[ "P4_INT_FMT" ] LITERAL ", x.n);
					}
				} else if (strncmp(x.w->name.string, "_slit", STRLEN("_slit")) == 0) {
					(void) printf("S\" %s\" ", (char *) &w.p[2]);
					w.u += P4_CELL + P4_CELL_ALIGN(w.p[1].u + 1);
				} else {
					(void) printf("%.*s ", (int) x.w->name.length, x.w->name.string);
					if ((x.w->code == &&_branch || x.w->code == &&_branchz || x.w->code == &&_call)) {
						/* If a branch/call is postponed then it is a control
						 * structure definition so what follows is an xt, not
						 * a relative distance.
						 */
						(void) printf("[ "P4_INT_FMT" CELLS , ] ", (*++w.p).n / P4_CELL);
					}
				}
			}
			(void) printf(";%s%s\r\n",
				P4_WORD_IS_IMM(word) ? " IMMEDIATE" : "",
				P4_WORD_IS_COMPILE(word) ? " compile-only" : ""
			);
		} else if (word->code == &&_do_does) {
			/* Test: 123 VALUE x SEE x */
			/*** If we change (again) how a P4_Word and data are
			 *** stored in memory, then most likely need to fix
			 *** this and DOES>.
			 ***/
			/* Dump word's data. data[0] = pointer to DOES>,
			 * data[n-1] = xt of defining word, see _does.
			 * data[1..n-2] is the actual data.
			 */
			w.s = (P4_Char *)word->data + word->ndata - sizeof (*word->data);
			for (x.p = word->data + 1; x.p < w.p; x.p++) {
				(void) printf(P4_HEX_FMT" ", x.p->u);
			}
			/* Print the defining word, eg. VALUE, and new word name. */
			w = *w.p;
			(void) printf(
				"%.*s %.*s\r\n",
				(int) w.w->name.length, w.w->name.string,
				(int) word->name.length, word->name.string
			);
		} else if (word->code == &&_data_field) {
			/* Test: CREATE y 1 , 2 , 3 , SEE y */
			(void) printf(
				"CREATE %.*s ( size %zu )\r\n",
				(int) word->name.length, word->name.string, word->ndata - P4_CELL
			);
			p4MemDump(stdout, (P4_Char *)(word->data + 1), word->ndata - P4_CELL);
		} else {
			/* Builtins or libraries.  Test: SEE LIT SEE CREATE */
			(void) printf(
				": %.*s ( code address "P4_PTR_FMT" ) ;\r\n",
				(int) word->name.length, word->name.string,
				word->code
			);
		}
		NEXT;
#endif

#ifdef HAVE_MATH_H
# if defined(FLT_EVAL_METHOD) == 0
#  define MAX_FLOAT	((float) FLT_MAX)
#  define MIN_FLOAT	((float) FLT_MIN)
# else
#  define MAX_FLOAT	((double) DBL_MAX)
#  define MIN_FLOAT	((double) DBL_MIN)
# endif
		// (F: -- f )
_max_float:	P4_PUSH(ctx->P4_FLOAT_STACK, (P4_Float) MAX_FLOAT);
		NEXT;

//		// (F: -- f )
//_min_float:	P4_PUSH(ctx->P4_FLOAT_STACK, MIN_FLOAT);
//		NEXT;

		// ( -- aaddr n s )
_fs:		w.n = P4_LENGTH(ctx->fs);
		P4_PUSH(ctx->ds, ctx->fs.base);
		P4_PUSH(ctx->ds, w);
		P4_PUSH(ctx->ds, ctx->fs.size);
		NEXT;

		// ( aaddr -- ) (F: -- f )
_f_fetch:	w = P4_POP(ctx->ds);
		P4_PUSH(ctx->P4_FLOAT_STACK, *w.p);
		NEXT;

		// ( aaddr -- ) (F: f -- )
_f_store:	w = P4_POP(ctx->ds);
		x = P4_POP(ctx->P4_FLOAT_STACK);
		*w.p = x;
		NEXT;

		// (x -- )(R: -- x )
_fs_to_rs:	w = P4_POP(ctx->P4_FLOAT_STACK);
		P4_PUSH(ctx->rs, w);
		NEXT;

		// (R: x -- )
_rs_to_fs:	w = P4_POP(ctx->rs);
		P4_PUSH(ctx->P4_FLOAT_STACK, w);
		NEXT;
	{
		// ( caddr u -- F:f bool )
		P4_Cell f;
		unsigned char *stop;
_to_float:	errno = 0;
		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		f.f = strtod(x.s, (char **)&stop);
		P4_PUSH(ctx->ds, (P4_Uint) P4_BOOL(errno == 0 && stop - x.s == w.u));
		if (P4_TOP(ctx->ds).n == P4_TRUE) {
			P4_TOP(ctx->P4_FLOAT_STACK).f = f.f;
		}
	}

		// (F: f -- )
_f_dot:		if (ctx->radix != 10) {
			THROW(P4_THROW_BAD_BASE);
		}
		w = P4_POP(ctx->P4_FLOAT_STACK);
		(void) printf("%.*lF ", (int) ctx->precision, w.f);
		NEXT;

		// (F: f -- )
_f_sdot:	if (ctx->radix != 10) {
			THROW(P4_THROW_BAD_BASE);
		}
		w = P4_POP(ctx->P4_FLOAT_STACK);
		(void) printf("%.*lE ", (int) ctx->precision, w.f);
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

		// (F: f -- )( -- bool_)
_f_eq0:		w = P4_POP(ctx->P4_FLOAT_STACK);
		P4_PUSH(ctx->ds, P4_BOOL(w.f == 0.0));
		NEXT;

		// (F: f -- )( -- bool_)
_f_lt0:		w = P4_POP(ctx->P4_FLOAT_STACK);
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
_f_sinh:		w = P4_TOP(ctx->P4_FLOAT_STACK);
		P4_TOP(ctx->P4_FLOAT_STACK).f = sinh(w.f);
		NEXT;

		// (F: f1 -- f2 )
_f_tanh:		w = P4_TOP(ctx->P4_FLOAT_STACK);
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
		// (S: n -- )(F: -- f )
		// : S>F S>D D>F ;
_s_to_f:	w = P4_POP(ctx->ds);
		P4_PUSH(ctx->P4_FLOAT_STACK, (P4_Float) w.n);
		NEXT;

		// (S: -- n)(F: f -- )
		// : F>S F>D D>S ;
_f_to_s:	w = P4_POP(ctx->P4_FLOAT_STACK);
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
p4EvalFp(P4_Ctx *ctx, FILE *fp)
{
	int rc;

	/* Do not save STATE, see A.6.1.2250 STATE. */
	P4_INPUT_PUSH(&ctx->input);
	ctx->input.fp = fp;
	p4ResetInput(ctx);
	rc = p4Repl(ctx);
	P4_INPUT_POP(&ctx->input);

	return rc;
}

int
p4EvalFile(P4_Ctx *ctx, const char *file)
{
	FILE *fp;
	int rc = P4_THROW_EIO;

	if ((fp = fopen(file, "r")) != NULL) {
		rc = p4EvalFp(ctx, fp);
		(void) fclose(fp);
	}

	return rc;
}

int
p4EvalString(P4_Ctx *ctx, const P4_Char *str, size_t len)
{
	int rc;

	/* Do not save STATE, see A.6.1.2250 STATE. */
	P4_INPUT_PUSH(&ctx->input);

	ctx->input.fp = (FILE *) -1;
	ctx->input.buffer = (P4_Char *) str;
	ctx->input.length = len;
	ctx->input.offset = 0;

	rc = p4Repl(ctx);

	P4_INPUT_POP(&ctx->input);

	return rc;
}

#ifdef TEST
/***********************************************************************
 *** Main
 ***********************************************************************/

static const char usage[] =
"usage: post4 [-V][-b file][-c file][-d size][-f size][-i file][-m size]\r\n"
"             [-r size][script [args ...]]\r\n"
"\r\n"
"-b file\t\topen a block file\r\n"
"-c file\t\tword definition file; default " P4_CORE_FILE " from $POST4_PATH\r\n"
"-d size\t\tdata stack size in cells; default " QUOTE(P4_STACK_SIZE) "\r\n"
"-f size\t\tfloat stack size; default " QUOTE(P4_FLOAT_STACK_SIZE) "\r\n"
"-i file\t\tinclude file; can be repeated; searches $POST4_PATH\r\n"
"-m size\t\tdata space memory in KB; default " QUOTE(P4_MEM_SIZE) "\r\n"
"-r size\t\treturn stack size in cells; default " QUOTE(P4_STACK_SIZE) "\r\n"
"-V\t\tbuild and version information\r\n\r\n"
"If script is \"-\", read it from standard input.\r\n"
;

static P4_Options options = {
	.ds_size = P4_STACK_SIZE,
	.rs_size = P4_STACK_SIZE,
	.fs_size = P4_FLOAT_STACK_SIZE,
	.mem_size = P4_MEM_SIZE,
	.core_file = P4_CORE_FILE,
	.block_file = NULL,
};

static const char p4_build_info[] =
	P4_NAME "/" P4_VERSION "  " P4_COPYRIGHT "\r\n\r\n"
	"BUILT=\"" P4_BUILT "\"\r\n"
	"CFLAGS=\"" P4_CFLAGS "\"\r\n"
	"LDFLAGS=\"" P4_LDFLAGS "\"\r\n"
	"LIBS=\"" P4_LIBS "\"\r\n"
	"POST4_PATH=\"" P4_CORE_PATH "\"\r\n"
;

static void
sig_int(int signum)
{
	switch (signum) {
	case SIGINT: signum = P4_THROW_SIGINT; break;
	case SIGFPE: signum = P4_THROW_SIGFPE; break;
	case SIGSEGV: signum = P4_THROW_SIGSEGV; break;
	}
	LONGJMP(signal_ctx->on_throw, signum);
}

int
main(int argc, char **argv)
{
	int ch, rc;
	P4_Ctx *ctx;

	p4Init();

	signal(SIGINT, sig_int);
	signal(SIGFPE, sig_int);
	signal(SIGSEGV, sig_int);

	while ((ch = getopt(argc, argv, "b:c:d:f:i:m:r:V")) != -1) {
		switch (ch) {
		case 'b':
			options.block_file = optarg;
			break;
		case 'c':
			options.core_file = optarg;
			break;
		case 'd':
			options.ds_size = strtol(optarg, NULL, 10);
			break;
		case 'f':
#ifdef HAVE_MATH_H
			options.fs_size = strtol(optarg, NULL, 10);
#else
			(void) warnx("float support disabled");
#endif
			break;
		case 'i':
			// Ignore for now.
			break;
		case 'm':
			options.mem_size = strtol(optarg, NULL, 10);
			break;
		case 'r':
			options.rs_size = strtol(optarg, NULL, 10);
			break;
		case 'V':
			(void) printf(
				"%s\r\nsizeof char=%zu short=%zu int=%zu long=%zu size_t=%zu "
				"intptr_t=%zu float=%zu double=%zu\r\nvoid *=%zu long long=%zu long double=%zu\r\n",
				p4_build_info,
				sizeof (char), sizeof (short), sizeof (int), sizeof (long),
				sizeof (size_t), sizeof (intptr_t), sizeof (float), sizeof (double),
				sizeof (void *), sizeof (long long), sizeof (long double)
			);
			return EXIT_SUCCESS;
		default:
			(void)fprintf(stderr, usage);
			return 2;
		}
	}

	options.argc = argc - optind;
	options.argv = argv + optind;

	if ((ctx = p4Create(&options)) == NULL) {
		return EXIT_FAILURE;
	}
	(void) p4HookInit(ctx);

	optind = 1;
	while ((ch = getopt(argc, argv, "b:c:d:f:i:m:r:V")) != -1) {
		if (ch == 'i' && (rc = p4EvalFile(ctx, optarg)) != P4_THROW_OK) {
			err(EXIT_FAILURE, "%s", optarg);
		}
	}

	if (argc <= optind || (argv[optind][0] == '-' && argv[optind][1] == '\0')) {
		while ((rc = p4Repl(ctx)) != P4_THROW_OK) {
			; // Remain in the REPL until EOF or BYE.
		}
	} else if (optind < argc && (rc = p4EvalFile(ctx, argv[optind]))) {
		err(EXIT_FAILURE, "%s", argv[optind]);
	}

	p4Free(ctx);

	return rc;
}

#endif /* TEST */
