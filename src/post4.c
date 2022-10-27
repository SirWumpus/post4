/*
 * post4.c
 *
 * Copyright 2007, 2022 by Anthony Howe. All rights reserved.
 */

#include "post4.h"

/***********************************************************************
 *** Globals
 ***********************************************************************/

static void *p4_program_end;
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

struct winsize window = {
	.ws_row = 24,
	.ws_col = 80,
};

#define P4_INTERACTIVE(ctx)	(ctx->state == P4_STATE_INTERPRET && is_tty && P4_INPUT_IS_TERM(ctx->input))

/*
 * See P4_THROW_*
 */
static const char *p4_exceptions[] = {
	"zero",
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

static int p4Repl(P4_Ctx *ctx);

/***********************************************************************
 *** Context
 ***********************************************************************/

static void
sig_int(int signum)
{
	if (signal_ctx != NULL) {
		switch (signum) {
		case SIGINT: signum = P4_THROW_SIGINT; break;
		case SIGFPE: signum = P4_THROW_SIGFPE; break;
		case SIGSEGV: signum = P4_THROW_SIGSEGV; break;
		}
		LONGJMP(signal_ctx->on_throw, signum);
	}
	abort();
}

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

void
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

	signal(SIGINT, sig_int);
	signal(SIGFPE, sig_int);
	signal(SIGWINCH, sig_winch);
#ifdef NDEBUG
/* When debugging with gdb can be helpful to see actual error
 * location; otherwise catch it.
 */
	signal(SIGSEGV, sig_int);
#endif
	is_tty = isatty(fileno(stdin));
#ifdef ASSERT_LINE_BUFFERING
	setvbuf(stdout, NULL, _IOLBF, 0);
	setvbuf(stderr, NULL, _IOLBF, 0);
#endif

	/* Remember the split between the program and its static
	 * data and data dynamically allocated later.  This can be
	 * used to distinguish between built-in and loaded words.
	 */
	p4_program_end = sbrk(0);

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
		(void) fprintf(stderr, "cannot find file: %s\n", file);
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
p4StrNum(P4_String str, P4_Uint base, P4_Int *out)
{
	P4_Int value;
	int negate = 0;
	int offset = 0;

	*out = 0;

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
		base = 8;
		offset++;
		if (2 < str.length && str.string[1] == 'x') {
			base = 16;
			offset++;
		}
		break;
	case '\'':	/* 'c' and '\x' escaped characters */
		if (str.length == 3 && str.string[2] == '\'') {
			*out = (P4_Int) str.string[1];
			return str.length;
		}
		/* Extension C style backslash literals */
		if (str.length == 4 && str.string[1] == '\\' && str.string[3] == '\'') {
			*out = (P4_Int) p4CharLiteral(str.string[2]);
			return str.length;
		}
		/* Nothing parsed. */
		return 0;
	}

	if (offset < str.length && str.string[offset] == '-') {
		negate = 1;
		offset++;
	}

	for (value = 0; offset < str.length; offset++) {
		int digit = p4Base36(str.string[offset]);
		if (base <= digit) {
			break;
		}
		value = value * base + digit;
	}
	if (negate) {
		value = -value;
	}

	*out = value;
	return offset;
}

/***********************************************************************
 *** Utility
 ***********************************************************************/

P4_String
p4Parse(P4_Input *input, P4_Uint delim, P4_Uint escape)
{
	P4_Int ch;
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
			(void) fputc('\n', fp);
		}
	}
	if ((count & 3) != 0) {
		(void) fputc('\n', fp);
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
			(void) fprintf(fp, P4_PTR_FMT" ", (long)addr);
			s = addr;
		}
		(void) fprintf(fp, " %.2x", (unsigned char) *addr);
		if ((++count & 0x3) == 0) {
			(void) fputc(' ', fp);
		}
		if ((count & 0xF) == 0) {
			fputc(' ', fp);
			for ( ; s <= addr; s++) {
				(void) fputc(isprint(*s) ? *s : '.', fp);
			}
			(void) fprintf(fp, "\r\n");
		}
	}
	if ((count & 0xF) != 0) {
		do {
			(void) fputs("   ", fp);
			if ((++count & 0x3) == 0) {
				(void) fputc(' ', fp);
			}
		} while ((count & 0xF) != 0);
		(void) fputc(' ', fp);
		for ( ; s < addr; s++) {
			(void) fputc(isprint(*s) ? *s : '.', fp);
		}
		(void) fprintf(fp, "\r\n");
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
	P4_Uint carry = ((al_bl >> P4_HALF_SHIFT) + (P4_Uint_Half) al_bh + (P4_Uint_Half) ah_bl) >> P4_HALF_SHIFT;
	*c1 = ah_bh + (ah_bl >> P4_HALF_SHIFT) + (al_bh >> P4_HALF_SHIFT) + carry;
	*c0 = (ah_bl << P4_HALF_SHIFT) + (al_bh << P4_HALF_SHIFT) + al_bl;
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
	int sign = ((a < 0) ^ (b < 0)) ? -1 : 1;
	p4Mulu(a < 0 ? -a : a, b < 0 ? -b : b, c0, c1);
	if (sign < 0) {
		P4_Int cc0 = ~*c0;
		*c0 = cc0 + 1;
		*c1 = ~*c1 + (*c0 < cc0);
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
	qhat = dend1 / (dsor >> P4_HALF_SHIFT);
	rhat = dend1 % (dsor >> P4_HALF_SHIFT);

	while (
		(qhat >> P4_HALF_SHIFT) != 0 ||
		// Both qhat and rhat are less 2**P4_HALF_SHIFT here!
		(qhat & P4_LOWER_MASK) * (dsor & P4_LOWER_MASK) >
		((rhat << P4_HALF_SHIFT) | (dend0 >> P4_HALF_SHIFT))
	) {
		qhat -= 1;
		rhat += (dsor >> P4_HALF_SHIFT);
		if ((rhat >> P4_HALF_SHIFT) != 0) {
			break;
		}
	}

	// Multiply and subtract.
	q1 = (qhat & P4_LOWER_MASK);
	uhat = ((dend1 << P4_HALF_SHIFT) | (dend0 >> P4_HALF_SHIFT)) - q1 * dsor;

	// Compute low quotient digit.
	qhat = uhat / (dsor >> P4_HALF_SHIFT);
	rhat = uhat % (dsor >> P4_HALF_SHIFT);

	while (
		(qhat >> P4_HALF_SHIFT) != 0 ||
		// Both qhat and rhat are less 2**P4_HALF_SHIFT here!
		(qhat & P4_LOWER_MASK) * (dsor & P4_LOWER_MASK) >
		((rhat << P4_HALF_SHIFT) | (dend0 >> P4_HALF_SHIFT))
	) {
		qhat -= 1;
		rhat += (dsor >> P4_HALF_SHIFT);
		if ((rhat >> P4_HALF_SHIFT) != 0) {
			break;
		}
	}

	q0 = (qhat & P4_LOWER_MASK);

	if (rem != NULL) {
		*rem =  (((uhat << P4_HALF_SHIFT) | (dend0 & P4_LOWER_MASK)) - q0 * dsor) >> shift;
	}

	return ((P4_Uint) q1 << P4_HALF_SHIFT) | q0;
}

P4_Int
p4Divs(P4_Int dend0, P4_Int dend1, P4_Int dsor, P4_Int *rem)
{
	P4_Int quot;
	int neg_rem = (dend1 < 0);
	int sign = (neg_rem ^ (dsor < 0)) ? -1 : 1;
	if (dend1 < 0) {
		P4_Int d0 = ~dend0;
		dend0 = d0 + 1;
		dend1 = ~dend1 + (dend0 < d0);
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
#ifdef HAVE_TCSETATTR
	/* For a terminal restore original line input and echo settings. */
	if (is_tty && tty_mode != &tty_saved) {
		(void) tcsetattr(tty_fd, TCSADRAIN, &tty_saved);
		tty_mode = &tty_saved;
	}
#endif
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
p4BlockGrow(P4_Int fd, P4_Uint block)
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
p4BlockRead(P4_Int fd, P4_Uint blk_num, P4_Block *block)
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
p4BlockWrite(P4_Int fd, P4_Block *block)
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

P4_Int
p4BlockOpen(const char *file)
{
	int cwd;
	P4_Int fd = -1;

	if ((cwd = open(".", O_RDONLY)) < 0) {
		goto error0;
	}
	if ((fd = open(file, O_CREAT|O_RDWR, S_IRWXU|S_IRWXG|S_IRWXO)) < 0 || flock(fd, LOCK_EX|LOCK_NB)) {
		if (errno == EAGAIN) {
			warn("%s already in use", file);
			goto error1;
		}
		const char *home = getenv("HOME");
		if (home == NULL || chdir(home)) {
			goto error1;
		}
		if ((fd = open(file, O_CREAT|O_RDWR, S_IRWXU|S_IRWXG|S_IRWXO)) < 0 || flock(fd, LOCK_EX|LOCK_NB)) {
			if (errno == EAGAIN) {
				warn("%s already in use", file);
			}
			goto error1;
		}
	}
error1:
	(void) fchdir(cwd);
	(void) close(cwd);
error0:
	return fd;
}

int
p4BlockClose(P4_Int fd, P4_Block *block)
{
	if (block->state == P4_BLOCK_DIRTY && p4BlockWrite(fd, block)) {
		return -1;
	}
	block->state = P4_BLOCK_FREE;
	block->number = 0;
	return close(fd);
}

void
p4BlockBuffer(P4_Ctx *ctx, P4_Uint blk_num, int with_read)
{
	if (ctx->block_fd <= 0) {
		LONGJMP(ctx->on_throw, P4_THROW_EIO);
	}
	if (blk_num == 0) {
		LONGJMP(ctx->on_throw, P4_THROW_BLOCK_BAD);
	}
	if (blk_num == ctx->block.number) {
		return;
	}
	/* Current there is no block buffer assignment strategy beyond
	 * a single buffer per context.  Might add one day.
	 */
	if (ctx->block.state == P4_BLOCK_DIRTY && p4BlockWrite(ctx->block_fd, &ctx->block)) {
		LONGJMP(ctx->on_throw, P4_THROW_BLOCK_WR);
	}
	if (with_read && p4BlockRead(ctx->block_fd, blk_num, &ctx->block)) {
		LONGJMP(ctx->on_throw, P4_THROW_BLOCK_RD);
	}
	ctx->block.state = P4_BLOCK_CLEAN;
	ctx->block.number = blk_num;
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

P4_Word *
p4WordCreate(P4_Ctx *ctx, const char *name, size_t length, P4_Code code)
{
	P4_Word *word;

	if ((word = malloc(sizeof (*word))) == NULL) {
		goto error0;
	}
	word->mdata = sizeof (*word->data);
	word->ndata = 0;
	word->bits = 0;

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

P4_Word *
p4WordAllot(P4_Ctx *ctx, P4_Word *word, P4_Int n)
{
	/* Always allocate in cell units... */
	size_t size = P4_CELL_ALIGN(word->ndata + n);

	/* Check for size overflow. */
	if (word->mdata <= size) {
		if ((word = realloc(word, sizeof (*word) + size)) == NULL) {
			LONGJMP(ctx->on_throw, P4_THROW_RESIZE);
		}
		word->mdata = size;
	}

	/* ... but house keeping done in address units. */
	word->ndata += n;

	return word;
}

P4_Word *
p4WordAppend(P4_Ctx *ctx, P4_Word *word, P4_Cell data)
{
	P4_Size index = P4_CELL_ALIGN(word->ndata);
	word = p4WordAllot(ctx, word, sizeof (P4_Cell));
	word->data[index / sizeof (P4_Cell)] = data;

	return word;
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
		for (word = ctx->words; p4_program_end <= (void *)word; word = prev) {
			prev = word->prev;
			p4WordFree(word);
		}
		(void) p4BlockClose(ctx->block_fd, &ctx->block);
		free(ctx->ds.base);
		free(ctx->rs.base);
		free(ctx);
	}
}

static void
p4ResetInput(P4_Ctx *ctx)
{
	ctx->input.size = sizeof (ctx->tty);
	ctx->input.buffer = ctx->tty;
	ctx->input.unget = EOF;
	ctx->input.length = 0;
	ctx->input.offset = 0;
	ctx->input.blk = 0;
}

static void
p4SetInput(P4_Ctx *ctx, FILE *fp)
{
	ctx->input.fp = fp;
	p4ResetInput(ctx);
}

P4_Ctx *
p4Create(P4_Options *opts)
{
	P4_Ctx *ctx;

	if ((ctx = calloc(1, sizeof (*ctx))) == NULL) {
		goto error0;
	}
	ctx->radix = 10;
	p4SetInput(ctx, stdin);
	ctx->argc = opts->argc;
	ctx->argv = opts->argv;
	ctx->state = P4_STATE_INTERPRET;

#ifdef HAVE_MATH_H
	if ((ctx->fs.base = malloc((opts->fs_size + 1) * sizeof (*ctx->fs.base))) == NULL) {
		goto error0;
	}
	ctx->fs.base[opts->fs_size].u = P4_SENTINEL;
	ctx->fs.size = opts->fs_size;
	P4_RESET(ctx->fs);
#endif

	if ((ctx->rs.base = malloc((opts->rs_size + 1) * sizeof (*ctx->rs.base))) == NULL) {
		goto error0;
	}
	ctx->rs.base[opts->rs_size].u = P4_SENTINEL;
	ctx->rs.size = opts->rs_size;
	P4_RESET(ctx->rs);

	if ((ctx->ds.base = malloc((opts->ds_size + 1) * sizeof (*ctx->ds.base))) == NULL) {
		goto error0;
	}
	ctx->ds.base[opts->ds_size].u = P4_SENTINEL;
	ctx->ds.size = opts->ds_size;
	P4_RESET(ctx->ds);

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

static void
p4Bp(P4_Ctx *ctx)
{
	int has_nl = ctx->input.buffer[ctx->input.length-1] == '\n';
	(void) printf(
		">> %.*s\r\n>> %*c\r\n",
		(int)ctx->input.length - has_nl, ctx->input.buffer,
		(int)ctx->input.offset, '^'
	);
}

static void
p4StackCanPopPush(P4_Ctx *ctx, P4_Stack *stack, int pop, int push)
{
	int length = (stack->top + 1 - stack->base);

	/* Stack has enough data to pop? */
	if (length < pop) {
		p4Bp(ctx);
		LONGJMP(ctx->on_throw, stack == &ctx->ds ? P4_THROW_DS_UNDER : P4_THROW_RS_UNDER);
	}
	/* Stack has enough space to push data? */
	if (stack->size < length + push - pop) {
		p4Bp(ctx);
		LONGJMP(ctx->on_throw, stack == &ctx->ds ? P4_THROW_DS_OVER : P4_THROW_RS_OVER);
	}
}

/* Display exception message when there is no catch-frame.
 *
 * @param ctx
 *	The Forth machine context.
 *
 * @param code
 *	A THROW code.
 *
 * @See
 *	3.4.4 Possible actions on an ambiguous condition.
 */
static int
p4Exception(P4_Ctx *ctx, int code)
{
	switch (code) {
	case P4_THROW_ABORT_MSG:
		/* Displays its own message. */
	case P4_THROW_ABORT:
	case P4_THROW_QUIT:
		/* Historically no message, simply return to REPL. */
	case P4_THROW_OK:
		return code;
	}
	(void) printf("%d thrown: %s", code, P4_THROW_future <= code && code < 0 ? p4_exceptions[-code] : "?");
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
		ctx->state = P4_STATE_INTERPRET;
		ctx->words = word->prev;
		p4WordFree(word);
	}
	(void) printf("\r\n");
	(void) fflush(stdout);
	return code;
}

static int
p4Repl(P4_Ctx *ctx)
{
	int rc;
	P4_Char *cstr;
	P4_Word *word;
	P4_String str;
	P4_Cell w, x, *ip;

	/* Wrap code pointers for Indirect Threading. */
	static P4_Word w_lit = P4_WORD("LIT", &&_lit, 0);
	static P4_Word w_exit = P4_WORD("EXIT", &&_exit, 0);
	static P4_Word w_repl = P4_WORD("_repl", &&_repl, 0);

	/* When the REPL executes a word, it puts the XT of the word here
	 * and starts the machine with the IP pointed to exec[].  When the
	 * word completes the next XT (w_repl) transitions from threaded
	 * code back into the C driven REPL.
	 */
	static P4_Cell exec[] = { { 0 }, { .w = &w_repl } };

	static P4_Word words[] = {
		/* Constants. */
		P4_WORD("/hold",		&&_pic_size,	0),	// p4
		P4_WORD("/pad",			&&_pad_size,	0),	// p4
		P4_WORD("address-unit-bits",	&&_char_bit,	0),	// p4
		P4_WORD("floored",		&&_floored,	0),	// p4
		P4_WORD("return-stack-cells",	&&_rs_size,	0),	// p4
		P4_WORD("stack-cells",		&&_ds_size,	0),	// p4

		/* Internal support. */
		P4_WORD("_bp",		&&_bp,		P4_BIT_IMM),	// p4
		P4_WORD("_branch",	&&_branch,	P4_BIT_COMPILE), // p4
		P4_WORD("_branchz",	&&_branchz,	P4_BIT_COMPILE), // p4
		P4_WORD("_call",	&&_call,	P4_BIT_COMPILE), // p4
		P4_WORD("_ds",		&&_ds,		0),		// p4
		P4_WORD("_dsp@",	&&_dsp_get,	0),		// p4
		P4_WORD("_dsp!",	&&_dsp_put,	0),		// p4
		P4_WORD("LIT",		&&_lit,		0),		// historic
		P4_WORD("_longjmp",	&&_longjmp,	0),		// p4
		P4_WORD("_rs",		&&_rs,		0),		// p4
		P4_WORD("_rsp@",	&&_rsp_get,	0),		// p4
		P4_WORD("_rsp!",	&&_rsp_put,	0),		// p4
		P4_WORD("_stack_dump",	&&_stack_dump,	0),		// p4
		P4_WORD("_stdin",	&&_stdin,	0),		// p4
		P4_WORD("_window",	&&_window,	0),		// p4

		/* Compiling Words */
		P4_WORD("compile-only",		&&_compile_only,	P4_BIT_IMM),	// p4
		P4_WORD("compile-only?",	&&_is_compile,		P4_BIT_COMPILE),// p4
		P4_WORD("'",		&&_tick,	0),
		P4_WORD(":NONAME",	&&_noname,	0),
		P4_WORD(":",		&&_colon,	0),
		P4_WORD(";",		&&_semicolon,	P4_BIT_IMM|P4_BIT_COMPILE),
		P4_WORD(">BODY",	&&_body,	0),
		P4_WORD("CREATE",	&&_create,	0),
		P4_WORD("DOES>",	&&_does,	P4_BIT_COMPILE),
		P4_WORD("EVALUATE",	&&_evaluate,	0),
		P4_WORD("EXECUTE",	&&_execute,	0),
		P4_WORD("EXIT",		&&_exit,	P4_BIT_COMPILE),
		P4_WORD("IMMEDIATE",	&&_immediate,	P4_BIT_IMM),
		P4_WORD("immediate?",	&&_is_immediate, 0),		// p4
		P4_WORD("MARKER",	&&_marker,	0),
		P4_WORD("STATE",	&&_state,	0),

		/* Data Space - Alignment */
		P4_WORD("CELLS",	&&_cells,	0),
		P4_WORD("CHARS",	&&_chars,	0),
		P4_WORD("ALIGN",	&&_align,	0),
		P4_WORD("ALLOT",	&&_allot,	0),
		P4_WORD("HERE",		&&_here_addr,	0),
		P4_WORD(">here",	&&_here_offset,	0),		// p4
		P4_WORD("UNUSED",	&&_unused,	0),

		/* Data Space - Access */
		P4_WORD("_ctx",		&&_ctx,		0),		// p4
		P4_WORD("!",		&&_store,	0),
		P4_WORD(">R",		&&_to_rs,	0),		// allow interpret
		P4_WORD("@",		&&_fetch,	0),
		P4_WORD("C!",		&&_cstore,	0),
		P4_WORD("C@",		&&_cfetch,	0),
		P4_WORD("CS-PICK",	&&_pick,	P4_BIT_COMPILE), // C: on data stack
		P4_WORD("CS-ROLL",	&&_roll,	P4_BIT_COMPILE), // C: on data stack
		P4_WORD("DROP",		&&_drop,	0),
		P4_WORD("DUP",		&&_dup,		0),
		P4_WORD("MOVE",		&&_move,	0),
		P4_WORD("PICK",		&&_pick,	0),
		P4_WORD("R>",		&&_from_rs,	0),		// allow interpret
		P4_WORD("ROLL",		&&_roll,	0),
		P4_WORD("SWAP",		&&_swap,	0),
		P4_WORD("BASE",		&&_base,	0),

		/* Dynamic Memory */
		P4_WORD("ALLOCATE",	&&_allocate,	0),
		P4_WORD("FREE",		&&_free,	0),
		P4_WORD("RESIZE",	&&_resize,	0),

		/* Operators */
		P4_WORD("BASE",         &&_base,        0),
		P4_WORD("*",		&&_mul,		0),
		P4_WORD("+",		&&_add,		0),
		P4_WORD("-",		&&_sub,		0),
		P4_WORD("/",		&&_div,		0),
		P4_WORD("AND",		&&_and,		0),
		P4_WORD("FM/MOD",	&&_fm_div_mod,	0),
		P4_WORD("INVERT",	&&_not,		0),
		P4_WORD("LSHIFT",	&&_lshift,	0),
		P4_WORD("M*",		&&_mstar,	0),
		P4_WORD("MOD",		&&_mod,		0),
		P4_WORD("OR",		&&_or,		0),
		P4_WORD("RSHIFT",	&&_rshift,	0),
		P4_WORD("SM/REM",	&&_sm_div_rem,	0),
		P4_WORD("UM*",		&&_umstar,	0),
		P4_WORD("UM/MOD",	&&_um_div_mod,	0),
		P4_WORD("XOR",		&&_xor,		0),

		/* Comparisons */
		P4_WORD("0=",		&&_eq0,		0),
		P4_WORD("0<",		&&_lt0,		0),
		P4_WORD("U<",		&&_u_lt,	0),

		/* Tools*/
		P4_WORD("args",		&&_args,	0),		// p4
		P4_WORD("bye-code",	&&_bye_code,	0),		// p4
		P4_WORD("env",		&&_env,		0),		// p4
		P4_WORD("_SEEXT",	&&_seext,	0),		// p4

		/* I/O */
		P4_WORD(">IN",		&&_input_offset,0),
		P4_WORD("ACCEPT",	&&_accept,	0),
		P4_WORD("BLK",		&&_blk,		0),
		P4_WORD("BLOCK",	&&_block,	0),
		P4_WORD("blocks",	&&_blocks, 	0),		// p4
		P4_WORD("BUFFER",	&&_buffer,	0),
		P4_WORD("DUMP",		&&_dump,	0),
		P4_WORD("EMIT",		&&_emit,	0),
		P4_WORD("EMPTY-BUFFERS", &&_empty_buffers, 0),
		P4_WORD("epoch-seconds", &&_epoch_seconds, 0),		// p4
		P4_WORD("FIND-NAME",	&&_find_name,	0),
		P4_WORD("INCLUDED",	&&_included,	0),
		P4_WORD("KEY",		&&_key,		0),
		P4_WORD("KEY?",		&&_key_ready,	0),
		P4_WORD("MS",		&&_ms,		0),
		P4_WORD("_parse",	&&_parse,	0),		// p4
		P4_WORD("PARSE-NAME",	&&_parse_name,	0),
		P4_WORD("RESTORE-INPUT", &&_restore_input, 0),
		P4_WORD("REFILL",	&&_refill,	0),
		P4_WORD("SAVE-BUFFERS",	&&_save_buffers, 0),
		P4_WORD("SAVE-INPUT",	&&_save_input, 0),
		P4_WORD("SOURCE",	&&_source,	0),
		P4_WORD("SOURCE-ID",	&&_source_id,	0),
		P4_WORD("TIME&DATE",	&&_time_date,	0),
		P4_WORD("UPDATE",	&&_update,	0),

		P4_WORD(NULL,		NULL,		0),
	};

	if (p4_builtin_words == NULL) {
		/* Link up the base dictionary. */
		for (word = words; word->code != NULL; word++) {
			word[1].prev = word;
		}
		p4_builtin_words = word->prev;
		ctx->words = p4_builtin_words;
	}

#define NEXT	goto _next

	signal_ctx = ctx;
	SETJMP_PUSH(ctx->on_throw);
	if ((rc = SETJMP(ctx->on_throw)) != 0) {
		switch (rc) {
		case P4_THROW_ABORT:
		case P4_THROW_ABORT_MSG:
		case P4_THROW_DS_OVER:
		case P4_THROW_DS_UNDER:
			P4_RESET(ctx->ds);
			/*@fallthrough@*/

		case P4_THROW_QUIT:
		case P4_THROW_SIGSEGV:
		case P4_THROW_RS_OVER:
		case P4_THROW_RS_UNDER:
		case P4_THROW_UNDEFINED:	/* Retain data stack. */
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
				if (p4StrNum(str, ctx->radix, &x.n) != str.length) {
					/* Not a word, not a number. */
					(void) printf("\"%.*s\" ", (int)str.length, str.string);
					/* An earlier version treated most exceptions like ABORT
					 * which would empty the stack, which is annoying when
					 * interactive as this could upset work in progress.
					 *
					 * An undefined word does not need to behave like ABORT,
					 * so the stacks can remain untouched. See Forth 200x
					 * Draft 19.1 section 3.4 d.
					 */
					LONGJMP(ctx->on_throw, P4_THROW_UNDEFINED);
				}
				if (ctx->state == P4_STATE_COMPILE) {
					ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) &w_lit);
					ctx->words = p4WordAppend(ctx, ctx->words, x);
				} else {
					p4StackCanPopPush(ctx, &ctx->ds, 0, 1);
					P4_PUSH(ctx->ds, x);
				}
			} else if (ctx->state == P4_STATE_COMPILE && !P4_WORD_IS_IMM(word)) {
				ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) word);
			} else {
				// Setup XT of word found to execute.
				exec[0].w = word;
				ip = exec;
				NEXT;
			}
		}
		if (P4_INTERACTIVE(ctx)) {
			(void) fputs("ok ", stdout);
			(void) fflush(stdout);
		}
	} while (p4Refill(ctx, &ctx->input));

	if (P4_INTERACTIVE(ctx)) {
		(void) fputc('\n', stdout);
	}

setjmp_cleanup:
	SETJMP_POP(ctx->on_throw);
	return rc;

		// Indirect threading.
_next:		/* Check data stack bounds. */
		p4StackCanPopPush(ctx, &ctx->ds, 0, 0);
		w = *ip++;
		goto *w.xt->code;

		// ( xt -- )
_execute:	w = P4_POP(ctx->ds);
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
_bp:		p4Bp(ctx);
		NEXT;

		// ( -- )
_call:		w = *ip;
		p4StackCanPopPush(ctx, &ctx->rs, 0, 1);
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

		// ( n -- )
_longjmp:	w = P4_POP(ctx->ds);
		LONGJMP(ctx->on_throw, (int) w.n);

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

		// ( -- flag )
		// C11 defines symmetric division, not floored.
_floored:	P4_PUSH(ctx->ds, (P4_Int) P4_FALSE);
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

_colon:		if (ctx->state == P4_STATE_COMPILE) {
			LONGJMP(ctx->on_throw, P4_THROW_COMPILING);
		}
		str = p4ParseName(&ctx->input);
		goto _do_colon;

		// (C: -- colon) (R: -- ip)
		// Save the current tops so we can check for imbalance.
_do_colon:	w.p = ctx->ds.top;
		P4_PUSH(ctx->ds, w);
		x.p = ctx->rs.top;
		P4_PUSH(ctx->ds, x);
		ctx->state = P4_STATE_COMPILE;
		word = p4WordCreate(ctx, str.string, str.length, &&_enter);
		P4_WORD_SET_HIDDEN(word);
		NEXT;

		// (C: colon -- ) (R: ip -- )
_semicolon:	ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) &w_exit);
		x = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		if (w.p != ctx->ds.top || x.p != ctx->rs.top) {
			/* Control structure imbalance.  Did we match
			 * all the IF-THEN, BEGIN-REPEAT, etc.
			 */
			LONGJMP(ctx->on_throw, P4_THROW_BAD_CONTROL);
		}
		P4_WORD_CLEAR_HIDDEN(ctx->words);
		ctx->state = P4_STATE_INTERPRET;
		if (ctx->words->name.length == 0) {
			/* :NONAME leaves xt on stack. */
			P4_PUSH(ctx->ds, ctx->words);
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
		P4_WORD_SET_CREATED(word);
		NEXT;

		// DOES>
_does:		word = ctx->words;
		if (!P4_WORD_WAS_CREATED(word)) {
			LONGJMP(ctx->on_throw, P4_THROW_NOT_CREATED);
		}
		word->code = &&_do_does;
		// Append the IP of the words following DOES> of the defining
		// word after the data of the current word being defined.
		//
		//	: word CREATE ( store data) DOES> ( code words) ;
		//	                                  ^--- IP
		ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) ip);
		goto _exit;

		// ( -- aaddr)
_do_does:	P4_PUSH(ctx->ds, w.xt->data);
		// Remember who called us.
		p4StackCanPopPush(ctx, &ctx->rs, 0, 1);
		P4_PUSH(ctx->rs, ip);
		// Continue execution just after DOES> of the defining word.
		ip = w.xt->data[(w.w->ndata / P4_CELL) - 1].p;
		NEXT;

		// ( -- addr )
		// w contains xt loaded by _next or _execute.;
_data_field:	P4_PUSH(ctx->ds, w.xt->data);
		NEXT;

		// ( xt -- addr )
_body:		w = P4_TOP(ctx->ds);
		if (!P4_WORD_WAS_CREATED(w.w)) {
			LONGJMP(ctx->on_throw, P4_THROW_NOT_CREATED);
		}
		P4_TOP(ctx->ds).p = w.xt->data;
		NEXT;


		/*
		 * Compiling
		 */
		// ( -- xt )
_tick:		str = p4ParseName(&ctx->input);
		word = p4FindName(ctx, str.string, str.length);
		if (word == NULL) {
			p4Bp(ctx);
			LONGJMP(ctx->on_throw, P4_THROW_UNDEFINED);
		}
		P4_PUSH(ctx->ds, word);
		NEXT;

		// ( n -- )
_allot:		w = P4_POP(ctx->ds);
		ctx->words = p4WordAllot(ctx, ctx->words, w.n);
		NEXT;

		// ( -- )
_align:		ctx->words->ndata = P4_CELL_ALIGN(ctx->words->ndata);
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

		/*
		 * Numeric formatting
		 */
		// ( -- addr )
_base:		P4_PUSH(ctx->ds, (P4_Cell *) &ctx->radix);
		NEXT;

		// ( -- u )
_pad_size:	P4_PUSH(ctx->ds, (P4_Uint) P4_PAD_SIZE);
		NEXT;

		// ( -- u )
_pic_size:	P4_PUSH(ctx->ds, P4_PIC_SIZE);
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


		/*
		 * ... >HERE ...
		 *
		 * (S: -- u )
		 *
		 * Offset from the data-space start address for the word being compiled.
		 * Similar to the word HERE, except expressed as an offset.
		 *
		 * @note
		 *	During the compiliation of a word with C based implementations
		 *	data-space regions may be relocated when they are enlarged,
		 *	thus invalidating previous values of HERE.
		 *
		 * @standard p4
		 */
		// ( -- u )
_here_offset:	P4_PUSH(ctx->ds, ctx->words->ndata);
		NEXT;

		// ( -- addr )
_here_addr:	P4_PUSH(ctx->ds, (P4_Char *)ctx->words->data + ctx->words->ndata);
		NEXT;

		// ( -- u )
_unused:	w.u = ctx->words->mdata - ctx->words->ndata;
		P4_PUSH(ctx->ds, w);
		NEXT;

		/*
		 * Dynamic Memory
		 */
		// ( u -- aaddr ior )
_allocate:	w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).s = malloc(w.u);
		P4_PUSH(ctx->ds, (P4_Int)(w.s == NULL));
		NEXT;

		// ( aaddr -- ior )
_free:		w = P4_TOP(ctx->ds);
		free(w.s);
		P4_TOP(ctx->ds).n = 0;
		NEXT;

		// ( aaddr1 u -- aaddr2 ior )
_resize:	w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		w.s = realloc(x.s, w.u);
		P4_TOP(ctx->ds) = w.s == NULL ? x : w;
		P4_PUSH(ctx->ds, (P4_Int)(w.s == NULL));
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
		// 0 PICK == DUP, 1 PICK == OVER
_pick:		w = P4_POP(ctx->ds);
		/* Check stack depth. */
		p4StackCanPopPush(ctx, &ctx->ds, w.n+1, w.n+2);
		x = P4_PICK(ctx->ds, w.n);
		P4_PUSH(ctx->ds, x);
		NEXT;

		// ( x y -- y x )
		// 1 ROLL == SWAP
_swap:		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds) = w;
		P4_PUSH(ctx->ds, x);
		NEXT;

		// (x -- )(R: -- x )
_to_rs:		w = P4_POP(ctx->ds);
		p4StackCanPopPush(ctx, &ctx->rs, 0, 1);
		P4_PUSH(ctx->rs, w);
		NEXT;

		// (R: x -- )
_from_rs:	p4StackCanPopPush(ctx, &ctx->rs, 1, 0);
		w = P4_POP(ctx->rs);
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( xu xu-1 ... x0 u –– xu-1 ... x0 xu )
		// 0 ROLL == noop, 1 ROLL == SWAP, 2 ROLL == ROT
_roll:		w = P4_POP(ctx->ds);
		/* Check stack depth. */
		p4StackCanPopPush(ctx, &ctx->ds, w.n+1, 0);
		x = P4_PICK(ctx->ds, w.n);
		(void) memmove(ctx->ds.top - w.n, ctx->ds.top - w.n + 1, w.n * P4_CELL);
		P4_TOP(ctx->ds) = x;
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
			LONGJMP(ctx->on_throw, P4_THROW_DIV_ZERO);
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
			LONGJMP(ctx->on_throw, P4_THROW_DIV_ZERO);
		}
		w.n = p4Divs(x.n, w.n, d.n, &x.n);
		P4_TOP(ctx->ds).n = x.n;
		P4_PUSH(ctx->ds, w.n);
		NEXT;

		// ( d dsor -- mod quot )
		// Dividend Divisor Remainder Quotient
		//       10       7         3        1
		//      -10       7         4       -2
		//       10      -7        -4       -2
		//      -10      -7        -3        1
		//
_fm_div_mod:	d = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		if (d.n == 0) {
			LONGJMP(ctx->on_throw, P4_THROW_DIV_ZERO);
		}
		w.n = p4Divs(x.n, w.n, d.n, &x.n);
		if (w.n < 0 && x.n != 0) {
			x.n = -x.n + (d.n < 0 ? -1 : 1);
			w.n -= 1;
		}
		P4_TOP(ctx->ds).n = x.n;
		P4_PUSH(ctx->ds, w.n);
		NEXT;

		// ( ud dsor -- mod quot )
_um_div_mod:	d = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		if (d.n == 0) {
			LONGJMP(ctx->on_throw, P4_THROW_DIV_ZERO);
		}
		w.u = p4Divu(x.u, w.u, d.u, &x.u);
		P4_TOP(ctx->ds).u = x.u;
		P4_PUSH(ctx->ds, w.u);
		NEXT;
	}
		// ( n1 n2 -- n3 )
_mod:		w = P4_POP(ctx->ds);
		if (w.n == 0) {
			LONGJMP(ctx->on_throw, P4_THROW_DIV_ZERO);
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

		// ( -- )
_stdin:		p4SetInput(ctx, stdin);
		NEXT;

		// ( caddr +n1 -- +n2 )
_accept:	w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		w.u = p4Accept(&ctx->input, x.s, w.u);
		P4_TOP(ctx->ds) = w;
		NEXT;

		// ( -- xn ... x1 n )
_save_input:	w.n = sizeof (P4_Input) / P4_CELL;
		p4StackCanPopPush(ctx, &ctx->ds, 0, w.n);
		(void) memcpy(ctx->ds.top + 1, &ctx->input, sizeof (ctx->input));
		/* TODO save file position. */
		P4_DROP(ctx->ds, -w.n);
		P4_PUSH(ctx->ds, w.n);
		NEXT;

		// ( xn ... x1 n -- bool )
_restore_input:	w = P4_POP(ctx->ds);
		P4_DROP(ctx->ds, w.n);
		/* TODO restore file position if possible, true on failure. */
		(void) memcpy(&ctx->input, ctx->ds.top + 1, sizeof (ctx->input));
		P4_PUSH(ctx->ds, (P4_Int) P4_FALSE);
		NEXT;

		// ( -- flag)
_refill:	w.n = p4Refill(ctx, &ctx->input);
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( -- n )
_key:		(void) fflush(stdout);
		if (ctx->input.unget != EOF) {
			P4_PUSH(ctx->ds, ctx->input.unget);
			ctx->input.unget = EOF;
			NEXT;
		}
#ifdef HAVE_TCSETATTR
		if (is_tty && tty_mode != &tty_raw) {
			(void) tcsetattr(tty_fd, TCSANOW, &tty_raw);
			tty_mode = &tty_raw;
		}
#endif
		P4_PUSH(ctx->ds, p4ReadByte(tty_fd));
		NEXT;

		// ( -- flag )
_key_ready:	(void) fflush(stdout);
		if (ctx->input.unget == EOF) {
#ifdef HAVE_TCSETATTR
			if (is_tty && tty_mode != &tty_raw_nb) {
				(void) tcsetattr(tty_fd, TCSANOW, &tty_raw_nb);
				tty_mode = &tty_raw_nb;
			}
			ctx->input.unget = p4ReadByte(tty_fd);
#else
			if (p4SetNonBlocking(tty_fd, 1) == 0) {
				ctx->input.unget = p4ReadByte(tty_fd);
				(void) p4SetNonBlocking(tty_fd, 0);
			}
#endif
		}
		P4_PUSH(ctx->ds, (P4_Uint) P4_BOOL(ctx->input.unget != EOF));
		NEXT;

		// ( c -- )
_emit:		w = P4_POP(ctx->ds);
		(void) fputc(w.n, stdout);
		NEXT;

		// ( caddr u -- )
_included:	w = P4_POP(ctx->ds);
		x = P4_POP(ctx->ds);
		if ((cstr = strndup(x.s, w.u)) == NULL) {
			LONGJMP(ctx->on_throw, P4_THROW_ALLOCATE);
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
		p4BlockBuffer(ctx, w.u, 1);
		P4_TOP(ctx->ds).s = ctx->block.buffer;
		NEXT;

	{	// ( -- u )
		struct stat sb;
_blocks:	if (fstat(ctx->block_fd, &sb) != 0) {
			LONGJMP(ctx->on_throw, P4_THROW_EIO);
		}
		w.u = sb.st_size / P4_BLOCK_SIZE;
		P4_PUSH(ctx->ds, w);
		NEXT;
	}
		// ( u -- aaddr )
_buffer:	w = P4_TOP(ctx->ds);
		p4BlockBuffer(ctx, w.u, 0);
		P4_TOP(ctx->ds).s = ctx->block.buffer;
		NEXT;

		// ( -- )
_empty_buffers:	ctx->block.state = P4_BLOCK_FREE;

		// ( -- )
_save_buffers:	if (ctx->block.state == P4_BLOCK_DIRTY && p4BlockWrite(ctx->block_fd, &ctx->block)) {
			LONGJMP(ctx->on_throw, P4_THROW_BLOCK_WR);
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
		p4StackCanPopPush(ctx, &ctx->rs, 0, 6);
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
		// ( -- aaddr n )
_ds:		w.n = P4_LENGTH(ctx->ds);
		P4_PUSH(ctx->ds, ctx->ds.base);
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( -- u )
_ds_size:	P4_PUSH(ctx->ds, (P4_Size) ctx->ds.size);
		NEXT;

		// ( -- aaddr n )
_rs:		w.n = P4_LENGTH(ctx->rs);
		P4_PUSH(ctx->ds, ctx->rs.base);
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( -- u )
_rs_size:	P4_PUSH(ctx->ds, (P4_Size) ctx->rs.size);
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

		// ( xt -- )
_seext:		word = P4_POP(ctx->ds).xt;
		if (word == NULL) {
			(void) printf("\"%.*s\" ", (int)str.length, str.string);
			LONGJMP(ctx->on_throw, P4_THROW_UNDEFINED);
		}
		if ((void *) word < p4_program_end) {
			(void) printf(": %.*s ( builtin ) ;\r\n", (int)word->name.length, word->name.string);
			NEXT;
		}
		if (word->code == &&_enter) {
			(void) printf(
				word->name.length == 0 ? ":NONAME " : ": %.*s ",
				(int) word->name.length, word->name.string
			);
			for (w.p = word->data; w.p->xt != &w_exit; w.p++) {
				x = *w.p;
				if (x.w->code == &&_lit) {
					(void) printf("[ "P4_INT_FMT" ] LITERAL ", (*++w.p).n);
				} else if (strncmp(x.w->name.string, "_slit", STRLEN("_slit")) == 0) {
					(void) printf("S\" %s\" ", &w.p[2]);
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
			(void) printf(";%s", P4_WORD_IS_IMM(word) ? " IMMEDIATE" : "");
			(void) printf("%s", P4_WORD_IS_COMPILE(word) ? " compile-only" : "");
			(void) printf("\r\n");
		} else if (word->code == &&_do_does) {
			// Dump word's data.
			for (w.u = 0, x.u = 0; x.u < word->ndata - P4_CELL; x.u += P4_CELL, w.u++) {
				(void) printf(P4_HEX_FMT" ", word->data[w.u].u);
			}
			// Search back for code field with _enter.
			for (w.p = word->data[(word->ndata / P4_CELL) - 1].p; w.p->xt != &&_enter; w.p--) {
				;
			}
			// Find defining word.
			w.w = (P4_Word *)(w.s - offsetof(P4_Word, code));
			(void) printf(
				"%.*s %.*s\r\n",
				(int) w.w->name.length, w.w->name.string,
				(int) word->name.length, word->name.string
			);
		} else if (word->code == &&_data_field) {
			P4_Uint stop;
			(void) printf(
				"CREATE %.*s ( size %zu )\r\n",
				(int) word->name.length, word->name.string, word->ndata
			);
			p4MemDump(stdout, (P4_Char *)(word->data), word->ndata);
		} else {
			(void) printf(
				": %.*s ( unknown code ) 0x%p\r\n",
				(int)word->name.length, word->name.string, word->code
			);
		}
		NEXT;
}

int
p4Eval(P4_Ctx *ctx)
{
	int rc;

	while ((rc = p4Repl(ctx)) != P4_THROW_OK) {
		(void) p4Exception(ctx, rc);
		p4SetInput(ctx, stdin);
	}

	return rc;
}

int
p4EvalFile(P4_Ctx *ctx, const char *file)
{
	int rc = P4_THROW_EIO;

	P4_INPUT_PUSH(&ctx->input);

	if ((ctx->input.fp = fopen(file, "r")) != NULL) {
		p4ResetInput(ctx);
		ctx->state = P4_STATE_INTERPRET;
		rc = p4Exception(ctx, p4Repl(ctx));
		(void) fclose(ctx->input.fp);
	}

	P4_INPUT_POP(&ctx->input);

	return rc;
}

int
p4EvalString(P4_Ctx *ctx, P4_Char *str, size_t len)
{
	int rc;

	P4_INPUT_PUSH(&ctx->input);

	ctx->state = P4_STATE_INTERPRET;
	ctx->input.fp = (FILE *) -1;
	ctx->input.length = len;
	ctx->input.buffer = str;
	ctx->input.offset = 0;
	ctx->input.blk = 0;

	rc = p4Exception(ctx, p4Repl(ctx));

	P4_INPUT_POP(&ctx->input);

	return rc;
}

#ifdef TEST
/***********************************************************************
 *** Main
 ***********************************************************************/

static const char usage[] =
"usage: post4 [-V][-b file][-c file][-d size][-i file][-r size] [script [args ...]]\n"
"\n"
"-b file\t\tblock file; default " P4_BLOCK_FILE "\n"
"-c file\t\tword definition file; default " P4_CORE_FILE "\n"
"-d size\t\tdata stack size in cells; default " QUOTE(P4_STACK_SIZE) "\n"
"-i file\t\tinclude file; can be repeated\n"
"-r size\t\treturn stack size in cells; default " QUOTE(P4_STACK_SIZE) "\n"
"-V\t\tbuild and version information\n\n"
"If script is \"-\", read it from standard input."
"\n"
;

static P4_Options options = {
	.ds_size = P4_STACK_SIZE,
	.rs_size = P4_STACK_SIZE,
	.fs_size = P4_FLOAT_STACK_SIZE,
	.core_file = P4_CORE_FILE,
	.block_file = P4_BLOCK_FILE,
};

static const char p4_build_info[] =
	P4_NAME "/" P4_VERSION "  " P4_COPYRIGHT "\n\n"
	"BUILT=\"" P4_BUILT "\"\n"
	"CFLAGS=\"" P4_CFLAGS "\"\n"
	"LDFLAGS=\"" P4_LDFLAGS "\"\n"
	"LIBS=\"" P4_LIBS "\"\n"
	"POST4_PATH=\"" P4_CORE_PATH "\"\n"
;

int
main(int argc, char **argv)
{
	int ch, rc;
	P4_Ctx *ctx;

	p4Init();
	(void) atexit(p4Fini);

	while ((ch = getopt(argc, argv, "b:c:d:i:r:V")) != -1) {
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
		case 'i':
			// Ignore for now.
			break;
		case 'r':
			options.rs_size = strtol(optarg, NULL, 10);
			break;
		case 'V':
			(void) printf("%s", p4_build_info);
			return EXIT_SUCCESS;
		default:
			(void)fprintf(stderr, usage);
			return 2;
		}
	}

	options.argc = argc - optind;
	options.argv = argv + optind;

	if ((ctx = p4Create(&options)) == NULL) {
		err(EXIT_FAILURE, NULL);
	}

	optind = 1;
	while ((ch = getopt(argc, argv, "b:c:d:i:r:V")) != -1) {
		if (ch == 'i' && (rc = p4EvalFile(ctx, optarg)) != P4_THROW_OK) {
			err(EXIT_FAILURE, "%s", optarg);
		}
	}

	if (argc <= optind || (argv[optind][0] == '-' && argv[optind][1] == '\0')) {
		rc = p4Eval(ctx);
	} else if (optind < argc && (rc = p4EvalFile(ctx, argv[optind]))) {
		err(EXIT_FAILURE, "%s", argv[optind]);
	}

	p4Free(ctx);

	return rc;
}

#endif /* TEST */
