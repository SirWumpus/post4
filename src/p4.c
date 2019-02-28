/*
 * p4.c
 *
 * Copyright 2007, 2019 by Anthony Howe. All rights reserved.
 */

#include "p4.h"

/***********************************************************************
 *** Globals
 ***********************************************************************/

static const char p4_build_info[] =
	"BUILT=\"" P4_BUILT "\"\n"
	"CFLAGS=\"" P4_CFLAGS "\"\n"
	"LDFLAGS=\"" P4_LDFLAGS "\"\n"
	"LIBS=\"" P4_LIBS "\"\n"
;

static P4_Options options = {
	.core_file = P4_CORE_FILE,
	.block_file = P4_BLOCK_FILE,
	.data_stack_size = P4_STACK_SIZE,
	.return_stack_size = P4_STACK_SIZE,
};

static void *p4_program_end;
static P4_Word *p4_builtin_words;
static P4_Ctx * volatile signal_ctx;
static unsigned char base36[256];
static char base36_digits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

static int is_tty;
#ifdef HAVE_TCGETATTR
static int tty_fd = -1;
static struct termios tty_raw;
static struct termios tty_raw_nb;
static struct termios tty_saved;
#endif

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
	"CLOSE",
	"CREATE",
	"DELETE",
	"FILE",
	"FILE",
	"FILE",
	"FLUSH",
	"OPEN",
	"READ",
	"READ",
	"RENAME",
	"REPOSITION",
	"RESIZE",
	"WRITE",
	"WRITE",
	"Malformed",
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
		case SIGINT:
			signum = P4_THROW_USER; break;
		case SIGSEGV:
			signum = P4_THROW_EFAULT; break;
		}
		LONGJMP(signal_ctx->on_throw, signum);
	}
	abort();
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
	signal(SIGSEGV, sig_int);

	is_tty = isatty(fileno(stdin));
	setvbuf(stdout, NULL, _IOLBF, 0);
	setvbuf(stderr, NULL, _IOLBF, 0);

	/* Remember the split between the program and its static
	 * data and data dynamically allocated later.  This can be
	 * used to distinguish between built-in and loaded words.
	 */
	p4_program_end = sbrk(0);

	/* Setup base 36 number conversion table. */
	(void) memset(base36, 0xFF, sizeof (base36));
	for (i = 0, p = base36_digits; *p != '\0'; p++, i++) {
		base36[toupper(*p)] = i;
		base36[tolower(*p)] = i;
	}
#ifdef HAVE_TCGETATTR
# ifdef HAVE_CTERMID
	tty_fd = open(ctermid(NULL), O_RDWR, S_IRWXU|S_IRWXG|S_IRWXO);
# else
	tty_fd = fileno(stdin);
# endif
	if (tty_fd != -1) {
		(void) tcgetattr(tty_fd, &tty_saved);

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
}

int
p4LoadFile(P4_Ctx *ctx, const char *file)
{
	struct stat sb;
	int rc = -1, cwd;
	char *path_copy, *path, *next;

	if ((cwd = open(".", O_RDONLY)) < 0) {
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

/**
 * @param s
 *	A pointer to a C string to reverse in place.
 *
 * @param length
 *	The length of the C string.
 */
void
p4StrRev(P4_Char *s, P4_Size length)
{
	P4_Char ch, *x, *y;
	for (x = s, y = s+length; x < --y; x++) {
		ch = *y;
		*y = *x;
		*x = ch;
	}
}

char *
p4StrDup(const P4_Char *str, P4_Size length)
{
	char *dup;

	if ((dup = malloc(length + 1)) != NULL) {
		strncpy(dup, str, length)[length] = '\0';
	}

	return dup;
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
		int digit = base36[str.string[offset]];
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

		/* Treat a space as indicating any white space. */
		if (ch == delim || (delim == ' ' && isspace(ch))) {
			/* Do NOT terminate the parsed string, since the
			 * source might be a block buffer or some read-only
			 * string, which cannot be modified.
			 */
			break;
		}
	}

	parsed.length = offset - input->offset;
	input->offset = offset + (offset < input->length);

	return parsed;
}

P4_String
p4ParseName(P4_Input *input)
{
	/* Skip leading spaces */
	for ( ; input->offset < input->length; input->offset++) {
		if (isprint(input->buffer[input->offset]) && input->buffer[input->offset] != ' ') {
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

	for (count = 0, cell = base + length; base <= --cell; ) {
		if ((count & 3) == 0) {
			(void) fprintf(fp, "top-%.2u  ", count);
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
			(void) fprintf(fp, P4_PTR_FMT" ", addr);
			s = addr;
		}
		(void) fprintf(fp, " %.2x", (unsigned char) *addr);
		if ((++count & 0x3) == 0) {
			(void) fputc(' ', fp);
		}
		if ((count & 0xF) == 0) {
			fputc(' ', fp);
			for ( ; s < addr; s++) {
				(void) fputc(isprint(*s) ? *s : '.', fp);
			}
			(void) fputc('\n', fp);
		}
	}
	if ((count & 0xF) != 0) {
		do {
			(void) fputs("   ", fp);
			count++;
			if ((count & 0x3) == 0) {
				(void) fputc(' ', fp);
			}
		} while ((count & 0xF) != 0);
		(void) fputc(' ', fp);
		for ( ; s < addr; s++) {
			(void) fputc(isprint(*s) ? *s : '.', fp);
		}
		(void) fputc('\n', fp);
	}
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
	if (input->fd < STDIN_FILENO) {
		return input->offset < input->length ? input->buffer[input->offset++] : EOF;
	}
	if (input->fp != NULL) {
		return fgetc(input->fp);
	}
	return p4ReadByte(input->fd);
}

P4_Int
p4Accept(P4_Input *input, P4_Char *buf, P4_Size size)
{
	int ch;
	P4_Char *ptr;

	if (input->fp == NULL || size-- <= 1) {
		return 0;
	}
	for (ptr = buf; ptr - buf < size; ) {
		if ((ch = p4GetC(input)) == EOF) {
			if (ptr - buf == 0) {
				return EOF;
			}
			break;
		}
		if (ch == '\n' || ch == '\r') {
			break;
		}
		*ptr++ = (P4_Char) ch;
	}

	return ptr - buf;
}

P4_Uint
p4Refill(P4_Ctx *ctx, P4_Input *input)
{
	P4_Int n;

	if (P4_INPUT_IS_STR(ctx->input)) {
		return 0;
	}
#ifdef HAVE_TCSETATTR
	/* For a terminal restore original line input and echo settings. */
	if (tty_fd != 0 && P4_INPUT_IS_TERM(ctx->input)) {
		(void) tcsetattr(tty_fd, TCSADRAIN, &tty_saved);
	}
#endif
	if ((n = p4Accept(&ctx->input, ctx->input.buffer, ctx->input.size)) < 0) {
		return 0;
	}
	input->length = n;
	input->offset = 0;

	return 1;
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
	if ((fd = open(file, O_CREAT|O_RDWR, S_IRWXU|S_IRWXG|S_IRWXO)) < 0 || flock(fd, LOCK_EX)) {
		const char *home = getenv("HOME");
		if (home == NULL || chdir(home)) {
			goto error1;
		}
		if ((fd = open(file, O_CREAT|O_RDWR, S_IRWXU|S_IRWXG|S_IRWXO)) < 0 || flock(fd, LOCK_EX)) {
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

void
p4BlockLoad(P4_Ctx *ctx, P4_Uint blk_num)
{
	P4_INPUT_PUSH(&ctx->input);

	p4BlockBuffer(ctx, blk_num, 1);

	/* Change input source to the block buffer. */
	ctx->input.buffer = ctx->block.buffer;
	ctx->input.length = P4_BLOCK_SIZE;
	ctx->input.blk = blk_num;
	ctx->input.offset = 0;
	ctx->input.fp = NULL;

	/* While input is a block, treat it as an input string for
	 * interpreting, otherwise REFILL would proceed to the next
	 * block, contrary to the defintion of LOAD.
	 */
	ctx->input.fd = P4_INPUT_STR;

	(void) p4Repl(ctx);

	P4_INPUT_POP(&ctx->input);
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

	if ((word = calloc(1, sizeof (*word))) == NULL) {
		goto error0;
	}
	word->mdata = sizeof (*word->data);

	if ((word->name.string = p4StrDup(name, length)) == NULL) {
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
	if (SIZE_MAX - word->mdata <= size) {
		LONGJMP(ctx->on_throw, P4_THROW_RESIZE);
	}

	if ((word = realloc(word, sizeof (*word) + size)) == NULL) {
		LONGJMP(ctx->on_throw, P4_THROW_RESIZE);
	}

	/* ... but house keeping done in address units. */
	word->mdata = size;
	word->ndata += n;

	return word;
}

P4_Word *
p4WordAppend(P4_Ctx *ctx, P4_Word *word, P4_Cell data)
{
	P4_Size index = word->ndata = P4_CELL_ALIGN(word->ndata);
	word = p4WordAllot(ctx, word, sizeof (P4_Cell));
	word->data[index / sizeof (P4_Cell)] = data;

	return word;
}

P4_Word *
p4FindWord(P4_Ctx *ctx, P4_Char *caddr, P4_Size length)
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

P4_Word *
p4FindXt(P4_Ctx *ctx, P4_Xt xt)
{
	P4_Word *word;

	for (word = ctx->words; word != NULL; word = word->prev) {
		if (xt == word) {
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

P4_Ctx *
p4Create()
{
	P4_Ctx *ctx;

	if ((ctx = calloc(1, sizeof (*ctx))) == NULL) {
		goto error0;
	}
	ctx->radix = 10;
	ctx->unget = EOF;
	ctx->input.fp = stdin;
	ctx->input.fd = fileno(stdin);
	ctx->state = P4_STATE_INTERPRET;

	if ((ctx->rs.base = malloc(options.return_stack_size * sizeof (*ctx->rs.base))) == NULL) {
		goto error0;
	}
	ctx->rs.size = options.return_stack_size;
	P4_RESET(ctx->rs);

	if ((ctx->ds.base = malloc(options.data_stack_size * sizeof (*ctx->ds.base))) == NULL) {
		goto error0;
	}
	ctx->ds.size = options.data_stack_size;
	P4_RESET(ctx->ds);

	ctx->block_fd = p4BlockOpen(P4_BLOCK_FILE);

	return ctx;
error0:
	p4Free(ctx);
	return NULL;
}

static void
p4Bp(P4_Ctx *ctx)
{
	(void) printf(">> %.*s\r\n", (int)ctx->input.length, ctx->input.buffer);
	(void) printf(">> %*c\r\n", (int)ctx->input.offset, '^');
}

static void
p4StackCheck(P4_Ctx *ctx)
{
	if (P4_IS_OVER(ctx->ds)) {
		p4Bp(ctx);
		LONGJMP(ctx->on_throw, P4_THROW_DS_OVER);
	}
	if (P4_IS_UNDER(ctx->ds)) {
		p4Bp(ctx);
		LONGJMP(ctx->on_throw, P4_THROW_DS_UNDER);
	}
	if (P4_IS_OVER(ctx->rs)) {
		p4Bp(ctx);
		LONGJMP(ctx->on_throw, P4_THROW_RS_OVER);
	}
	if (P4_IS_UNDER(ctx->rs)) {
		p4Bp(ctx);
		LONGJMP(ctx->on_throw, P4_THROW_RS_UNDER);
	}
}

static void
p4Align(P4_Ctx *ctx)
{
	if (ctx->words->mdata < ctx->words->ndata) {
		p4Bp(ctx);
		LONGJMP(ctx->on_throw, P4_THROW_RESIZE);
	}
	ctx->words->ndata = P4_CELL_ALIGN(ctx->words->ndata);
}

static int
p4Exception(P4_Ctx *ctx, int code)
{
	if (code == P4_THROW_OK) {
		return code;
	}
	(void) printf("%d thrown: %s", code, P4_THROW_future < code && code < 0 ? p4_exceptions[-code] : "?");
	if (ctx->state == P4_STATE_COMPILE) {
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
	(void) fputc('\n', stdout);
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
	static P4_Word w_lit = P4_WORD("_lit", &&_lit, 0);
	static P4_Word w_exit = P4_WORD("_exit", &&_exit, 0);
	static P4_Word w_repl = P4_WORD("_repl", &&_repl, 0);
	static P4_Word w_post = P4_WORD("POSTPONE", &&_post, 0);

	/* When the REPL executes a word, it puts the XT of the word here
	 * and starts the machine with the IP pointed to exec[].  When the
	 * word completes the next XT (w_repl) transitions from threaded
	 * code back into the C driven REPL.
	 */
	static P4_Cell exec[] = { { 0 }, { .w = &w_repl } };

	static P4_Word words[] = {
		/* Constants. */
		P4_WORD("address-unit-bits",	&&_char_bit,	0),	// p4
		P4_WORD("floored",		&&_floored,	0),	// p4
		P4_WORD("max-char",		&&_max_char,	0),	// p4
		P4_WORD("max-n",		&&_max_n,	0),	// p4
		P4_WORD("max-u",		&&_max_u,	0),	// p4

		/* Internal support. */
		P4_WORD("_args",	&&_args,	0),		// p4
		P4_WORD("_bp",		&&_bp,		P4_BIT_IMM),	// p4
		P4_WORD("_branch",	&&_branch,	0),		// p4
		P4_WORD("_branchz",	&&_branchz,	0),		// p4
		P4_WORD("_call",	&&_call,	0),		// p4
		P4_WORD("_ds",		&&_ds,		0),		// p4
		P4_WORD("_dsp@",	&&_dsp_get,	0),		// p4
		P4_WORD("_dsp!",	&&_dsp_put,	0),		// p4
		P4_WORD("_ds_size",	&&_ds_size,	0),		// p4
		P4_WORD("_ip",		&&_ip,		0),		// p4
		P4_WORD("_lit",		&&_lit,		0),		// p4
		P4_WORD("_longjmp",	&&_longjmp,	0),		// p4
		P4_WORD("_rs",		&&_rs,		0),		// p4
		P4_WORD("_rsp@",	&&_rsp_get,	0),		// p4
		P4_WORD("_rsp!",	&&_rsp_put,	0),		// p4
		P4_WORD("_rs_size",	&&_rs_size,	0),		// p4
		P4_WORD("_stack_dump",	&&_stack_dump,	0),		// p4

		/* Compiling Words */
		P4_WORD("'",		&&_tick,	0),
		P4_WORD(":",		&&_colon,	0),
		P4_WORD(";",		&&_semicolon,	P4_BIT_IMM),
		P4_WORD(">BODY",	&&_body,	0),
		P4_WORD("CREATE",	&&_create,	0),
		P4_WORD("DOES>",	&&_does,	0),
		P4_WORD("EVALUATE",	&&_evaluate,	0),
		P4_WORD("EXECUTE",	&&_execute,	0),
		P4_WORD("EXIT",		&&_exit,	0),
		P4_WORD("IMMEDIATE",	&&_immediate,	P4_BIT_IMM),
		P4_WORD("MARKER",	&&_marker,	0),
		P4_WORD("POSTPONE",	&&_postpone,	P4_BIT_IMM),
		P4_WORD("STATE",	&&_state,	0),

		/* Numeric formatting. */
		P4_WORD("<#",		&&_pic_start,	0),
		P4_WORD("#",		&&_pic_digit,	0),
		P4_WORD("#S",		&&_pic_rest,	0),
		P4_WORD("#>",		&&_pic_end,	0),
		P4_WORD("BASE",		&&_base,	0),
		P4_WORD("HOLD",		&&_pic_hold,	0),
		P4_WORD("SIGN",		&&_pic_sign,	0),
		P4_WORD("/hold",	&&_pic_size,	0),		// p4

		/* Data Space - Alignment */
		P4_WORD("CELLS",	&&_cells,	0),
		P4_WORD("CHARS",	&&_chars,	0),
		P4_WORD("ALIGN",	&&_align,	0),
		P4_WORD("ALLOT",	&&_allot,	0),
		P4_WORD("HERE",		&&_here_addr,	0),
		P4_WORD(">here",	&&_here_offset,	0),		// p4
		P4_WORD("UNUSED",	&&_unused,	0),

		/* Data Space - Access */
		P4_WORD("!",		&&_store,	0),
		P4_WORD(">R",		&&_to_rs,	0),
		P4_WORD("@",		&&_fetch,	0),
		P4_WORD("C!",		&&_cstore,	0),
		P4_WORD("C@",		&&_cfetch,	0),
		P4_WORD("CMOVE",	&&_move,	0),
		P4_WORD("CMOVE>",	&&_move,	0),
		P4_WORD("CS-PICK",	&&_pick,	0),		// C: on data stack
		P4_WORD("CS-ROLL",	&&_roll,	0),		// C: on data stack
		P4_WORD("DROP",		&&_drop,	0),
		P4_WORD("DUP",		&&_dup,		0),
		P4_WORD("llor",		&&_llor,	0),		// p4
		P4_WORD("MOVE",		&&_move,	0),
		P4_WORD("PICK",		&&_pick,	0),
		P4_WORD("R>",		&&_from_rs,	0),
		P4_WORD("R@",		&&_rs_copy,	0),
		P4_WORD("ROLL",		&&_roll,	0),
		P4_WORD("SWAP",		&&_swap,	0),

		/* Dynamic Memory */
		P4_WORD("ALLOCATE",	&&_allocate,	0),
		P4_WORD("FREE",		&&_free,	0),
		P4_WORD("RESIZE",	&&_resize,	0),

		/* Operators */
		P4_WORD("*",		&&_mul,		0),
		P4_WORD("+",		&&_add,		0),
		P4_WORD("-",		&&_sub,		0),
		P4_WORD("/",		&&_div,		0),
		P4_WORD("/MOD",		&&_sm_div_rem,	0),
		P4_WORD("AND",		&&_and,		0),
		P4_WORD("FM/MOD",	&&_fm_div_mod,	0),
		P4_WORD("INVERT",	&&_not,		0),
		P4_WORD("LSHIFT",	&&_lshift,	0),
		P4_WORD("MOD",		&&_mod,		0),
		P4_WORD("OR",		&&_or,		0),
		P4_WORD("RSHIFT",	&&_rshift,	0),
		P4_WORD("SM/REM",	&&_sm_div_rem,	0),
		P4_WORD("UM/MOD",	&&_um_div_mod,	0),
		P4_WORD("XOR",		&&_xor,		0),

		/* Comparisons */
		P4_WORD("0=",		&&_eq0,		0),
		P4_WORD("0<",		&&_lt0,		0),
		P4_WORD("U<",		&&_u_lt,	0),

		/* I/O */
		P4_WORD(">IN",		&&_input_offset,0),
		P4_WORD("ACCEPT",	&&_accept,	0),
		P4_WORD("BLK",		&&_blk,		0),
		P4_WORD("BLOCK",	&&_block,	0),
		P4_WORD("blocks",	&&_blocks, 	0),		// p4
		P4_WORD("BUFFER",	&&_buffer,	0),
		P4_WORD("DUMP",		&&_dump,	P4_BIT_IMM),
		P4_WORD("EMIT",		&&_emit,	0),
		P4_WORD("EMPTY-BUFFERS", &&_empty_buffers, 0),
		P4_WORD("epoch-seconds", &&_epoch_seconds, 0),		// p4
		P4_WORD("INCLUDED",	&&_included,	0),
		P4_WORD("KEY",		&&_key,		0),
		P4_WORD("KEY?",		&&_key_ready,	0),
		P4_WORD("LOAD",		&&_load,	0),
		P4_WORD("MS",		&&_ms,		0),
		P4_WORD("PARSE",	&&_parse,	0),
		P4_WORD("parse-escape",	&&_parse_escape,0),		// p4
		P4_WORD("PARSE-NAME",	&&_parse_name,	0),
		P4_WORD("REFILL",	&&_refill,	0),
		P4_WORD("SAVE-BUFFERS",	&&_save_buffers, 0),
		P4_WORD("SOURCE",	&&_source,	0),
		P4_WORD("SOURCE-ID",	&&_source_id,	0),
		P4_WORD("TIME&DATE",	&&_time_date,	0),
		P4_WORD("UPDATE",	&&_update,	0),

		/* Tools*/
		P4_WORD("BYE",		&&_bye,		0),
		P4_WORD("bye-code",	&&_bye_code,	0),		// p4
		P4_WORD("SEE",		&&_see,		P4_BIT_IMM),
		P4_WORD("WORDS",	&&_words,	0),

		P4_WORD(NULL,		NULL,		0),
	};

	if (p4_builtin_words == NULL) {
		/* Link up the base dictionary. */
		for (word = words; word->code != NULL; word++) {
			word[1].prev = word;
		}
		p4_builtin_words = word->prev;
	}
	if (ctx->words == NULL) {
		ctx->words = p4_builtin_words;
		if (*options.core_file != '\0' && p4LoadFile(ctx, options.core_file)) {
			return P4_THROW_EIO;
		}
	}

#define NEXT	goto _next

	signal_ctx = ctx;
	SETJMP_PUSH(ctx->on_throw);
	if ((rc = SETJMP(ctx->on_throw)) != 0) {
		return rc;
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
			word = p4FindWord(ctx, str.string, str.length);
			if (word == NULL) {
				if (p4StrNum(str, ctx->radix, &x.n) != str.length) {
					/* Not a word, not a number. */
					(void) printf("\"%.*s\" ", (int)str.length, str.string);
					/* Throwing while interactive is really annoying, since
					 * it clears the stacks of the user's workspace; disagrees
					 * with Forth 200x 18-1 3.4.d
					 */
					if (!P4_INTERACTIVE(ctx)) {
						LONGJMP(ctx->on_throw, P4_THROW_UNDEFINED);
					}
					(void) printf("?\r\n");
					continue;
				}
				if (ctx->state == P4_STATE_COMPILE) {
					ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) &w_lit);
					ctx->words = p4WordAppend(ctx, ctx->words, x);
				} else {
					P4_PUSH(ctx->ds, x);
					p4StackCheck(ctx);
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

	SETJMP_POP(ctx->on_throw);
	return rc;

		// Indirect threading.
_next:		p4StackCheck(ctx);
		w = *ip++;
		goto *w.xt->code;

		// ( xt -- )
_execute:	w = P4_POP(ctx->ds);
		goto *w.xt->code;

		// ( i*x -- j*y )(R: -- ip)
_enter:		P4_PUSH(ctx->rs, ip);
		// w contains xt loaded by _next or _execute.
		ip = w.xt->data;
		NEXT;

		// ( i*x -- i*x )(R:ip -- )
_exit:		ip = P4_POP(ctx->rs).p;
		NEXT;

		// ( -- )
_bye:		exit(0);

		// ( ex_code -- )
_bye_code:	w = P4_TOP(ctx->ds);
		exit((int) w.n);

		// ( -- )
_bp:		p4Bp(ctx);
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

		// ( -- addr )
_ip:		P4_PUSH(ctx->ds, (P4_Cell *) &ip);
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
_floored:	P4_PUSH(ctx->ds, (P4_Int) 0);
		NEXT;

		// ( -- u )
_max_char:	P4_PUSH(ctx->ds, (P4_Uint) P4_CHAR_MAX);
		NEXT;

		// ( -- u )
_max_n:		P4_PUSH(ctx->ds, (P4_Uint) P4_INT_MAX);
		NEXT;

		// ( -- u )
_max_u:		P4_PUSH(ctx->ds, (P4_Uint) P4_UINT_MAX);
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
		// (R: -- ip)
_colon:		if (ctx->state == P4_STATE_COMPILE) {
			LONGJMP(ctx->on_throw, P4_THROW_COMPILING);
		}
		ctx->state = P4_STATE_COMPILE;
		str = p4ParseName(&ctx->input);
		word = p4WordCreate(ctx, str.string, str.length, &&_enter);
		P4_WORD_SET_HIDDEN(word);
		NEXT;

		// (C: colon -- )
_semicolon:	ctx->state = P4_STATE_INTERPRET;
		ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) &w_exit);
		P4_WORD_CLEAR_HIDDEN(ctx->words);
		NEXT;

		// ( -- )
_immediate:	P4_WORD_SET_IMM(ctx->words);
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
		 */
		// ( -- addr )
_create:	str = p4ParseName(&ctx->input);
		word = p4WordCreate(ctx, str.string, str.length, &&_data_field);
		P4_WORD_SET_CREATED(word);
		// Reserve the first cell for a code pointer for DOES>.
		word->ndata += P4_CELL;
		NEXT;

		// DOES>
_does:		word = ctx->words;
		if (!P4_WORD_WAS_CREATED(word)) {
			LONGJMP(ctx->on_throw, P4_THROW_NOT_CREATED);
		}
		word->code = &&_do_does;
		// New word's code follows DOES> of the defining word.
		word->data[0].p = ip;
		goto _exit;

		// ( -- addr) and chain to defining word after DOES>.
_do_does:	P4_PUSH(ctx->ds, w.xt->data + 1);
		P4_PUSH(ctx->rs, ip);
		ip = w.xt->data[0].p;
		NEXT;

		// ( -- addr )
_data_field:	P4_PUSH(ctx->ds, w.xt->data + 1);
		NEXT;

		// ( xt -- addr )
_body:		w = P4_TOP(ctx->ds);
		if (!P4_WORD_WAS_CREATED(w.w)) {
			LONGJMP(ctx->on_throw, P4_THROW_NOT_CREATED);
		}
		P4_TOP(ctx->ds).p = w.xt->data + 1;
		NEXT;


		/*
		 * Compiling
		 */
		// ( -- xt )
_tick:		str = p4ParseName(&ctx->input);
		word = p4FindWord(ctx, str.string, str.length);
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
_align:		p4Align(ctx);
		NEXT;


		/* POSTPONE <spaces>name
		 *
		 * <Arnie>
		 * "Compile now or compile later! Execute now or execute later!"
		 * </Arnie>
		 *
		 * For immediate words, simply compile the word into the current
		 * definition for execution when the current definition is later
		 * executed (during definition of another word).  Otherwise compile
		 * the word during the definition of another word.
		 *
		 * Ideally we don't need _post for an immediate word, but it makes
		 * SEE easier to implement.
		 */
_postpone:	str = p4ParseName(&ctx->input);
		word = p4FindWord(ctx, str.string, str.length);
		if (word == NULL) {
			p4Bp(ctx);
			LONGJMP(ctx->on_throw, P4_THROW_UNDEFINED);
		}
		ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) &w_post);
		ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) word);
		NEXT;

_post:		w = *ip++;
		if (P4_WORD_IS_IMM(w.w)) {
			goto *w.xt->code;
		}
		ctx->words = p4WordAppend(ctx, ctx->words, w);
		NEXT;

		/*
		 * Context variables
		 */
		// ( -- caddr u )
_args:		P4_PUSH(ctx->ds, (P4_Cell *) options.argv);
		P4_PUSH(ctx->ds, (P4_Int) options.argc);
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

		// ( -- )
_pic_start:	ctx->picptr = memset(ctx->pic, ' ', sizeof (ctx->pic));
		NEXT;

		// ( x -- caddr u )
_pic_end:	w.u = ctx->picptr - ctx->pic;
		p4StrRev(ctx->pic, w.u);
		P4_TOP(ctx->ds).s = ctx->pic;
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( u1 -- u2 )
_pic_digit:	if (ctx->pic + sizeof (ctx->pic) <= ctx->picptr) {
			LONGJMP(ctx->on_throw, P4_THROW_PIC_OVER);
		}
		w = P4_TOP(ctx->ds);
		*ctx->picptr++ = base36_digits[w.u % ctx->radix];
		P4_TOP(ctx->ds).u /= ctx->radix;
		NEXT;

		// ( u -- 0 )
_pic_rest:	w = P4_TOP(ctx->ds);
		do {
			if (ctx->pic + sizeof (ctx->pic) <= ctx->picptr) {
				LONGJMP(ctx->on_throw, P4_THROW_PIC_OVER);
			}
			*ctx->picptr++ = base36_digits[w.u % ctx->radix];
			w.u /= ctx->radix;
		} while (0 < w.u);
		P4_TOP(ctx->ds).u = 0;
		NEXT;

		// ( n -- )
_pic_sign:	if (ctx->pic + sizeof (ctx->pic) <= ctx->picptr) {
			LONGJMP(ctx->on_throw, P4_THROW_PIC_OVER);
		}
		w = P4_POP(ctx->ds);
		if (w.n < 0) {
			*ctx->picptr++ = '-';
		}
		NEXT;

		// ( ch -- )
_pic_hold:	if (ctx->pic + sizeof (ctx->pic) <= ctx->picptr) {
			LONGJMP(ctx->on_throw, P4_THROW_PIC_OVER);
		}
		w = P4_POP(ctx->ds);
		*ctx->picptr++ = (P4_Char) w.u;
		NEXT;

		// ( -- n )
_pic_size:	P4_PUSH(ctx->ds, sizeof(ctx->pic));
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
		x = P4_PICK(ctx->ds, w.n);
		P4_PUSH(ctx->ds, x);
		NEXT;

		// ( x y -- y x )
_swap:		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds) = w;
		P4_PUSH(ctx->ds, x);
		NEXT;

		// (x -- )(R: -- x )
_to_rs:		w = P4_POP(ctx->ds);
		P4_PUSH(ctx->rs, w);
		NEXT;

		// (R: x -- )
_from_rs:	w = P4_POP(ctx->rs);
		P4_PUSH(ctx->ds, w);
		NEXT;

		// (R: x -- x)
_rs_copy:	w = P4_TOP(ctx->rs);
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( xu xu-1 ... x0 u –– xu-1 ... x0 xu )
_roll:		w = P4_POP(ctx->ds);
		x = P4_PICK(ctx->ds, w.n);
		(void) memmove(ctx->ds.top - w.n, ctx->ds.top - w.n + 1, w.n * P4_CELL);
		P4_TOP(ctx->ds) = x;
		NEXT;

		// ( xu xu-1 ... x0 u –– x0 xu xu-1 ... x1 )
_llor:		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		(void) memmove(ctx->ds.top - w.n + 1, ctx->ds.top - w.n, w.n * P4_CELL);
		ctx->ds.top[-w.n] = x;
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
		P4_TOP(ctx->ds).n /= w.n;
		NEXT;

	{	// ( dend dsor -- rem quot )
		// C99+ specifies symmetric division.
		// Dividend Divisor Remainder Quotient
		//       10       7         3        1
		//      -10       7        -3       -1
		//       10      -7         3       -1
		//      -10      -7        -3        1
		//
		DIV_T qr;
_sm_div_rem:	w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		qr = DIV(x.n, w.n);
		P4_TOP(ctx->ds).n = qr.rem;
		P4_PUSH(ctx->ds, qr.quot);
		NEXT;
	}
	{	// ( dend dsor -- mod quot )
		// Dividend Divisor Remainder Quotient
		//       10       7         3        1
		//      -10       7         4       -2
		//       10      -7        -4       -2
		//      -10      -7        -3        1
		//
		P4_Int q, m;
_fm_div_mod:	w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		q = x.n / w.n;
		m = x.n % w.n;
		if (m != 0 && (w.n ^ x.n) < 0) {
			q -= 1;
			m += w.n;
		}
		P4_TOP(ctx->ds).n = m;
		P4_PUSH(ctx->ds, q);
		NEXT;
	}
	{	// ( dend dsor -- mod quot )
		P4_Uint q, m;
_um_div_mod:	w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		q = x.u / w.u;
		m = x.u % w.u;
		P4_TOP(ctx->ds).u = m;
		P4_PUSH(ctx->ds, q);
		NEXT;
	}
		// ( n1 n2 -- n3 )
_mod:		w = P4_POP(ctx->ds);
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
		P4_TOP(ctx->ds).u = w.u == 0 ? ~0 : 0;
		NEXT;

		// ( x -- flag )
_lt0:		w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).u = w.n < 0 ? ~0 : 0;
		NEXT;

		// ( u1 u2 -- )
_u_lt:		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).u = x.u < w.u;
		NEXT;


		/*
		 * I/O
		 */
		// ( -- u )
_input_offset:	P4_PUSH(ctx->ds, (P4_Cell *) &ctx->input.offset);
		NEXT;

		// ( -- caddr u )
_source:	P4_PUSH(ctx->ds, ctx->input.buffer + ctx->input.offset);
		P4_PUSH(ctx->ds, ctx->input.length - ctx->input.offset);
		NEXT;

		// ( -- -2 | -1 | 0 | fd )
_source_id:	P4_PUSH(ctx->ds, ctx->input.fd);
		NEXT;

		// ( caddr +n1 -- +n2 )
_accept:	w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		w.u = p4Accept(&ctx->input, x.s, w.u);
		P4_TOP(ctx->ds) = w;
		NEXT;

		// ( -- flag)
_refill:	w.u = p4Refill(ctx, &ctx->input);
		P4_PUSH(ctx->ds, w);
		NEXT;

		// ( -- n )
_key:		(void) fflush(stdout);
		if (ctx->unget != EOF) {
			P4_PUSH(ctx->ds, ctx->unget);
			ctx->unget = EOF;
			NEXT;
		}
#ifdef HAVE_TCSETATTR
		P4_PUSH(ctx->ds, (P4_Int)EOF);
		if (tcsetattr(tty_fd, TCSANOW, &tty_raw) == 0) {
			P4_TOP(ctx->ds).u = p4ReadByte(tty_fd);
		}
#else
		P4_PUSH(ctx->ds, p4ReadByte(tty_fd));
#endif
		NEXT;

		// ( -- flag )
_key_ready:	(void) fflush(stdout);
		if (ctx->unget == EOF) {
#ifdef HAVE_TCSETATTR
			if (tcsetattr(tty_fd, TCSANOW, &tty_raw_nb) == 0) {
				ctx->unget = p4ReadByte(tty_fd);
			}
#else
			if (p4SetNonBlocking(tty_fd, 1) == 0) {
				ctx->unget = p4ReadByte(tty_fd);
				(void) p4SetNonBlocking(tty_fd, 0);
			}
#endif
		}
		P4_PUSH(ctx->ds, (P4_Uint)(ctx->unget != EOF ? ~0 : 0));
		NEXT;

		// ( c -- )
_emit:		w = P4_POP(ctx->ds);
		(void) fputc(w.n, stdout);
		NEXT;

		// ( caddr u -- )
_included:	w = P4_POP(ctx->ds);
		x = P4_POP(ctx->ds);
		if ((cstr = p4StrDup(x.s, w.u)) == NULL) {
			LONGJMP(ctx->on_throw, P4_THROW_ALLOCATE);
		}
		(void) p4LoadFile(ctx, cstr);
		free(cstr);
		NEXT;


		/*
		 * Block I/O
		 */
		// ( -- aaddr )
_blk:		P4_PUSH(ctx->ds, (P4_Cell *) &ctx->block.number);
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

		// ( u -- )
_load:		w = P4_POP(ctx->ds);
		p4BlockLoad(ctx, w.u);
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
		// ( char -- c-addr u )
_parse:		w = P4_POP(ctx->ds);
		str = p4Parse(&ctx->input, w.u, 0);
		P4_PUSH(ctx->ds, str.string);
		P4_PUSH(ctx->ds, str.length);
		NEXT;

		// ( char -- c-addr u )
_parse_escape:	w = P4_POP(ctx->ds);
		str = p4Parse(&ctx->input, w.u, 1);
		P4_PUSH(ctx->ds, str.string);
		P4_PUSH(ctx->ds, str.length);
		NEXT;

		// ( -- c-addr u )
_parse_name:	str = p4ParseName(&ctx->input);
		P4_PUSH(ctx->ds, str.string);
		P4_PUSH(ctx->ds, str.length);
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

		// ( -- )
_see:		str = p4ParseName(&ctx->input);
		word = p4FindWord(ctx, str.string, str.length);
		if (word == NULL) {
			(void) printf("\"%.*s\" ", (int)str.length, str.string);
			LONGJMP(ctx->on_throw, P4_THROW_UNDEFINED);
		}
		if ((void *) word < p4_program_end) {
			(void) printf(": %.*s ( _builtin ) ;\r\n", (int)word->name.length, word->name.string);
			NEXT;
		}
		if (word->code == &&_enter) {
			(void) printf(
				word->name.length == 0 ? ":NONAME" : ": %.*s ",
				(int) word->name.length, word->name.string
			);
			for (w.p = word->data; w.p->xt != &w_exit; w.p++) {
				x = *w.p;
				(void) printf(
					"%.*s ",
					(int) x.w->name.length, x.w->name.string
				);
				if (x.w->code == &&_lit) {
					(void) printf("[ "P4_INT_FMT" , ] ", (*++w.p).n);
				} else if (x.w->code == &&_branch || x.w->code == &&_branchz || x.w->code == &&_call) {
					(void) printf("[ "P4_INT_FMT" CELLS , ] ", (*++w.p).n / P4_CELL);
				}
			}
			(void) printf("; %s\r\n", P4_WORD_IS_IMM(word) ? "IMMEDIATE" : "");
		} else if (word->code == &&_do_does) {
			// Dump word's data.
			for (w.u = 1, x.u = P4_CELL; x.u < word->ndata; x.u += P4_CELL, w.u++) {
				(void) printf(P4_HEX_FMT" ", word->data[w.u].u);
			}
			// Search back for code field with _enter.
			for (w.p = word->data[0].p; w.p->xt != &&_enter; w.p--) {
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
			(void) printf("CREATE %.*s ", (int) word->name.length, word->name.string);
			for (stop = word->ndata / P4_CELL, x.u = 1; x.u < stop; x.u++) {
				(void) printf(P4_HEX_FMT" , ", word->data[x.u].u);
			}
			for (x.u *= P4_CELL; x.u < word->ndata; x.u++) {
				(void) printf(P4_CHAR_FMT" C, ", ((P4_Char *) word->data)[x.u]);
			}
			(void) printf("\r\n");
			p4MemDump(stdout, (P4_Char *)(word->data + 1), word->ndata - P4_CELL);
		} else {
			(void) printf("(unknown) 0x%p\r\n", word->code);
		}
		NEXT;

	{	// ( -- )
		P4_Uint column;
		unsigned short window[4] = { 24, 80 };
_words:
#ifdef TIOCGWINSZ
		ioctl(0, TIOCGWINSZ, window);
#endif
		column = 0;
		for (word = ctx->words; word != NULL; word = word->prev) {
			if (window[1] <= column + word->name.length + 1) {
				(void) printf("\r\n");
				column = 0;
			}
			column += fprintf(stdout, "%s ", word->name.string);
		}
		(void) printf("\r\n");
		NEXT;
	}
}

int
p4Eval(P4_Ctx *ctx)
{
	int rc = P4_THROW_OK;

	do {
		switch (rc) {
		default:
		case P4_THROW_ABORT:
		case P4_THROW_ABORT_MSG:
			(void) p4Exception(ctx, rc);
			P4_RESET(ctx->ds);
			/*@fallthrough@*/

		case P4_THROW_QUIT:
			P4_RESET(ctx->rs);
			ctx->input.fp = stdin;
			/*@fallthrough@*/

		case P4_THROW_OK:
			ctx->state = P4_STATE_INTERPRET;
			ctx->input.fd = fileno(ctx->input.fp);
			ctx->input.size = sizeof (ctx->tty);
			ctx->input.buffer = ctx->tty;
			ctx->input.length = 0;
			ctx->input.offset = 0;
			ctx->input.blk = 0;
		}
	} while ((rc = p4Repl(ctx)) != P4_THROW_OK);

	return rc;
}

int
p4EvalFile(P4_Ctx *ctx, const char *file)
{
	int rc;
	P4_Cell *ds, *rs;

	ds = ctx->ds.top;
	rs = ctx->rs.top;
	P4_INPUT_PUSH(&ctx->input);

	if ((ctx->input.fp = fopen(file, "r")) == NULL) {
		rc = P4_THROW_EIO;
	} else {
		ctx->state = P4_STATE_INTERPRET;
		ctx->input.fd = fileno(ctx->input.fp);
		ctx->input.size = sizeof (ctx->tty);
		ctx->input.buffer = ctx->tty;
		ctx->input.length = 0;
		ctx->input.offset = 0;
		ctx->input.blk = 0;
		if ((rc = p4Exception(ctx, p4Repl(ctx))) != 0) {
			ctx->rs.top = rs;
			ctx->ds.top = ds;
			rc = 0;
		}
		(void) fclose(ctx->input.fp);
	}

	P4_INPUT_POP(&ctx->input);

	return rc;
}

int
p4EvalString(P4_Ctx *ctx, P4_Char *str, size_t len)
{
	int rc;
	P4_Cell *ds, *rs;

	ds = ctx->ds.top;
	rs = ctx->rs.top;
	P4_INPUT_PUSH(&ctx->input);

	ctx->state = P4_STATE_INTERPRET;
	ctx->input.fd = P4_INPUT_STR;
	ctx->input.fp = NULL;
	ctx->input.length = len;
	ctx->input.buffer = str;
	ctx->input.offset = 0;
	ctx->input.blk = 0;

	if ((rc = p4Exception(ctx, p4Repl(ctx))) != 0) {
		ctx->rs.top = rs;
		ctx->ds.top = ds;
		rc = 0;
	}

	P4_INPUT_POP(&ctx->input);

	return rc;
}

#ifdef TEST
/***********************************************************************
 *** Main
 ***********************************************************************/

static const char usage[] =
"usage: p4 [-V][-b file][-c file][-d size][-r size] [script [args ...]]\n"
"\n"
"-b file\t\tblock file; default " P4_BLOCK_FILE "\n"
"-c file\t\tword definition file; default " P4_CORE_FILE "\n"
"-d size\t\tdata stack size in cells; default " QUOTE(P4_STACK_SIZE) "\n"
"-r size\t\treturn stack size in cells; default " QUOTE(P4_STACK_SIZE) "\n"
"-V\t\tbuild and version information\n"
"\n"
;

int
main(int argc, char **argv)
{
	int ch, rc;
	P4_Ctx *ctx;

	while ((ch = getopt(argc, argv, "b:c:d:r:V")) != -1) {
		switch (ch) {
		case 'b':
			options.block_file = optarg;
			break;
		case 'c':
			options.core_file = optarg;
			break;
		case 'd':
			options.data_stack_size = strtol(optarg, NULL, 10);
			break;
		case 'r':
			options.return_stack_size = strtol(optarg, NULL, 10);
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

	p4Init();
	(void) atexit(p4Fini);

	if ((ctx = p4Create()) == NULL) {
		(void) fprintf(stderr, "p4: %s\n", strerror(errno));
		return EXIT_FAILURE;
	}

	if (argc <= optind) {
		rc = p4Eval(ctx);
	} else if (optind < argc && (rc = p4EvalFile(ctx, argv[optind]))) {
		(void) fprintf(stderr, "p4: %s: %s\n", argv[optind], strerror(errno));
	}

	p4Free(ctx);

	return rc;
}

#endif /* TEST */
