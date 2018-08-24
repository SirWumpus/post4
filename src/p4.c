/*
 * p4.c
 *
 * Copyright 2007, 2018 by Anthony Howe. All rights reserved.
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
static P4_Word *p4_bultin_words;
static volatile int last_signal;
static P4_Ctx *signal_ctx;
static unsigned char base36[256];
static char base36_digits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

static int is_tty;
#ifdef HAVE_TCGETATTR
static int tty_fd = -1;
static struct termios tty_raw;
static struct termios tty_raw_nb;
static struct termios tty_saved;
#endif

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

static int p4Repl(P4_Ctx *ctx, int is_executing);

/***********************************************************************
 *** Context
 ***********************************************************************/

static void
sig_int(int signum)
{
	last_signal = signum;
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
p4LoadCore(P4_Ctx *ctx, const char *file)
{
	struct stat sb;
	int rc = -1, cwd;
	char *core_path, *path, *next;

	if ((cwd = open(".", O_RDONLY)) < 0) {
		goto error0;
	}
	if ((core_path = getenv("POST4_PATH")) == NULL || *core_path == '\0') {
		core_path = P4_CORE_PATH;
	}
	if ((core_path = strdup(core_path)) == NULL) {
		goto error1;
	}
	for (next = core_path; (path = strtok(next, ":")) != NULL; next = NULL) {
		if (stat(path, &sb) || !S_ISDIR(sb.st_mode) || chdir(path)) {
			continue;
		}
		if (stat(file, &sb) == 0 && S_ISREG(sb.st_mode)) {
			break;
		}
	}
	if (path == NULL) {
		(void) fprintf(stderr, "cannot find core words definition file: %s", options.core_file);
	} else {
		rc = p4EvalFile(ctx, file);
	}
error2:
	free(core_path);
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
 *	Special case radix 0 returns the first byte of s.
 *	Special case radix 1 returns the value of a backslash
 *	escape character; see p4CharLiteral().
 *
 * @return
 *	A number.
 */
P4_Int
p4StrToInt(const P4_Char *s, P4_Char **stop, P4_Uint base)
{
	P4_Int num;
	int digit, sign = 1;

	if (s == NULL) {
		return 0;
	}
	if (base == 0) {
		num = *s++;
	} else if (base == 1) {
		num = p4CharLiteral(*s++);
	} else {
		if (*s == '-') {
			sign = -1;
			s++;
		}

		for (num = 0; *s != '\0'; s++) {
			digit = base36[*(unsigned char *)s];
			if (base <= digit)
				break;
			num = num * base + digit;
		}
	}
	if (stop != NULL) {
		*stop = (char *) s;
	}

	return sign * num;
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
			input->buffer[offset] = '\0';
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
	for ( ; input->offset < input->length; input->offset++) {
		if (!isspace(input->buffer[input->offset]))
			break;
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
			(void) fprintf(fp, P4_HEX_FMT" ", (unsigned long) addr);
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
p4GetC(P4_Ctx *ctx)
{
	if (ctx->input.fd < STDIN_FILENO) {
		return ctx->input.offset < ctx->input.length ? ctx->input.buffer[ctx->input.offset++] : EOF;
	}
	if (ctx->input.fp != NULL) {
		return fgetc(ctx->input.fp);
	}
	return p4ReadByte(ctx->input.fd);
}

P4_Uint
p4Refill(P4_Ctx *ctx, P4_Input *input)
{
	P4_Int ch;
	P4_Uint i;

	if (P4_INPUT_IS_STR(ctx->input)) {
		return 0;
	}
#ifdef HAVE_TCSETATTR
	/* For a terminal restore original line input and echo settings. */
	if (tty_fd != 0 && P4_INPUT_IS_TERM(ctx->input)) {
		(void) tcsetattr(tty_fd, TCSADRAIN, &tty_saved);
	}
#endif
	input->offset = 0;
	for (i = 0; i < input->size; ) {
		if ((ch = p4GetC(ctx)) == EOF) {
			break;
		}
		input->buffer[i++] = (P4_Char) ch;
		if (ch == '\n') {
			break;
		}
	}
	input->length = i;

	return i;
}

/***********************************************************************
 *** Block I/O
 ***********************************************************************/

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
		memset(blanks, ' ', sizeof (blanks));

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

void
p4BlockBuffer(P4_Ctx *ctx, P4_Uint blk_num)
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
	/* Current there is no block buffer assignment stragegy beyond
	 * a single buffer per context.  Might add one day.
	 */
	if (ctx->block.state == P4_BLOCK_DIRTY && p4BlockWrite(ctx->block_fd, &ctx->block)) {
		LONGJMP(ctx->on_throw, P4_THROW_BLOCK_WR);
	}
	ctx->block.state = P4_BLOCK_CLEAN;
	ctx->block.number = blk_num;
}

void
p4BlockGet(P4_Ctx *ctx, P4_Uint blk_num)
{
	p4BlockBuffer(ctx, blk_num);
	if (p4BlockRead(ctx->block_fd, blk_num, &ctx->block)) {
		LONGJMP(ctx->on_throw, P4_THROW_BLOCK_RD);
	}
}

void
p4BlockLoad(P4_Ctx *ctx, P4_Uint blk_num)
{
	P4_INPUT_PUSH(&ctx->input);

	p4BlockGet(ctx, blk_num);

	/* Change input source to the block buffer. */
	ctx->input.buffer = ctx->block.buffer;
	ctx->input.length = P4_BLOCK_SIZE;
	ctx->input.fd = P4_INPUT_BLK;
	ctx->input.blk = blk_num;
	ctx->input.offset = 0;
	ctx->input.fp = NULL;

	p4Repl(ctx, 1);

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

	if ((word->name.string = malloc(length + 1)) == NULL) {
		goto error1;
	}
	(void) strncpy(word->name.string, name, length);
	word->name.string[length] = '\0';
	word->name.length = length;
	word->code = code;

	return word;
error1:
	free(word);
error0:
	LONGJMP(ctx->on_throw, P4_THROW_DICT_OVER);
}

P4_Word *
p4WordAllot(P4_Ctx *ctx, P4_Word *word, P4_Int n)
{
	/* Always allocate in cell units... */
	size_t size = P4_CELL_ALIGN(word->ndata + n);

	/* Check for size overflow. */
	if (size < SIZE_MAX - word->mdata) {
		if ((word = realloc(word, sizeof (*word) + size)) == NULL) {
			LONGJMP(ctx->on_throw, P4_THROW_RESIZE);
		}
		/* ... but house keeping done in address units. */
		word->mdata = size;
		word->ndata += n;
	} else {
		LONGJMP(ctx->on_throw, P4_THROW_RESIZE);
	}

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
		if (word->name.length > 0 && word->name.length == length
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
#ifdef CODE_FIELD
		if (xt == &word->code) {
#else
		if (xt == word) {
#endif
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
		(void) close(ctx->block_fd);
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
	(void) printf(">> %.*s", (int)ctx->input.length, ctx->input.buffer);
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
	if (ctx->words->ndata <= ctx->words->mdata) {
		ctx->words->ndata = P4_CELL_ALIGN(ctx->words->ndata);
		return;
	}
	p4Bp(ctx);
	LONGJMP(ctx->on_throw, P4_THROW_RESIZE);
}

static int
p4Repl(P4_Ctx *ctx, int is_executing)
{
	P4_Word *word;
	P4_String str;
	P4_Cell w, x, *ip;

	/* Wrap code pointers for Indirect Threading. */
	static P4_Word w_lit = P4_WORD("_lit", &&_lit, 0);
	static P4_Word w_exit = P4_WORD("_exit", &&_exit, 0);
	static P4_Word w_repl = P4_WORD("_repl", &&_repl, 0);

	/* When the REPL executes a word, it puts the XT of the word here
	 * and starts the machine with the IP pointed to exec[].  When the
	 * word completes the next XT (w_repl) transitions from threaded
	 * code back into the C driven REPL.
	 */
#ifdef CODE_FIELD
	static P4_Cell exec[] = { (P4_Cell) &w_repl.code };
#else
	static P4_Cell exec[] = { (P4_Cell) &w_repl };
#endif

	static P4_Word words[] = {
		/* Internal support. */
		P4_WORD("_args",	&&_args,	0),		// p4
		P4_WORD("_bp",		&&_bp,		P4_BIT_IMM),	// p4
		P4_WORD("_branch",	&&_branch,	0),		// p4
		P4_WORD("_branchz",	&&_branchz,	0),		// p4
		P4_WORD("_ds",		&&_ds,		0),		// p4
		P4_WORD("_dsp@",	&&_dsp_get,	0),		// p4
		P4_WORD("_dsp!",	&&_dsp_put,	0),		// p4
		P4_WORD("_ip",		&&_ip,		0),		// p4
		P4_WORD("_longjmp",	&&_longjmp,	0),		// p4
		P4_WORD("_rs",		&&_rs,		0),		// p4
		P4_WORD("_rsp@",	&&_rsp_get,	0),		// p4
		P4_WORD("_rsp!",	&&_rsp_put,	0),		// p4
		P4_WORD("_stack_dump",	&&_stack_dump,	0),		// p4

		/* Compiling Words */
		P4_WORD("'",		&&_tick,	0),
		P4_WORD(",",		&&_comma,	0),
		P4_WORD(":",		&&_colon,	0),
		P4_WORD(";",		&&_semicolon,	P4_BIT_IMM),
		P4_WORD(">BODY",	&&_body,	0),
		P4_WORD("COMPILE,",	&&_comma,	0),
		P4_WORD("CREATE",	&&_create,	0),
		P4_WORD("DOES>",	&&_does,	0),
		P4_WORD("EXECUTE",	&&_execute,	0),
		P4_WORD("EXIT",		&&_exit,	0),
		P4_WORD("IMMEDIATE",	&&_immediate,	P4_BIT_IMM),
		P4_WORD("LITERAL",	&&_literal,	P4_BIT_IMM),
		P4_WORD("MARKER",	&&_marker,	0),
		P4_WORD("STATE",	&&_state,	0),

		/* Numeric formatting. */
		P4_WORD("<#",		&&_pic_start,	0),
		P4_WORD("#",		&&_pic_digit,	0),
		P4_WORD("#S",		&&_pic_rest,	0),
		P4_WORD("#>",		&&_pic_end,	0),
		P4_WORD("BASE",		&&_base,	0),
		P4_WORD("HOLD",		&&_pic_hold,	0),
		P4_WORD("SIGN",		&&_pic_sign,	0),
		P4_WORD("/HOLD",	&&_pic_size,	0),

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
		P4_WORD("C,",		&&_ccomma,	0),
		P4_WORD("C@",		&&_cfetch,	0),
		P4_WORD("CMOVE",	&&_move,	0),
		P4_WORD("CMOVE>",	&&_move,	0),
		P4_WORD("CS-PICK",	&&_pick,	0),		// C: on data stack
		P4_WORD("CS-ROLL",	&&_roll,	0),		// C: on data stack
		P4_WORD("DROP",		&&_drop,	0),
		P4_WORD("DUP",		&&_dup,		0),
		P4_WORD("LLOR",		&&_llor,	0),		// p4
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
		P4_WORD("NEGATE",	&&_neg,		0),
		P4_WORD("OR",		&&_or,		0),
		P4_WORD("RSHIFT",	&&_rshift,	0),
		P4_WORD("SM/REM",	&&_sm_div_rem,	0),
		P4_WORD("XOR",		&&_xor,		0),

		/* Comparisons */
		P4_WORD("0=",		&&_eq0,		0),
		P4_WORD("0<",		&&_lt0,		0),
		P4_WORD("U<",		&&_u_lt,	0),

		/* I/O */
		P4_WORD(">IN",		&&_input_offset,0),
		P4_WORD("BLK",		&&_blk,		0),
		P4_WORD("BLOCK",	&&_block,	0),
		P4_WORD("BUFFER",	&&_buffer,	0),
		P4_WORD("DUMP",		&&_dump,	0),
		P4_WORD("EMIT",		&&_emit,	0),
		P4_WORD("EMPTY-BUFFERS", &&_empty_buffers, 0),
		P4_WORD("INCLUDED",	&&_included,	0),
		P4_WORD("KEY",		&&_key,		0),
		P4_WORD("KEY?",		&&_key_ready,	0),
		P4_WORD("LOAD",		&&_load,	0),
		P4_WORD("MS",		&&_ms,		0),
		P4_WORD("PARSE",	&&_parse,	0),
		P4_WORD("PARSE-ESCAPE",	&&_parse_escape,0),
		P4_WORD("PARSE-NAME",	&&_parse_name,	0),
		P4_WORD("REFILL",	&&_refill,	0),
		P4_WORD("SAVE-BUFFERS",	&&_save_buffers, 0),
		P4_WORD("SOURCE",	&&_source,	0),
		P4_WORD("SOURCE-ID",	&&_source_id,	0),
		P4_WORD("UPDATE",	&&_update,	0),

		/* Tools*/
		P4_WORD("SEE",		&&_see,		P4_BIT_IMM),
		P4_WORD("WORDS",	&&_words,	0),

		P4_WORD(NULL,		NULL,		0),
	};

	if (is_executing) {
		goto _execute;
	}
	if (p4_bultin_words == NULL) {
		/* Link up the base dictionary. */
		for (word = words; word->code != NULL; word++) {
			word[1].prev = word;
		}
		p4_bultin_words = word->prev;
	}
	if (ctx->words == NULL) {
		ctx->words = p4_bultin_words;
		if (*options.core_file != '\0' && p4LoadCore(ctx, options.core_file)) {
			return P4_THROW_EIO;
		}
	}

#define NEXT	goto _next

	for (;;) {
		if (ctx->state == P4_STATE_INTERPRET && is_tty && P4_INPUT_IS_TERM(ctx->input)) {
			(void) fputs("ok ", stdout);
			(void) fflush(stdout);
		}
		if (p4Refill(ctx, &ctx->input) == 0) {
			break;
		}
	_repl:
		while (ctx->input.offset < ctx->input.length) {
			str = p4ParseName(&ctx->input);
			if (str.length == 0) {
				break;
			}
			word = p4FindWord(ctx, str.string, str.length);
			if (word == NULL) {
				int offset = 0;
				int radix = ctx->radix;

				switch (str.string[0]) {
				case '$':	/* $F9 hex */
					radix = 16;
					offset++;
					break;
				case '#':	/* #99 decimal */
					radix = 10;
					offset++;
					break;
				case '%':	/* %1011 binary */
					radix = 2;
					offset++;
					break;
				case '0':	/* 0377 octal or 0xFF hex */
					if (2 < str.length && str.string[1] == 'x') {
						radix = 16;
						offset += 2;
					} else {
						radix = 8;
					}
					break;
				case '\'':	/* 'c' and '\x' escaped characters */
					if (str.length == 3 && str.string[2] == '\'') {
						x.u = str.string[1];
						goto compile_or_push;
					}
					/* Extension C style backslash literals */
					if (str.length == 4 && str.string[1] == '\\' && str.string[3] == '\'') {
						x.n = p4CharLiteral(str.string[2]);
						goto compile_or_push;
					}
				}

				char *stop;
				x.n = p4StrToInt(str.string + offset, &stop, radix);
				if (stop - str.string != str.length) {
					/* Not a word, not a number. */
					(void) printf("\"%.*s\" ", (int)str.length, str.string);
					if (ctx->state == P4_STATE_COMPILE) {
						LONGJMP(ctx->on_throw, P4_THROW_UNDEFINED);
					}
					(void) printf("?\r\n");
					continue;
				}

			compile_or_push:
				if (ctx->state == P4_STATE_COMPILE) {
#ifdef CODE_FIELD
					ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) &w_lit.code);
#else
					ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) &w_lit);
#endif
					ctx->words = p4WordAppend(ctx, ctx->words, x);
				} else {
					P4_PUSH(ctx->ds, x);
					p4StackCheck(ctx);
				}
			} else if (ctx->state == P4_STATE_COMPILE && !P4_WORD_IS_IMM(word)) {
#ifdef CODE_FIELD
				ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) &word->code);
#else
				ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) word);
#endif
			} else {
				// Setup XT of word found to execute.
				P4_PUSH(ctx->ds, word);
				ip = exec;
				goto _execute;
			}
		}
	}
	if (ctx->state == P4_STATE_INTERPRET && is_tty && P4_INPUT_IS_TERM(ctx->input)) {
		(void) fputc('\n', stdout);
	}
	return P4_THROW_OK;

	/*
	 * Flow control.
	 */
	_next: {	// Indirect threading.
		p4StackCheck(ctx);
		w = *ip++;
#ifdef CODE_FIELD
		goto **w.xt;
#else
		goto *w.xt->code;
#endif
	}
	_execute: {	// ( xt -- )
		w = P4_POP(ctx->ds);
#ifdef CODE_FIELD
		goto **w.xt;
#else
		goto *w.xt->code;
#endif
	}
	_bp: {	// ( -- )
		p4Bp(ctx);
		NEXT;
	}
	_chars: {	// ( n1 -- n2 )
		P4_TOP(ctx->ds).n *= sizeof (P4_Char);
		NEXT;
	}
	_cells: {	// ( n1 -- n2 )
		P4_TOP(ctx->ds).n *= sizeof (P4_Cell);
		NEXT;
	}
	_branch: {	// ( -- ) relative offset in address units
		w = *ip;
		ip = (P4_Cell *)((P4_Char *) ip + w.n);
		NEXT;
	}
	_branchz: {	// ( flag -- ) relative offset in address units
		w = *ip;
		x = P4_POP(ctx->ds);
		ip = (P4_Cell *)((P4_Char *) ip + (x.u == 0 ? w.n : sizeof (P4_Cell)));
		NEXT;
	}
	_ip:		// ( -- addr )
		P4_PUSH(ctx->ds, (P4_Cell *) &ip);
		NEXT;
	_dsp_get:	// ( -- aaddr )
		w.p = ctx->ds.top;
		P4_PUSH(ctx->ds, w);
		NEXT;
	_dsp_put:	// ( aaddr -- )
		w = P4_POP(ctx->ds);
		ctx->ds.top = w.p;
		NEXT;
	_rsp_get:	// ( -- aaddr )
		w.p = ctx->rs.top;
		P4_PUSH(ctx->ds, w);
		NEXT;
	_rsp_put:	// ( aaddr -- )
		w = P4_POP(ctx->ds);
		ctx->rs.top = w.p;
		NEXT;
	_longjmp:	// ( n -- )
		w = P4_POP(ctx->ds);
		LONGJMP(ctx->on_throw, (int) w.n);

	/* LITERAL (S: x -- )
	 *
	 *	: name ... [ x ] LITERAL ... ;
	 */
	_literal: {
		w = P4_POP(ctx->ds);
#ifdef CODE_FIELD
		ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) &w_lit.code);
#else
		ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) &w_lit);
#endif
		ctx->words = p4WordAppend(ctx, ctx->words, w);
		NEXT;
	}
	_lit: {		// ( -- x )
		w = *ip++;
		P4_PUSH(ctx->ds, w);
		NEXT;
	}

	/*
	 * Defining words.
	 */
	_colon: {	// (R: -- ip)
		if (ctx->state == P4_STATE_COMPILE) {
			LONGJMP(ctx->on_throw, P4_THROW_COMPILING);
		}
		str = p4ParseName(&ctx->input);
		word = p4WordCreate(ctx, str.string, str.length, &&_enter);
		ctx->state = P4_STATE_COMPILE;
		word->prev = ctx->words;
		ctx->words = word;
		NEXT;
	}
 	_enter: {	// ( i*x -- j*y )(R: -- ip)
		P4_PUSH(ctx->rs, ip);
#ifdef CODE_FIELD
		ip = w.p + 1;
#else
		ip = w.xt->data;
#endif
		NEXT;
	}
	_semicolon: {	// (C: colon -- )
		ctx->state = P4_STATE_INTERPRET;
#ifdef CODE_FIELD
		ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) &w_exit.code);
#else
		ctx->words = p4WordAppend(ctx, ctx->words, (P4_Cell) &w_exit);
#endif
		NEXT;
	}
	_exit: {	// ( i*x -- i*x )(R:ip -- )
		ip = P4_POP(ctx->rs).p;
		NEXT;
	}
	_immediate: {	// ( -- )
		P4_WORD_SET_IMM(ctx->words);
		NEXT;
	}
	_marker: {
		str = p4ParseName(&ctx->input);
		word = p4WordCreate(ctx, str.string, str.length, &&_rm_marker);
		word->prev = ctx->words;
		ctx->words = word;
		NEXT;
	}
	_rm_marker: {
#ifdef CODE_FIELD
		x.w = (P4_Word *)(w.s - offsetof(P4_Word, code));
#else
		x.w = w.xt;
#endif
		for (word = ctx->words; word != x.w; word = w.w) {
			w.w = word->prev;
			p4WordFree(word);
		}
		ctx->words = word->prev;
		p4WordFree(word);
		NEXT;
	}

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
	 *	a b c word1 word2 ...
	 *	x y z word2 ...
	 */
	_create: {	// ( -- addr )
		str = p4ParseName(&ctx->input);
		word = p4WordCreate(ctx, str.string, str.length, &&_data_field);
		P4_WORD_SET_CREATED(word);
		word->prev = ctx->words;
		ctx->words = word;
		// Reserve the first cell for a code pointer for DOES>.
		word->ndata += sizeof (P4_Cell);
		NEXT;
	}
	_data_field: {	// ( -- addr )
#ifdef CODE_FIELD
		P4_PUSH(ctx->ds, w.p + 2);
#else
		P4_PUSH(ctx->ds, w.xt->data + 1);
#endif
		NEXT;
	}
	_does: {	// DOES>
		word = ctx->words;
		if (P4_WORD_WAS_CREATED(word)) {
			word->code = &&_do_does;
			// New word's code follows DOES> of the defining word.
			word->data[0].p = ip;
			goto _exit;
		}
		LONGJMP(ctx->on_throw, P4_THROW_NOT_CREATED);
	}
	_do_does: {	// ( -- addr) and chain to defining word after DOES>.
#ifdef CODE_FIELD
		P4_PUSH(ctx->ds, w.p + 2);
		P4_PUSH(ctx->rs, ip);
		ip = w.p[1].p;
#else
		P4_PUSH(ctx->ds, w.xt->data + 1);
		P4_PUSH(ctx->rs, ip);
		ip = w.xt->data[0].p;
#endif
		NEXT;
	}
	_body: {	// ( xt -- addr )
		w = P4_TOP(ctx->ds);
#ifdef CODE_FIELD
		w.w = (P4_Word *)(w.s - offsetof(P4_Word, code));
#endif
		if (P4_WORD_WAS_CREATED(w.w)) {
			P4_TOP(ctx->ds) = (P4_Cell) &w.w->data[1];
			NEXT;
		}
		LONGJMP(ctx->on_throw, P4_THROW_NOT_CREATED);
	}

	/*
	 * Compiling
	 */
	_tick: {	// ( -- xt )
		str = p4ParseName(&ctx->input);
		word = p4FindWord(ctx, str.string, str.length);
		if (word != NULL) {
#ifdef CODE_FIELD
			P4_PUSH(ctx->ds, &word->code);
#else
			P4_PUSH(ctx->ds, word);
#endif
			NEXT;
		}
		p4Bp(ctx);
		LONGJMP(ctx->on_throw, P4_THROW_UNDEFINED);
	}
	_comma: {	// ( x -- )
	_compile:	// ( xt -- )
		p4Align(ctx);
		w = P4_POP(ctx->ds);
		ctx->words = p4WordAppend(ctx, ctx->words, w);
		NEXT;
	}
	_ccomma: {	// ( char -- )
		w = P4_POP(ctx->ds);
		ctx->words = p4WordAllot(ctx, ctx->words, 1);
		((P4_Char *) ctx->words->data)[ctx->words->ndata - 1] = (P4_Char) w.u;
		NEXT;
	}
	_allot: {	// ( n -- )
		w = P4_POP(ctx->ds);
		ctx->words = p4WordAllot(ctx, ctx->words, w.n);
		NEXT;
	}
	_align: {	// ( -- )
		p4Align(ctx);
		NEXT;
	}

	/*
	 * Context variables
	 */
	_args: {// ( -- aaddr u )
		P4_PUSH(ctx->ds, (P4_Cell *) options.argv);
		P4_PUSH(ctx->ds, (P4_Int) options.argc);
		NEXT;
	}
	_state: {	// ( -- addr )
		P4_PUSH(ctx->ds, (P4_Cell *) &ctx->state);
		NEXT;
	}

	/*
	 * Numeric formatting
	 */
	_base: {	// ( -- addr )
		P4_PUSH(ctx->ds, (P4_Cell *) &ctx->radix);
		NEXT;
	}
	_pic_start: {	// ( -- )
		(void) memset(ctx->pic, ' ', sizeof (ctx->pic));
		ctx->picptr = ctx->pic;
		NEXT;
	}
	_pic_end: {	// ( x -- caddr u )
		w.u = ctx->picptr - ctx->pic;
		p4StrRev(ctx->pic, w.u);
		P4_TOP(ctx->ds).s = ctx->pic;
		P4_PUSH(ctx->ds, w);
		NEXT;
	}
	_pic_digit: {	// ( u1 -- u2 )
		if (ctx->pic + sizeof (ctx->pic) <= ctx->picptr) {
			LONGJMP(ctx->on_throw, P4_THROW_PIC_OVER);
		}
		w = P4_TOP(ctx->ds);
		*ctx->picptr++ = base36_digits[w.u % ctx->radix];
		P4_TOP(ctx->ds).u /= ctx->radix;
		NEXT;
	}
	_pic_rest: {	// ( u -- 0 )
		w = P4_TOP(ctx->ds);
		do {
			if (ctx->pic + sizeof (ctx->pic) <= ctx->picptr) {
				LONGJMP(ctx->on_throw, P4_THROW_PIC_OVER);
			}
			*ctx->picptr++ = base36_digits[w.u % ctx->radix];
			w.u /= ctx->radix;
		} while (0 < w.u);
		P4_TOP(ctx->ds).u = 0;
		NEXT;
	}
	_pic_sign: {	// ( n -- )
		if (ctx->pic + sizeof (ctx->pic) <= ctx->picptr) {
			LONGJMP(ctx->on_throw, P4_THROW_PIC_OVER);
		}
		w = P4_POP(ctx->ds);
		if (w.n < 0) {
			*ctx->picptr++ = '-';
		}
		NEXT;
	}
	_pic_hold: {	// ( ch -- )
		if (ctx->pic + sizeof (ctx->pic) <= ctx->picptr) {
			LONGJMP(ctx->on_throw, P4_THROW_PIC_OVER);
		}
		w = P4_POP(ctx->ds);
		*ctx->picptr++ = (P4_Char) w.u;
		NEXT;
	}
	_pic_size: {	// ( -- n )
		P4_PUSH(ctx->ds, sizeof(ctx->pic));
		NEXT;
	}

	/*
	 * Memory access.
	 */
	_cfetch: {	// ( caddr -- char )
		w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).u = *w.s;
		NEXT;
	}
	_cstore: {	// ( char caddr -- )
		w = P4_POP(ctx->ds);
		*w.s = P4_POP(ctx->ds).u;
		NEXT;
	}
	_fetch: {	// ( aaddr -- x )
		w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds) = *w.p;
		NEXT;
	}
	_store: {	// ( x aaddr -- )
		w = P4_POP(ctx->ds);
		x = P4_POP(ctx->ds);
		*w.p = x;
		NEXT;
	}
	_move: {	// ( addr1 addr2 u -- )
		w = P4_POP(ctx->ds);
		x = P4_POP(ctx->ds);
		(void) memmove(x.s, P4_POP(ctx->ds).s, w.u);
		NEXT;
	}

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
	_here_offset: {	// ( -- u )
		P4_PUSH(ctx->ds, ctx->words->ndata);
		NEXT;
	}
	_here_addr: {	// ( -- addr )
		P4_PUSH(ctx->ds, (P4_Char *)ctx->words->data + ctx->words->ndata);
		NEXT;
	}
	_unused: {	// ( -- u )
		w.u = ctx->words->mdata - ctx->words->ndata;
		P4_PUSH(ctx->ds, w);
		NEXT;
	}

	/*
	 * Dynamic Memory
	 */
	_allocate: {	// ( u -- aaddr ior )
		w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).s = malloc(w.u);
		P4_PUSH(ctx->ds, (P4_Int)(w.s == NULL));
		NEXT;
	}
	_free: {	// ( aaddr -- ior )
		w = P4_TOP(ctx->ds);
		free(w.s);
		P4_TOP(ctx->ds).n = 0;
		NEXT;
	}
	_resize: {	// ( aaddr1 u -- aaddr2 ior )
		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		w.s = realloc(x.s, w.u);
		P4_TOP(ctx->ds) = w.s == NULL ? x : w;
		P4_PUSH(ctx->ds, (P4_Int)(w.s == NULL));
		NEXT;
	}

	/*
	 * Stack manipulation.
	 */
	_drop: {	// ( x -- )
		P4_DROP(ctx->ds, 1);
		NEXT;
	}
	_dup: {		// ( x -- x x )
		w = P4_TOP(ctx->ds);
		P4_PUSH(ctx->ds, w);
		NEXT;
	}
	_pick: {	// ( xU ... x1 x0 -- xU ... x1 x0 xU )
			// 0 PICK == DUP, 1 PICK == OVER
		w = P4_POP(ctx->ds);
		x = P4_PICK(ctx->ds, w.u);
		P4_PUSH(ctx->ds, x);
		NEXT;
	}
	_swap: {	// ( x y -- y x )
		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds) = w;
		P4_PUSH(ctx->ds, x);
		NEXT;
	}
	_to_rs: {	// (x -- )(R: -- x )
		w = P4_POP(ctx->ds);
		P4_PUSH(ctx->rs, w);
		NEXT;
	}
	_from_rs: {	// (R: x -- )
		w = P4_POP(ctx->rs);
		P4_PUSH(ctx->ds, w);
		NEXT;
	}
	_rs_copy: {	// (R: x -- x)
		w = P4_TOP(ctx->rs);
		P4_PUSH(ctx->ds, w);
		NEXT;
	}
	_roll: {	// ( xu xu-1 ... x0 u –– xu-1 ... x0 xu )
		w = P4_POP(ctx->ds);
		x = P4_PICK(ctx->ds, w.u);
		(void) memmove(ctx->ds.top - w.u, ctx->ds.top - w.u + 1, w.u * sizeof (P4_Cell));
		P4_TOP(ctx->ds) = x;
		NEXT;
	}
	_llor: {	// ( xu xu-1 ... x0 u –– x0 xu xu-1 ... x1 )
		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		(void) memmove(ctx->ds.top - w.u + 1, ctx->ds.top - w.u, w.u * sizeof (P4_Cell));
		ctx->ds.top[-w.u] = x;
		NEXT;
	}

	/*
	 * Operators
	 */
	_add: {		// ( n1 n2 -- n3 )
		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).n += w.n;
		NEXT;
	}
	_sub: {		// ( n1 n2 -- n3 )
		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).n -= w.n;
		NEXT;
	}
	_mul: {		// ( n1 n2 -- n3 )
		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).n *= w.n;
		NEXT;
	}
	_div: {		// ( n1 n2 -- n3 )
		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).n /= w.n;
		NEXT;
	}
	_sm_div_rem: {	// ( dend dsor -- rem quot )
		// C99+ specifies symmetric division.
		ldiv_t qr;
		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		qr = ldiv(x.n, w.n);
		P4_TOP(ctx->ds).n = qr.rem;
		P4_PUSH(ctx->ds, qr.quot);
		NEXT;
	}
	_fm_div_mod: {	// ( dend dsor -- mod quot )
		P4_Int q, m;
		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		q = x.n / w.n;
		m = x.n % w.n;
		if (m != 0 && (w.n ^ x.n) < 0) {
			q -= 1;
			m += x.n;
		}
		P4_TOP(ctx->ds).n = m;
		P4_PUSH(ctx->ds, q);
		NEXT;
	}
	_mod: {		// ( n1 n2 -- n3 )
		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).n %= w.n;
		NEXT;
	}
	_and: {		// ( x1 x2 -- x3 )
		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).n &= w.n;
		NEXT;
	}
	_or: {		// ( x1 x2 -- x3 )
		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).n |= w.n;
		NEXT;
	}
	_xor: {		// ( x1 x2 -- x3 )
		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).n ^= w.n;
		NEXT;
	}
	_not: {		// ( n1 -- n2 )
		w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).n = ~w.u;
		NEXT;
	}
	_neg: {		// ( n1 -- n2 )
		w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).n = -w.n;
		NEXT;
	}
	_lshift: {	// ( x1 u -- x2 )
		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).u <<= w.u;
		NEXT;
	}
	_rshift: {	// ( x1 u -- x2 )
		w = P4_POP(ctx->ds);
		P4_TOP(ctx->ds).u >>= w.u;
		NEXT;
	}

	/*
	 * Comparision
	 */
	_eq0: {		// ( x -- flag )
		w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).u = w.u == 0 ? ~0 : 0;
		NEXT;
	}
	_lt0: {		// ( x -- flag )
		w = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).u = w.n < 0 ? ~0 : 0;
		NEXT;
	}
	_u_lt: {	// ( u1 u2 -- )
		w = P4_POP(ctx->ds);
		x = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds).u = x.u < w.u;
		NEXT;
	}

	/*
	 * I/O
	 */
	_input_offset: {// ( -- u )
		P4_PUSH(ctx->ds, (P4_Cell *) &ctx->input.offset);
		NEXT;
	}
	_source: {	// ( -- caddr u )
		P4_PUSH(ctx->ds, ctx->input.buffer + ctx->input.offset);
		P4_PUSH(ctx->ds, ctx->input.length - ctx->input.offset);
		NEXT;
	}
	_source_id: {	// ( -- -2 | -1 | 0 | fd )
		P4_PUSH(ctx->ds, ctx->input.fd);
		NEXT;
	}
	_refill: {	// ( -- flag)
		w.u = p4Refill(ctx, &ctx->input);
		P4_PUSH(ctx->ds, w);
		NEXT;
	}
	_key: {		// ( -- n )
		(void) fflush(stdout);
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
	}
	_key_ready: {	// ( -- flag )
		(void) fflush(stdout);
		if (ctx->unget == EOF) {
#ifdef HAVE_TCSETATTR
			if (tcsetattr(tty_fd, TCSANOW, &tty_raw_nb) == 0)
				ctx->unget = p4ReadByte(tty_fd);
#else
			if (p4SetNonBlocking(tty_fd, 1) == 0) {
				ctx->unget = p4ReadByte(tty_fd);
				(void) p4SetNonBlocking(tty_fd, 0);
			}
#endif
		}
		P4_PUSH(ctx->ds, (P4_Uint)(ctx->unget != EOF));
	}
	_emit: {	// ( c -- )
		w = P4_POP(ctx->ds);
		(void) fputc(w.n, stdout);
		NEXT;
	}
	_included: {	// ( caddr u -- )
		P4_DROP(ctx->ds, 1);	// ignore u, caddr is NUL terminated.
		x = P4_POP(ctx->ds);
		(void) p4EvalFile(ctx, x.s);
		NEXT;
	}

	/*
	 * Block I/O
	 */
	_blk: {		// ( -- addr )
		P4_PUSH(ctx->ds, (P4_Cell *) &ctx->block.number);
		NEXT;
	}
	_block: {	// ( u -- aaddr )
		w = P4_POP(ctx->ds);
		p4BlockGet(ctx, w.u);
		P4_PUSH(ctx->ds, ctx->block.buffer);
		NEXT;
	}
	_buffer: {	// ( u -- aaddr )
		w = P4_POP(ctx->ds);
		p4BlockBuffer(ctx, w.u);
		P4_PUSH(ctx->ds, ctx->block.buffer);
		NEXT;
	}
	_load: {	// ( u -- )
		w = P4_POP(ctx->ds);
		p4BlockLoad(ctx, w.u);
		NEXT;
	}
	_empty_buffers: { // ( -- )
		ctx->block.state = P4_BLOCK_FREE;
	}
	_save_buffers: { // ( -- )
		p4BlockBuffer(ctx, ctx->block.number);
		NEXT;
	}
	_update: {	// ( -- )
		ctx->block.state = P4_BLOCK_DIRTY;
		NEXT;
	}

	/*
	 */
	_parse: {	// ( char -- c-addr u )
		w = P4_POP(ctx->ds);
		str = p4Parse(&ctx->input, w.u, 0);
		P4_PUSH(ctx->ds, str.string);
		P4_PUSH(ctx->ds, str.length);
		NEXT;
	}
	_parse_escape: {	// ( char -- c-addr u )
		w = P4_POP(ctx->ds);
		str = p4Parse(&ctx->input, w.u, 1);
		P4_PUSH(ctx->ds, str.string);
		P4_PUSH(ctx->ds, str.length);
		NEXT;
	}
	_parse_name: {	// ( -- c-addr u )
		str = p4ParseName(&ctx->input);
		P4_PUSH(ctx->ds, str.string);
		P4_PUSH(ctx->ds, str.length);
		NEXT;
	}
	_ms: {		// ( ms -- )
		w = P4_POP(ctx->ds);
		p4Nap(w.u / 1000L, (w.u % 1000L) * 1000000L);
		NEXT;
	}

	/*
	 * Tools
	 */
	_ds: {	// ( -- aaddr u )
		w.u = P4_LENGTH(ctx->ds);
		P4_PUSH(ctx->ds, ctx->ds.base);
		P4_PUSH(ctx->ds, w);
		NEXT;
	}
	_rs: {	// ( -- aaddr u )
		w.u = P4_LENGTH(ctx->rs);
		P4_PUSH(ctx->ds, ctx->rs.base);
		P4_PUSH(ctx->ds, w);
		NEXT;
	}
	_stack_dump: {	// ( addr u -- )
		x = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		p4StackDump(stdout, w.p, x.u);
		NEXT;
	}
	_dump: {	// ( addr u -- )
		x = P4_POP(ctx->ds);
		w = P4_POP(ctx->ds);
		p4MemDump(stdout, w.s, x.u);
		NEXT;
	}
	_see: {		// ( -- )
		str = p4ParseName(&ctx->input);
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
#ifdef CODE_FIELD
			for (w.p = word->data; w.w->code != &&_exit; w.p++) {
				x.w = (P4_Word *)(w.s - offsetof(P4_Word, code));
#else
			for (w.p = word->data; w.p->xt != &w_exit; w.p++) {
				x = *w.p;
#endif
				if (x.w->code == &&_lit) {
					P4_Word *xt_word = p4FindXt(ctx, w.p[1].xt);
					if (xt_word == NULL) {
						(void) printf("_lit "P4_INT_FMT" ", (*++w.p).n);
					} else {
						(void) printf("['] %.*s ", (int)xt_word->name.length, xt_word->name.string);
						w.p++;
					}
					continue;
				}
				(void) printf(
					"%s%.*s ",
					P4_WORD_IS_IMM(x.w) ? "POSTPONE " : "",
					(int) x.w->name.length, x.w->name.string
				);
				if (x.w->code == &&_branch || x.w->code == &&_branchz) {
					(void) printf(P4_INT_FMT" ", (*++w.p).n);
				}
			}
			(void) printf("; %s\r\n", P4_WORD_IS_IMM(word) ? "IMMEDIATE" : "");
		} else if (word->code == &&_do_does) {
			// Dump word's data.
			for (w.u = 1; w.u < word->ndata; w.u++) {
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
		} else {
			(void) printf("(unknown) 0x%p ", word->code);
		}
		NEXT;
	}
	_words: {	// ( -- )
		P4_Uint column = 0;
		unsigned short window[4] = { 24, 80 };
#ifdef TIOCGWINSZ
		ioctl(0, TIOCGWINSZ, window);
#endif
		for (word = ctx->words; word != NULL; word = word->prev) {
			if (window[1] <= column + word->name.length + 1) {
				(void) fputc('\n', stdout);
				column = 0;
			}
			column += fprintf(stdout, "%s ", word->name.string);
		}
		(void) fputc('\n', stdout);
		NEXT;
	}
}

int
p4Eval(P4_Ctx *ctx)
{
	int rc;
	P4_Word *word;

	SETJMP_PUSH(ctx->on_throw);
	signal_ctx = ctx;

	switch (rc = SETJMP(ctx->on_throw)) {
	case P4_THROW_BYE:
		rc = 0;
		break;

	default:
	case P4_THROW_ABORT:
	case P4_THROW_ABORT_MSG:
		(void) printf("%d thrown: %s", rc, P4_THROW_future < rc && rc < 0 ? p4_exceptions[-rc] : "?");
		if (ctx->state == P4_STATE_COMPILE) {
			/* A thrown error while compiling a word leaves the
			 * definition in an incomplete state; discard it.
			 */
			word = ctx->words;
			(void) printf(
				" while compiling \"%s\"",
				word->name.length == 0 ? ":NONAME" : (char *)word->name.string
			);
			ctx->words = word->prev;
			p4WordFree(word);
		}
		(void) fputc('\n', stdout);
		(void) fflush(stdout);
		P4_RESET(ctx->ds);
		last_signal = 0;
		/*@fallthrough@*/

	case P4_THROW_QUIT:
		P4_RESET(ctx->rs);
		if (STDIN_FILENO < ctx->input.fd) {
			(void) fclose(ctx->input.fp);
			break;
		}
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

		rc = p4Repl(ctx, 0);
	}

	SETJMP_POP(ctx->on_throw);

	return rc;
}

int
p4EvalFile(P4_Ctx *ctx, const char *file)
{
	int rc;

	if (ctx == NULL || file == NULL) {
		errno = EFAULT;
		return P4_THROW_EFAULT;
	}

	P4_INPUT_PUSH(&ctx->input);

	if ((ctx->input.fp = fopen(file, "r")) == NULL) {
		rc = errno == ENOENT ? P4_THROW_ENOENT : P4_THROW_EIO;
	} else {
		rc = p4Eval(ctx);
		(void) fclose(ctx->input.fp);
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
