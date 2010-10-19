/*
 * p4.c
 *
 * Copyright 2007, 2010 by Anthony Howe. All rights reserved.
 */

#include "config.h"
#include "p4.h"

/***********************************************************************
 *** Globals
 ***********************************************************************/

P4_Cell p4_null_cell;
static void *p4_program_end;
static unsigned char base36[256];
static char base36_digits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

#ifdef HAVE_TCGETATTR
static int tty_fd = -1;
static struct termios tty_raw;
static struct termios tty_raw_nb;
static struct termios tty_saved;
#endif

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
	">BODY used on non-CREATEd definition",
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
	NULL
};

/***********************************************************************
 *** Conversion API
 ***********************************************************************/

static void
error_abort(int lineno)
{
	fprintf(stderr, "%s(%d) %s (%d)\n", __FILE__, lineno, strerror(errno), errno);
	abort();
}

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
	case '?': return '\177';	/* delete */
	}

	return ch;			/* identity */
}

/**
 * @param value
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
 *	Length of string in buffer.
 *
 * @example
 *
 * 	The following representations all represent the same number:
 *
 *	base 16	DEADBEEF
 *	base 10	-559038737
 * 	base 8	33653337357
 *	base 2	1101 1110 1010 1101 1011 1110 1110 1111 (spaces added for clarity)
 */
static P4_Size
p4_unsigned_to_string(P4_Unsigned value, P4_Unsigned base, P4_Byte *buffer, P4_Size size)
{
	P4_Size i;

	if (size == 0)
		return 0;

	i = 0;
	size--;

	if (i < size) {
		buffer[i++] = base36_digits[value % base];
		value /= base;

		while (0 < value && i < size) {
			ldiv_t qr = ldiv(value, base);
			buffer[i++] = base36_digits[qr.rem];
			value = qr.quot;
		}
	}

	buffer[i] = '\0';

	return i;
}

P4_Size
p4UnsignedToString(P4_Unsigned number, P4_Unsigned base, P4_Byte *buffer, P4_Size size)
{
	P4_Size length;

	length = p4_unsigned_to_string(number, base, buffer, size);
	p4StringReverse(buffer, length);

	return length;
}

P4_Size
p4SignedToString(P4_Signed number, P4_Unsigned base, P4_Byte *buffer, P4_Size size)
{
	int sign = 1;
	P4_Unsigned value;
	P4_Size length = 0;

	value = (P4_Unsigned) number;

	if (base == 10 && number < 0) {
		/* Negating a long can overflow on a two's complement
		 * machine. Instead convert to unsigned long and negate
		 * the new form, which cannot overflow.  So long as an
		 * arbitrary unsigned long can be safely converted to
		 * long and back again, this works fine.
		 */
		value = -value;
		sign = -1;
	}

	length = p4_unsigned_to_string(value, base, buffer, size);
	if (sign == -1 && length < size)
		buffer[length++] = '-';
	p4StringReverse(buffer, length);
	buffer[length] = '\0';

	return length;
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
P4_Signed
p4StringToSigned(const P4_Byte *s, P4_Byte **stop, P4_Unsigned base)
{
	P4_Signed num;
	int digit, sign = 1;

	if (s == NULL)
		return 0;

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

	if (stop != NULL)
		*stop = (char *) s;

	return sign * num;
}

/**
 * @param s
 *	A pointer to a C string to reverse in place.
 *
 * @param length
 *	The length of the C string.
 */
void
p4StringReverse(P4_Byte *s, P4_Size length)
{
	P4_Byte ch, *x, *y;

	/* Reverse segment of string. */
	for (x = s, y = s+length; x < --y; x++) {
		ch = *y;
		*y = *x;
		*x = ch;
	}
}

/**
 * @param s
 *	A pointer to a C string to convert in place to upper case.
 */
void
p4StringToUpper(P4_Byte *s, P4_Size length)
{
	for ( ; 0 < length && *s != '\0'; length--, s++)
		*s = toupper(*s);
}

/***********************************************************************
 *** Contiguous Memory Array API
 ***********************************************************************/

void
p4ArrayRelease(P4_Array *table)
{
	if (table != NULL)
		free(table->base);
}

void
p4ArrayFree(void *_table)
{
	P4_Array * table = _table;

	if (table != NULL) {
		p4ArrayRelease(table);
		free(table);
	}
}

int
p4ArrayAssign(P4_Array *table, size_t size)
{
	if ((table->base = malloc(size * sizeof (*table->base))) == NULL)
		return -1;

	table->size = size;
	P4_RESET_(table);

	return 0;
}

P4_Array *
p4ArrayCreate(size_t size)
{
	P4_Array *table;

	if ((table = malloc(sizeof (*table))) != NULL) {
		if (p4ArrayAssign(table, size)) {
			free(table);
			table = NULL;
		}
	}

	return table;
}

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
void
p4ArrayGrow(P4_Array *table, size_t count, size_t extra)
{
	P4_Cell *replace;
	P4_Unsigned depth;

	if (table->size < (depth = P4_LENGTH_(table)) + count) {
		table->size += count + extra;

		if ((replace = realloc(table->base, table->size * P4_CELL)) == NULL)
			error_abort(__LINE__);

		table->base = replace;
		P4_SET_DEPTH_(table, depth);
	}
}

void
p4ArrayPick(P4_Array *table, P4_Unsigned index)
{
	P4_Cell cell = index < P4_LENGTH_(table) ? P4_PEEK_(table, -index) : p4_null_cell;
	P4_PUSH_SAFE_(table) = cell;
}

void
p4ArrayRoll(P4_Array *table, P4_Unsigned count)
{
	P4_Cell cell = P4_PEEK_(table, -count);
	memmove(table->top - count, table->top - count + 1, count * P4_CELL);
	P4_TOP_(table) = cell;
}

void
p4ArrayDump(P4_Array *table, FILE *fp)
{
	P4_Cell *top;
	unsigned count;

	for (count = 0, top = table->top; table->base <= top; top--) {
		if ((count & 3) == 0)
			fprintf(fp, "+%.2u  ", count);
		fprintf(fp, P4_POINTER_FMT " ", (unsigned long) top->p);
		if ((++count & 3) == 0)
			fputc('\n', fp);
	}

	if ((count & 3) != 0)
		fputc('\n', fp);
	fprintf(fp, "+%.2u\n", count);
}

/***********************************************************************
 ***
 ***********************************************************************/

void
p4Dump(FILE *fp, P4_Byte *addr, P4_Size length)
{
	P4_Byte *s, *u;
	unsigned count;

	s = addr;
	for (count = 0; count < length; addr++) {
		if ((count & 0xF) == 0) {
			fprintf(fp, "0x%.8lx ", (unsigned long) addr);
			s = addr;
		}

		fprintf(fp, " %.2x", *addr);
		count++;

		if ((count & 0x3) == 0)
			fputc(' ', fp);

		if ((count & 0xF) == 0) {
			fputc(' ', fp);
			for (u = s; s < addr; s++)
				fputc(isprint(*s) ? *s : '.', fp);
			fputc('\n', fp);

		}
	}

	if ((count & 0xF) != 0) {
		do {
			fputs("   ", fp);
			count++;
			if ((count & 0x3) == 0)
				fputc(' ', fp);
		} while ((count & 0xF) != 0);

		fputc(' ', fp);
		for (u = s; s < addr; s++)
			fputc(isprint(*s) ? *s : '.', fp);
		fputc('\n', fp);
	}
}

P4_Word *
p4FindXt(P4_Context *ctx, P4_Exec_Token xt)
{
	P4_Word *word;

	for (word = ctx->words; word != NULL; word = word->prev) {
		if (word->xt == xt)
			return word;
	}

	return NULL;
}

P4_Signed
p4IsNoname(P4_Context *ctx, P4_Exec_Token xt)
{
	P4_Cell *cell;

	for (cell = ctx->noname.base; cell <= ctx->noname.top; cell++) {
		if (cell->xt == xt)
			return 1;
	}

	return 0;
}

P4_WORD_DECL(extern, _does);
P4_WORD_DECL(extern, _exit);
P4_WORD_DECL(extern, _lit);
P4_WORD_DECL(extern, _var);
P4_WORD_DECL(extern, JUMP);
P4_WORD_DECL(extern, JUMPZ);
P4_WORD_DECL(extern, BRANCH);
P4_WORD_DECL(extern, BRANCHZ);
P4_WORD_DECL(extern, THROW);

void
p4See(FILE *fp, P4_Context *ctx, P4_Exec_Token xt)
{
	P4_Byte *s;
	P4_Exec_Token *ip;
	P4_Word *word, *found;

	if (xt == NULL)
		return;

	word = p4FindXt(ctx, xt);

	if (xt->data == NULL) {
		if (word == NULL )
			fprintf(fp, P4_POINTER_FMT " ", (unsigned long) xt);
		else
			fprintf(fp, "%s\n", word->name.string);
		return;
	} else if (xt->code == p4_do__var) {
		fprintf(fp, "var 0x%.8lx (%ld) ", (P4_Unsigned) xt->data->base, *(P4_Signed *) xt->data->base);
		return;
	}

	if (word == NULL)
		fputs(":NONAME ", fp);
	else
		fprintf(fp, ": %s ", word->name.string);

	for (ip = (P4_Exec_Token *) xt->data->base; *ip != P4_WORD_XT(_exit); ip++) {
		found = p4FindXt(ctx, *ip);

		if (found == NULL) {
			if (p4IsNoname(ctx, *ip)) {
				p4See(fp, ctx, *ip);
			} else {
				s = (P4_Byte *) ip;
				fprintf(fp, "0x%.2x 0x%.2x 0x%.2x 0x%.2x ", s[0], s[1], s[2], s[3]);
			}
		} else if (P4_WORD_IS_IMM(found)) {
			fprintf(fp, "POSTPONE %s ", found->name.string);
		} else if (*ip == P4_WORD_XT(JUMP)) {
			fprintf(fp, "%s 0x%.8lx ", found->name.string, (P4_Unsigned) *++ip);
			ip = (P4_Exec_Token *) *ip - 1;
		} else if (*ip == P4_WORD_XT(BRANCH) || *ip == P4_WORD_XT(BRANCHZ) || strcasecmp(found->name.string, "SLIT") == 0) {
			fprintf(fp, "%s %ld ", found->name.string, (P4_Signed) *++ip);
		} else if (*ip == P4_WORD_XT(_does)) {
			fprintf(fp, "DOES> ");
			ip++;
		} else if (*ip == P4_WORD_XT(_lit)) {
			if ((found = p4FindXt(ctx, *++ip)) == NULL)
				fprintf(fp, "lit 0x%lx ", (P4_Unsigned) *ip);
			else
				fprintf(fp, "['] %s ", found->name.string);
		} else {
			fprintf(fp, "%s ", found->name.string);
		}
	}
	fputs("; ", fp);

	if (word != NULL && P4_WORD_IS_IMM(word))
		fputs("IMMEDIATE", fp);
	fputc('\n', fp);
}

P4_Word *
p4FindWord(P4_Context *ctx, P4_Byte *caddr, P4_Size length)
{
	P4_Word *word;

	if (caddr != NULL) {
		for (word = ctx->words; word != NULL; word = word->prev) {
			if (word->name.length == length && strncmp(word->name.string, caddr, length) == 0) {
				for ( ; word->bits & P4_BIT_ALIAS; word = word->prev)
					;
				return word;
			}
		}

		for (word = ctx->words; word != NULL; word = word->prev) {
			if (word->name.length == length && strncasecmp(word->name.string, caddr, length) == 0) {
				for ( ; word->bits & P4_BIT_ALIAS; word = word->prev)
					;
				return word;
			}
		}
	}

	return NULL;
}

P4_String
p4Parse(P4_Input *input, P4_Unsigned delim, P4_Unsigned escape)
{
	P4_Signed ch;
	P4_String parsed;
	P4_Unsigned offset;

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
		if (ch == delim || (delim == ' ' && isspace(ch)))
			break;
	}

	parsed.length = offset - input->offset;
	input->offset = offset + (offset < input->length);

	return parsed;
}

P4_String
p4ParseWord(P4_Input *input)
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
p4Nap(P4_Unsigned seconds, P4_Unsigned nanoseconds)
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
p4Throw(P4_Context *ctx, P4_Signed exception)
{
	if (exception != 0) {
		if ((ctx->jmp_set & P4_JMP_THROW) && exception != P4_THROW_START)
			LONGJMP(ctx->on_throw, exception);

		if (exception < 0
		&& exception != P4_THROW_ABORT
		&& exception != P4_THROW_QUIT
		&& exception != P4_THROW_START) {
			printf("%ld thrown: %s\n", exception, exception < 0 ? p4_exceptions[-exception] : "");
			if (ctx->state) {
				printf("compiling %s\n", (ctx->word == NULL) ? ":NONAME" : (char *)ctx->word->name.string);
			} else if (ctx->ip != NULL) {
				P4_Word *word = p4FindXt(ctx, ctx->ip[-1]);
				printf("executing %s\n", (word == NULL) ? ":NONAME" : (char *)word->name.string);
			}
		}

		LONGJMP(ctx->on_abort, exception);
	}
}

void
p4OnSignal(P4_Context *ctx)
{
	switch (ctx->signal) {
	case SIGINT:
		ctx->signal = 0;
		p4Throw(ctx, P4_THROW_USER);

	case SIGQUIT:
		LONGJMP(ctx->on_abort, P4_ABORT_BYE);

	case SIGSEGV:
		fputs("segmentation fault\n", stdout);
		LONGJMP(ctx->on_abort, P4_THROW_ABORT_MSG);
	}
}

/***********************************************************************
 *** Input / Ouput
 ***********************************************************************/

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

P4_Signed
p4ReadByte(int fd)
{
	unsigned char ch;

	if (read(fd, &ch, sizeof (ch)) != sizeof (ch))
		return EOF;

	return ch;
}

P4_Signed
p4GetC(P4_Context *ctx)
{
	if (ctx->input.fd == P4_INPUT_STR)
		return ctx->input.offset < ctx->input.length ? ctx->input.buffer[ctx->input.offset++] : EOF;

	if (ctx->input.fp != NULL)
		return fgetc(ctx->input.fp);

	return p4ReadByte(ctx->input.fd);
}

P4_Unsigned
p4GetLine(P4_Context *ctx, P4_Byte *line, P4_Size size)
{
	P4_Signed ch;
	P4_Unsigned i;

	if (line == NULL || size == 0)
		return 0;

#ifdef HAVE_TCSETATTR
	/* For a terminal restore original line input and echo settings. */
	if (tty_fd != 0 && P4_INPUT_IS_TERM(ctx))
		(void) tcsetattr(tty_fd, TCSADRAIN, &tty_saved);
#endif
	for (i = 0; i < size; ) {
		if ((ch = p4GetC(ctx)) == EOF)
			break;

		line[i++] = (P4_Byte) ch;

		if (ch == '\n')
			break;
	}

	return i;
}

#ifdef NOT_USED
P4_Unsigned
p4InputLine(FILE *fp, P4_Byte *line, P4_Size size)
{
	P4_Signed i;

	if (line == NULL || size == 0)
		return 0;

	for (i = 0, --size; i < size; ) {
		line[i] = (P4_Byte) fgetc(fp);

		if (feof(fp) || ferror(fp))
			break;

		if (line[i++] == '\n')
			break;
	}

	line[i] = '\0';

	return i;
}
#endif

/***********************************************************************
 *** Block I/O
 ***********************************************************************/

int
p4BlockGrow(int fd, P4_Unsigned block)
{
	size_t n;
	struct stat sb;
	unsigned char blanks[P4_BLOCK_SIZE];

	if (fstat(fd, &sb))
		return -1;

	/* Is the file large enough to contain the requested block? */
	if (sb.st_size < block * P4_BLOCK_SIZE) {
		if (lseek(fd, 0, SEEK_END) == (off_t) -1)
			return -1;

		memset(blanks, ' ', sizeof (blanks));

		/* P4_BLOCK_SIZE is a power of 2. */
		if ((n = (sb.st_size & (P4_BLOCK_SIZE-1))) != 0) {
			/* Extend the file to a multiple of block size. */
			if (write(fd, blanks, P4_BLOCK_SIZE - n) != P4_BLOCK_SIZE - n)
				return -1;

			sb.st_size = sb.st_size - n + P4_BLOCK_SIZE;
		}

		/* Extend the file with blank blocks. */
		for (n = sb.st_size / P4_BLOCK_SIZE; n < block; n++) {
			if (write(fd, blanks, P4_BLOCK_SIZE) != P4_BLOCK_SIZE)
				return -1;
		}
	}

	if (lseek(fd, (block - 1) * P4_BLOCK_SIZE, SEEK_SET) == (off_t) -1)
		return -1;

	return 0;
}

int
p4BlockRead(int fd, P4_Unsigned number, P4_Block *block)
{
	if (fd <= 0 || number == 0 || block == NULL)
		return 0;

	if (p4BlockGrow(fd, number))
		return -1;

	if (read(fd, block->buffer, P4_BLOCK_SIZE) != P4_BLOCK_SIZE)
		return -1;

	block->state = P4_BLOCK_CLEAN;
	block->number = number;

	return 0;
}

int
p4BlockWrite(int fd, P4_Block *block)
{
	if (fd <= 0 || block == NULL)
		return 0;

	if (p4BlockGrow(fd, block->number))
		return -1;

	if (write(fd, block->buffer, P4_BLOCK_SIZE) != P4_BLOCK_SIZE)
		return -1;

	block->state = P4_BLOCK_CLEAN;

	return 0;
}

/***********************************************************************
 *** Core Words
 ***********************************************************************/

/**
 * ... NOOP ...
 *
 * (S: -- )
 *
 * @standard extension
 */
P4_WORD_DEFINE(NOOP)
{
	/* Do nothing. */
}

#ifdef P4_DEFINE_CS
P4_DEFINE_CS(NOOP, "NOOP");
struct p4_xt p4_xt_NOOP = { p4_do_NOOP, NULL };
P4_Word p4_word_NOOP = { 0, (P4_CountedString *) &p4_cs_NOOP, NULL, P4_WORD_XT(NOOP) };
#else
struct p4_xt p4_xt_NOOP = { p4_do_NOOP, NULL };
P4_Word p4_word_NOOP = { 0, { sizeof ("NOOP")-1, "NOOP" }, NULL, P4_WORD_XT(NOOP) };
#endif

/**
 * ... TRUE ...
 *
 * (S: -- flag)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(TRUE)
{
	P4_PUSH_SAFE(ctx->ds).n = ~0;
}

/**
 * ... FALSE ...
 *
 * (S: -- flag)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(FALSE)
{
	P4_PUSH_SAFE(ctx->ds).n = 0;
}

/**
 * ... char PARSE ...
 *
 * (S: char "ccc<char>" -- c-addr u )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(PARSE)
{
	P4_String parsed = p4Parse(&ctx->input, P4_POP_SAFE(ctx->ds).u, 0);
	P4_PUSH_SAFE(ctx->ds).s = parsed.string;
	P4_PUSH(ctx->ds).u = parsed.length;
}

/**
 * ... char PARSE-ESCAPE ...
 *
 * (S: char "ccc<char>" -- c-addr u )
 *
 * @standard extension
 */
P4_WORD_DEFINE(PARSE_ESCAPE)
{
	P4_String parsed = p4Parse(&ctx->input, P4_POP_SAFE(ctx->ds).u, 1);
	P4_PUSH_SAFE(ctx->ds).s = parsed.string;
	P4_PUSH(ctx->ds).u = parsed.length;
}

/**
 * ... PARSE-WORD ...
 *
 * (S: <spaces>name -- c-addr u )
 *
 * @standard extension
 */
P4_WORD_DEFINE(PARSE_WORD)
{
	P4_String parsed = p4ParseWord(&ctx->input);
	P4_PUSH_SAFE(ctx->ds).s = parsed.string;
	P4_PUSH(ctx->ds).u = parsed.length;
}

/**
 * ... FIND-WORD ...
 *
 * (S: c-addr u -- c-addr u 0 | xt 1 | xt -1 )
 *
 * @standard extension
 */
P4_WORD_DEFINE(FIND_WORD)
{
	P4_Word *word;

	if ((word = p4FindWord(ctx, P4_PEEK(ctx->ds, -1).s, P4_TOP(ctx->ds).u)) == NULL) {
		P4_PUSH_SAFE(ctx->ds).n = 0;
	} else {
		P4_POKE(ctx->ds, -1).xt = word->xt;
		P4_TOP(ctx->ds).n = P4_WORD_IS_IMM(word) ? 1 : -1;
	}
}

/**
 * ... milliseconds MS ...
 *
 * (S: u -- )
 *
 * @standard ANS-Forth 1994, Facility
 */
P4_WORD_DEFINE(MS)
{
	P4_Unsigned s, ns, ms = P4_POP_SAFE(ctx->ds).u;

	s = ms / 1000L;
	ns = (ms % 1000L) * 1000000L;
	p4Nap(s, ns);
}

/**
 * ... TIME&DATE ...
 *
 * (S: -- s m h D M Y )
 *
 * @standard ANS-Forth 1994, Facility
 */
P4_WORD_DEFINE(TIME_DATE)
{
	time_t now;
	struct tm local;

	(void) time(&now);
	LOCALTIME_R(&now, &local);

	p4GrowDS(ctx, 6);
	P4_PUSH(ctx->ds).n = local.tm_sec;
	P4_PUSH(ctx->ds).n = local.tm_min;
	P4_PUSH(ctx->ds).n = local.tm_hour;
	P4_PUSH(ctx->ds).n = local.tm_mday;
	P4_PUSH(ctx->ds).n = local.tm_mon + 1;
	P4_PUSH(ctx->ds).n = local.tm_year + 1900;
}

/**
 * ... ALLOCATE ...
 *
 * (S: u -- a-addr ior )
 *
 * ior equals zero (0) on success.
 *
 * @standard ANS-Forth 1994, Memory
 */
P4_WORD_DEFINE(ALLOCATE)
{
	if (P4_LENGTH(ctx->ds) < 1)
		p4Throw(ctx, P4_THROW_DS_UNDER);

	P4_TOP(ctx->ds).p = calloc(1, P4_TOP(ctx->ds).u * P4_CELL);
	P4_PUSH_SAFE(ctx->ds).n = (P4_TOP(ctx->ds).p == NULL);
}

/**
 * ... FREE ...
 *
 * (S: a-addr -- ior )
 *
 * ior equals zero (0) on success.
 *
 * @standard ANS-Forth 1994, Memory
 */
P4_WORD_DEFINE(FREE)
{
	free(P4_TOP(ctx->ds).p);
	P4_TOP(ctx->ds).n = 0;
}

/**
 * ... RESIZE ...
 *
 * (S: a-addr1 u -- a-addr2 ior )
 *
 * ior equals zero (0) on successful allocation.
 *
 * @standard ANS-Forth 1994, Memory
 */
P4_WORD_DEFINE(RESIZE)
{
	void *copy = realloc(P4_PEEK(ctx->ds, -1).p, P4_TOP(ctx->ds).u * P4_CELL);
	if (copy != NULL)
		P4_POKE(ctx->ds, -1).p = copy;
	P4_TOP(ctx->ds).n = (copy == NULL);
}

/**
 * ... RESERVE ...
 *
 * (S: n -- a-addr )
 *
 * @note
 *	During the compiliation of a word in C based implementations
 *	data-space regions may be relocated when as they are enlarged,
 *	thus invalidating previous values of HERE. Therefore:
 *
 *	... HERE 100 ALLOT ... \ fill in allotment
 *
 *	Should ALLOT enlarge and relocate the data-space, the address
 *	saved by HERE on the stack will now point into invalid memory.
 *
 *	With RESERVE the address of the region just reserved is on
 *	top of the stack insuring that the address is valid until the
 *	next enlargement of the data-space by RESERVE, comma (,),
 *	c-comma (C,), compile-comma (COMPILE,), ALIGN.
 *
 * @standard extension
 *
 * @see >BODY >HERE
 */
P4_WORD_DEFINE(RESERVE)
{
	P4_Byte *here;
	P4_Data *data, *replace;
	P4_Signed nbytes = P4_TOP(ctx->ds).n;

	P4_TOP(ctx->ds).p = NULL;

	if (ctx->xt == NULL)
		return;

	data = ctx->xt->data;

	if (data == NULL) {
		if (nbytes < 0)
			return;
		if ((replace = malloc(sizeof (*data) - sizeof (data->base) + nbytes)) == NULL)
			error_abort(__LINE__);
		data = replace;
		data->length = 0;
		data->size = nbytes;
	} else if (data->size <= data->length + nbytes) {
		if ((replace = realloc(data, sizeof (*data) - sizeof (data->base) + data->size + nbytes)) == NULL)
			error_abort(__LINE__);
		data = replace;
		data->size += nbytes;
	}

	/* Can't release more space than we've used. */
	if (nbytes < 0 && 0 <= data->length + nbytes) {
		data->length += nbytes;
		/* Report HERE after releasing the space. */
		here = &data->base[data->length];
	} else {
		/* Report HERE before claiming the space. */
		here = &data->base[data->length];
		data->length += nbytes;
	}

	P4_TOP(ctx->ds).p = here;
	ctx->xt->data = data;
}

/**
 * ... ALIGN ...
 *
 * (S: -- )
 *
 * @note
 *	During the compiliation of a word with C based implementations
 *	data-space regions may be relocated when as they are enlarged,
 *	thus invalidating previous values of HERE.
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(ALIGN)
{
	if (ctx->xt != NULL && ctx->xt->data != NULL) {
		P4_PUSH_SAFE(ctx->ds).n = P4_CELL_ALIGN(ctx->xt->data->length);
		P4_WORD_DO(RESERVE);
		P4_POP(ctx->ds);
	}
}

/**
 * ... .S ...
 *
 * (S: -- )
 *
 * @standard ANS-Forth 1994, Tools
 */
P4_WORD_DEFINE(DOT_S)
{
	p4ArrayDump(&ctx->ds, stdout);
}

/**
 * ... .RS ...
 *
 * (S: -- )
 *
 * @standard extension
 */
P4_WORD_DEFINE(DOT_RS)
{
	p4ArrayDump(&ctx->rs, stdout);
}

/**
 * ... DUMP ...
 *
 * (S: addr u -- )
 *
 * @standard ANS-Forth 1994, Tools
 */
P4_WORD_DEFINE(DUMP)
{
	P4_Size size = P4_POP_SAFE(ctx->ds).u;
	P4_Byte *addr = P4_POP_SAFE(ctx->ds).s;
	p4Dump(stdout, addr, size);
}

/**
 * ... 'SEE ...
 *
 * (S: xt -- )
 *
 * @standard extension
 */
P4_WORD_DEFINE(TICK_SEE)
{
	p4See(stdout, ctx, P4_POP_SAFE(ctx->ds).xt);
}

/**
 * ... PICK ...
 *
 * (S: xu ... x1 x0 u -- xu ... x1 x0 xu )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(PICK)
{
	p4ArrayPick(&ctx->ds, P4_POP(ctx->ds).u);
}

/**
 * ... CS-PICK ...
 *
 * (C: xu ... x1 x0 -- xu ... x1 x0 xu ) (S: u -- )
 *
 * @standard ANS-Forth 1994, Tools
 */
P4_WORD_DEFINE(CS_PICK)
{
	p4ArrayPick(&ctx->ds, P4_POP(ctx->ds).u);
}

/**
 * ... RS-PICK ...
 *
 * (R: xu ... x1 x0 -- xu ... x1 x0 xu ) (S: u -- )
 *
 * @standard extension
 */
P4_WORD_DEFINE(RS_PICK)
{
	p4ArrayPick(&ctx->rs, P4_POP(ctx->ds).u);
}

/**
 * ... ROLL ...
 *
 * (S: xu xu-1 ... x0 u -- xu-1 ... x0 xu )
 *
 * @standard ANS-Forth 1994, Core
 *
 * @note
 *	2 ROLL		=	ROT	( a b c -- b c a)
 *	1 ROLL		=	SWAP	( a b -- b a )
 *	0 ROLL		=	NOOP
 */
P4_WORD_DEFINE(ROLL)
{
	p4ArrayRoll(&ctx->ds, P4_POP(ctx->ds).u);
}

/**
 * ... CS-ROLL ...
 *
 * (C: xu xu-1 ... x0 u -- xu-1 ... x0 xu ) (S: u -- )
 *
 * @standard ANS-Forth 1994, Tools
 */
P4_WORD_DEFINE(CS_ROLL)
{
	p4ArrayRoll(&ctx->ds, P4_POP(ctx->ds).u);
}

/**
 * ... RS-ROLL ...
 *
 * (R: xu xu-1 ... x0 u -- xu-1 ... x0 xu ) (S: u -- )
 *
 * @standard extension
 */
P4_WORD_DEFINE(RS_ROLL)
{
	p4ArrayRoll(&ctx->rs, P4_POP(ctx->ds).u);
}

/**
 * ... DUP ...
 *
 * (S: x -- x x )
 *
 * @standard ANS-Forth 1994, Core
 *
 * @note
 *	: DUP >R R@ R> ;
 *	: DUP 0 PICK ;
 */
P4_WORD_DEFINE(DUP)
{
	P4_Cell top;

	if (P4_LENGTH(ctx->ds) < 1)
		p4Throw(ctx, P4_THROW_DS_UNDER);

	top = P4_TOP(ctx->ds);
	P4_PUSH_SAFE(ctx->ds) = top;
}

/**
 * ... DROP ...
 *
 * (S: x -- )
 *
 * @standard ANS-Forth 1994, Core
 *
 * @note
 *	: DROP 0 * + ;
 */
P4_WORD_DEFINE(DROP)
{
	P4_POP_SAFE(ctx->ds);
}

/**
 * ... AND ...
 *
 * (S: a b -- c)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(AND)
{
	P4_Unsigned top = P4_POP_SAFE(ctx->ds).u;
	P4_TOP(ctx->ds).u &= top;
}

/**
 * ... OR ...
 *
 * (S: a b -- c)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(OR)
{
	P4_Unsigned top = P4_POP_SAFE(ctx->ds).u;
	P4_TOP(ctx->ds).u |= top;
}

/**
 * ... XOR ...
 *
 * (S: a b -- c)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(XOR)
{
	P4_Unsigned top = P4_POP_SAFE(ctx->ds).u;
	P4_TOP(ctx->ds).u ^= top;
}

/**
 * ... INVERT ...
 *
 * (S: a -- b)
 *
 * @standard ANS-Forth 1994, Core
 *
 * @note
 *	: INVERT -1 XOR ;
 */
P4_WORD_DEFINE(INVERT)
{
	P4_TOP(ctx->ds).u = ~P4_TOP(ctx->ds).u;
}

/**
 *  ... NEGATE ...
 *
 * (S: x1 -- x2 )
 *
 * @standard ANS-Forth 1994, Core
 *
 * @note
 *	: NEGATE 0 SWAP - ;
 */
P4_WORD_DEFINE(NEGATE)
{
	P4_TOP(ctx->ds).n = -P4_TOP(ctx->ds).n;
}

/**
 * ... + ...
 *
 * (S: a b -- c)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(ADD)
{
	P4_Signed top = P4_POP_SAFE(ctx->ds).n;
	P4_TOP(ctx->ds).n += top;
}

/**
 * ... - ...
 *
 * (S: a b -- c)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(SUB)
{
	P4_Signed top = P4_POP_SAFE(ctx->ds).n;
	P4_TOP(ctx->ds).n -= top;
}

/**
 * ... * ...
 *
 * (S: a b -- c)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(MUL)
{
	P4_Signed top = P4_POP_SAFE(ctx->ds).n;
	P4_TOP(ctx->ds).n *= top;
}

/**
 * ... / ...
 *
 * (S: dividend divisor -- quotient)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(DIV)
{
	P4_Signed top = P4_POP_SAFE(ctx->ds).n;
	P4_TOP(ctx->ds).n /= top;
}

/**
 * ... MOD ...
 *
 * (S: dividend divisor -- remainder)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(MOD)
{
	P4_Signed top = P4_POP_SAFE(ctx->ds).n;
	P4_TOP(ctx->ds).n %= top;
}

/**
 * ... /MOD ...
 *
 * (S: dividend divisor -- remainder quotient)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(DIV_MOD)
{
	ldiv_t answer = ldiv(P4_PEEK(ctx->ds, -1).n, P4_TOP(ctx->ds).n);
	P4_POKE(ctx->ds, -1).n = answer.rem;
	P4_TOP(ctx->ds).n = answer.quot;
}

/**
 * ... LSHIFT ...
 *
 * (S: x1 u --  x2)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(LSHIFT)
{
	P4_Unsigned top = P4_POP_SAFE(ctx->ds).u;
	P4_TOP(ctx->ds).u <<= top;
}

/**
 * ... RSHIFT ...
 *
 * (S: x1 u --  x2)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(RSHIFT)
{
	P4_Unsigned top = P4_POP_SAFE(ctx->ds).u;
	P4_TOP(ctx->ds).u >>= top;
}

/**
 * ... 2* ...
 *
 * (S: n1 -- n2)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(TWO_MUL)
{
	P4_TOP(ctx->ds).n <<= 1;
}

/**
 * ... 2/ ...
 *
 * (S: n1 -- n2)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(TWO_DIV)
{
	P4_TOP(ctx->ds).n >>= 1;
}

/**
 * ... 0= ...
 *
 * (S: n -- flag)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(ZERO_EQ)
{
	P4_TOP(ctx->ds).u = (P4_TOP(ctx->ds).u == 0) ? ~0 : 0;
}

/**
 * ... 0< ...
 *
 * (S: n -- flag)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(ZERO_LT)
{
	P4_TOP(ctx->ds).n = (P4_TOP(ctx->ds).n < 0) ? ~0 : 0;
}

/**
 * ... U> ...
 *
 * (S: u1 u2 -- flag)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(U_GT)
{
	P4_Unsigned top = P4_POP_SAFE(ctx->ds).u;
	P4_TOP(ctx->ds).u = (P4_TOP(ctx->ds).u > top) ? ~0 : 0;
}

/**
 * ... U< ...
 *
 * (S: u1 u2 -- flag)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(U_LT)
{
	P4_Unsigned top = P4_POP_SAFE(ctx->ds).u;
	P4_TOP(ctx->ds).u = (P4_TOP(ctx->ds).u < top) ? ~0 : 0;
}

/**
 * ... /CELL ...
 *
 * (S:  -- u)
 *
 * @standard extension
 */
P4_WORD_DEFINE(SLASH_CELL)
{
	P4_PUSH_SAFE(ctx->ds).u = P4_CELL;
}

/**
 * : X ... [ ...
 *
 * (S: -- )
 *
 * Set interpret state.
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(LSQUARE)
{
	ctx->state = 0;
}

/**
 * ... ] ... ;
 *
 * (S: -- )
 *
 * Set compilate state.
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(RSQUARE)
{
	ctx->state = 1;
}

/**
 *  ... STATE ...
 *
 * (S: -- a-addr)
 *
 * Address of interpret state variable.
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(STATE)
{
	P4_PUSH_SAFE(ctx->ds).p = &ctx->state;
}

/**
 * : X ... ; IMMEDIATE
 *
 * (S: -- )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(IMMEDIATE)
{
	P4_WORD_SET_IMM(ctx->words);
}

/**
 * ... SWAP ...
 *
 * (S: x y -- y x )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(SWAP)
{
	P4_Cell tmp;

	if (P4_LENGTH(ctx->ds) < 2)
		p4Throw(ctx, P4_THROW_DS_UNDER);

	/* (S: x y -- y x ) */
	tmp = P4_TOP(ctx->ds);
	P4_TOP(ctx->ds) = P4_PEEK(ctx->ds, -1);
	P4_POKE(ctx->ds, -1) = tmp;
}

/**
 * ... DEPTH ...
 *
 * (S: -- u )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(DEPTH)
{
	P4_Signed depth = P4_LENGTH(ctx->ds);
	P4_PUSH_SAFE(ctx->ds).n = depth;
}

/**
 * ... ! ...
 *
 * (S: x a-addr -- )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(STORE)
{
	P4_Cell *a_addr = P4_POP_SAFE(ctx->ds).a;
	*a_addr = P4_POP_SAFE(ctx->ds);
}

/**
 * ... @ ...
 *
 * (S: a-addr -- x)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(FETCH)
{
	P4_TOP(ctx->ds) = *P4_TOP(ctx->ds).a;
}

/**
 * ... >R ...
 *
 * (S: x -- ) (R: -- x)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(RS_PUT)
{
	P4_PUSH_SAFE(ctx->rs) = P4_POP_SAFE(ctx->ds);
}

/**
 * ... R> ...
 *
 * (S: -- x) (R: x -- )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(RS_GET)
{
	if (P4_LENGTH(ctx->rs) < 1)
		p4Throw(ctx, P4_THROW_RS_UNDER);
	P4_PUSH_SAFE(ctx->ds) = P4_POP(ctx->rs);
}

/**
 * ... R@ ...
 *
 * (S:  -- x) (R: x -- x)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(RS_COPY)
{
	P4_PUSH_SAFE(ctx->ds).p = P4_IS_EMPTY(ctx->rs) ? NULL : P4_TOP(ctx->rs).p;
}

/**
 *  ... 2R@  ...
 *
 * (S: -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(TWO_RS_COPY)
{
	if (P4_LENGTH(ctx->rs) < 2)
		p4Throw(ctx, P4_THROW_RS_UNDER);

	p4GrowDS(ctx, 2);
	P4_PUSH(ctx->ds).u = P4_PEEK(ctx->rs, -1).u;
	P4_PUSH(ctx->ds).u = P4_TOP(ctx->rs).u;
}

/**
 *  ... 2R> ...
 *
 * (S: -- x1 x2 ) ( R: x1 x2 -- )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(TWO_RS_GET)
{
	if (P4_LENGTH(ctx->rs) < 2)
		p4Throw(ctx, P4_THROW_RS_UNDER);

	p4GrowDS(ctx, 2);
	P4_PUSH(ctx->ds).u = P4_POP(ctx->rs).u;
	P4_PUSH(ctx->ds).u = P4_POP(ctx->rs).u;
	P4_WORD_DO(SWAP);
}

/**
 *  ... 2>R ...
 *
 * (S: x1 x2 -- ) ( R:  -- x1 x2 )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(TWO_RS_PUT)
{
	p4GrowRS(ctx, 2);
	P4_WORD_DO(SWAP);
	P4_PUSH(ctx->rs).u = P4_POP(ctx->ds).u;
	P4_PUSH(ctx->rs).u = P4_POP(ctx->ds).u;
}

/**
 * ... C@ ...
 *
 * (S: c-addr -- char )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(C_FETCH)
{
	P4_TOP(ctx->ds).n = *P4_TOP(ctx->ds).s;
}

/**
 * ... C! ...
 *
 * (S: char c-addr -- )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(C_STORE)
{
	P4_Byte *c_addr = P4_POP_SAFE(ctx->ds).s;
	*c_addr = (P4_Byte) P4_POP_SAFE(ctx->ds).n;
}

/**
 * ... C, ...
 *
 * (S: char -- )
 *
 * @note
 *	During the compiliation of a word with C based implementations
 *	data-space regions may be relocated when as they are enlarged,
 *	thus invalidating previous values of HERE.
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(C_COMMA)
{
	if (ctx->xt != NULL) {
		P4_PUSH_SAFE(ctx->ds).n = 1;
		P4_WORD_DO(RESERVE);
		P4_WORD_DO(C_STORE);
	}
}

/**
 * ... >BODY ...
 *
 * (S: xt -- a-addr )
 *
 * @note
 *	In a C based implementation, this function does not suffer
 *	from the issue concerning relocation of enlarged data-space
 *	regions. In order to have found the xt, the word definition
 *	must be complete and in the dictionary such that the data-
 *	space will no longer change size, thus it is now possible
 *	to find and use its base address.
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(GT_BODY)
{
	P4_Exec_Token xt = P4_TOP(ctx->ds).xt;
	P4_TOP(ctx->ds).p = xt->data == NULL ? NULL : xt->data->base;
}

/**
 * ... >HERE ...
 *
 * (S: -- u )
 *
 * Offset from the data-space start address for the word being compiled.
 * Similar to the original ANS-Forth 1994 word HERE, except expressed
 * as an offset.
 *
 * @note
 *	During the compiliation of a word with C based implementations
 *	data-space regions may be relocated when as they are enlarged,
 *	thus invalidating previous values of HERE.
 *
 * @standard extension
 */
P4_WORD_DEFINE(GT_HERE)
{
	P4_PUSH_SAFE(ctx->ds).u = ctx->xt != NULL && ctx->xt->data != NULL ? ctx->xt->data->length : 0;
}

/**
 * ... HERE ...
 *
 * (S: -- a-addr )
 *
 * @note
 *	During the compiliation of a word with C based implementations
 *	data-space regions may be relocated when as they are enlarged,
 *	thus invalidating previous values of HERE.
 *
 * @standard extension
 */
P4_WORD_DEFINE(HERE)
{
	P4_PUSH_SAFE(ctx->ds).s = ctx->xt != NULL && ctx->xt->data != NULL
		? (ctx->xt->data->base + ctx->xt->data->length)
		: 0
	;
}

/**
 * ... UNUSED ...
 *
 * (S: -- u )
 *
 * @note
 *	During the compiliation of a word with C based implementations
 *	data-space regions may be relocated when as they are enlarged,
 *	thus invalidating previous values of HERE.
 *
 * @standard extension
 */
P4_WORD_DEFINE(UNUSED)
{
	P4_PUSH_SAFE(ctx->ds).u = ctx->xt != NULL && ctx->xt->data != NULL
		? (ctx->xt->data->size - ctx->xt->data->length)
		: 0
	;
}

/**
 * ... 123 , ...
 *
 * (S: x -- )
 *
 * @note
 *	During the compiliation of a word with C based implementations
 *	data-space regions may be relocated when as they are enlarged,
 *	thus invalidating previous values of HERE.
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(COMMA)
{
	if (ctx->xt != NULL) {
		P4_PUSH_SAFE(ctx->ds).n = P4_CELL;
		P4_WORD_DO(RESERVE);
		P4_WORD_DO(STORE);
	}
}

/**
 * ... COMPILE, ...
 *
 * (S: xt -- )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(COMPILE_COMMA)
{
	P4_WORD_DO(COMMA);
}

/**
 * ... NEW_WORD ...
 *
 * (S: "<spaces>name" -- xt )
 *
 * @standard internal
 */
static
P4_WORD_DEFINE(NEW_WORD)
{
	P4_Word *word;
	P4_String name;

	P4_WORD_DO(PARSE_WORD);
	name.length = (P4_Byte) P4_POP(ctx->ds).u;
	name.string = P4_TOP(ctx->ds).s;

	if (name.length == 0)
		p4Throw(ctx, P4_THROW_EMPTY_NAME);

	if ((word = malloc(sizeof (*word) + name.length + 1)) == NULL)
		error_abort(__LINE__);

	word->name.length = name.length;
	word->name.string = (P4_Byte *) &word[1];
	memcpy(word->name.string, name.string, name.length);
	word->name.string[name.length] = '\0';
	word->xt = NULL;
	word->bits = 0;

	P4_TOP(ctx->ds).w = word;
}

/**
 * ... _enter ...
 *
 * (R: -- ip )
 *
 * @note
 *	Entry code used by colon definitions. See :NONAME and colon (:).
 *
 * @standard internal
 */
P4_WORD_DEFINE(_enter)
{
	P4_Exec_Token xt = ctx->ip[-1];
	P4_PUSH_SAFE(ctx->rs).p = ctx->ip;
	ctx->ip = (P4_Exec_Token *) xt->data->base;
}

/**
 * ... _exit ...
 *
 * (R:ip -- )
 *
 * @note
 *	Exit code used by colon definitions. See semi-colon (;) and DOES>.
 *
 * @standard internal
 */
P4_WORD_DEFINE(_exit)
{
	if (P4_LENGTH(ctx->rs) < 1)
		p4Throw(ctx, P4_THROW_RS_UNDER);
	ctx->ip = P4_POP(ctx->rs).p;
}

/**
 * ... _finish ...
 *
 * @note
 *	Used by semi-colon or when creating a new definition
 *	to add the current now completed word to the dictionary,
 *
 *		: NAME ... ;
 *
 *		:NONAME ... ;
 *
 *		... CREATE ... CREATE ... : ...
 *
 * @standard internal
 */
P4_WORD_DEFINE(_finish)
{
#ifdef OVER_ALLOCATE_DATA_SPACE
	P4_Data *replace;
	P4_Exec_Token xt;

	if ((xt = ctx->xt) != NULL) {
		if (xt->data != NULL && xt->data->length < xt->data->size) {
			if ((replace = realloc(xt->data, sizeof (*replace) - sizeof (replace->base) + xt->data->length)) == NULL)
				error_abort(__LINE__);
			replace->size = replace->length;
			xt->data = replace;
		}
		ctx->xt = NULL;
	}
#endif
	if (ctx->word != NULL) {
		/* Add the word to the dictionary. */
		ctx->word->prev = ctx->words;
		ctx->words = ctx->word;
		ctx->word = NULL;
	}
}

/**
 * ... _noname ...
 *
 * @standard internal
 */
P4_WORD_DEFINE(_noname)
{
	P4_Exec_Token xt;

	P4_WORD_DO(_finish);

	if ((xt = malloc(sizeof (*xt))) == NULL)
		error_abort(__LINE__);

	xt->code = p4_do__enter;
	xt->data = NULL;
	ctx->xt = xt;

#ifdef OVER_ALLOCATE_DATA_SPACE
	/* Allocate some free data space for compiling. */
	P4_PUSH(ctx->ds).n = 128 * P4_CELL;
	P4_WORD_DO(RESERVE);

	/* Replace data-space a-addr on stack with noname xt. */
	P4_TOP(ctx->ds).xt = xt;
#else
	P4_PUSH_SAFE(ctx->ds).xt = xt;
#endif
	/* Add the xt to the noname stack for context clean-up by p4Free. */
	P4_PUSH_SAFE(ctx->noname).xt = xt;
}

/**
 * ... :NONAME ...
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(COLON_NONAME)
{
	if (P4_IS_INTERPRETING(ctx)) {
		P4_WORD_DO(RSQUARE);
		P4_WORD_DO(_noname);
	}
}

/**
 * ... : word ...
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(COLON)
{
	if (P4_IS_INTERPRETING(ctx)) {
		P4_WORD_DO(COLON_NONAME);
		P4_WORD_DO(NEW_WORD);

		/* Remember the current word being defined. */
		ctx->word = P4_POP(ctx->ds).w;
		ctx->word->xt = P4_POP(ctx->ds).xt;
	}
}

/**
 * : X ... ;
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(SEMICOLON)
{
	if (P4_IS_COMPILING(ctx)) {
		P4_WORD_COMPILE(ctx, _exit);
		P4_WORD_DO(LSQUARE);
	}

	P4_WORD_DO(_finish);
}

/**
 * ... CMOVE ...
 *
 * (S: c-addr-src c-addr-dst u -- )
 *
 * @standard ANS-Forth 1994, String
 */
P4_WORD_DEFINE(CMOVE)
{
	P4_Size size = P4_POP_SAFE(ctx->ds).u;
	P4_Byte *dst = P4_POP_SAFE(ctx->ds).s;
	P4_Byte *src = P4_POP_SAFE(ctx->ds).s;

	while (0 < size--)
		*dst++ = *src++;
}

/**
 * ... MOVE ...
 *
 * (S: a-addr-src a-addr-dst u -- )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(MOVE)
{
	P4_Size size = P4_POP_SAFE(ctx->ds).u;
	P4_Pointer dst = P4_POP_SAFE(ctx->ds).p;
	P4_Pointer src = P4_POP_SAFE(ctx->ds).p;
	memmove(dst, src, size);
}

/**
 * ... _lit x ...
 *
 * (S: -- x )
 *
 * @standard internal
 */
P4_WORD_DEFINE(_lit)
{
	P4_PUSH_SAFE(ctx->ds).xt = *ctx->ip++;
}

/**
 * : X ... [ x ] LITERAL ...
 *
 * (S: -- x ) // (S: x -- )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(LITERAL)
{
	if (P4_IS_COMPILING(ctx)) {
		P4_WORD_COMPILE(ctx, _lit);
		P4_WORD_DO(COMMA);
	}
}

/**
 * ... _var ...
 *
 * (S: -- a-addr )
 *
 * @standard internal
 */
P4_WORD_DEFINE(_var)
{
	P4_Exec_Token xt = ctx->ip[-1];
	P4_PUSH_SAFE(ctx->ds).p = xt->data == NULL ? NULL : xt->data->base;
}

/**
 * ... CREATE word ...
 *
 * (S: "<spaces>name" -- ) // (S: -- a-addr )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(CREATE)
{
	P4_Word *word;

	P4_WORD_DO(_noname);
	P4_WORD_DO(NEW_WORD);
	word = P4_POP(ctx->ds).w;
	word->xt = P4_POP(ctx->ds).xt;
	word->xt->code = p4_do__var;

	word->prev = ctx->words;
	ctx->words = word;
}

/**
 * ... BRANCH ...
 *
 * (S: -- )
 *
 * @standard internal
 *
 * @note
 *	Unconditional branch relative; offset found immediately after branch xt.
 */
P4_WORD_DEFINE(BRANCH)
{
	ctx->ip = (P4_Exec_Token *)((P4_Byte *) ctx->ip + (P4_Signed) *ctx->ip);
}

/**
 * ... BRANCHZ ...
 *
 * (S: flag -- )
 *
 * @standard internal
 *
 * @note
 *	Conditional branch relative; offset found immediately after BRANCHZ xt.
 */
P4_WORD_DEFINE(BRANCHZ)
{
	if (P4_POP(ctx->ds).n == 0)
		ctx->ip = (P4_Exec_Token *)((P4_Byte *) ctx->ip + (P4_Signed) *ctx->ip);
	else
		ctx->ip++;
}

/**
 * ... JUMP ...
 *
 * (S: -- )
 *
 * @note
 *	Unconditional absolute jump; address found immediately after JUMP xt.
 *
 * @standard internal
 */
P4_WORD_DEFINE(JUMP)
{
	ctx->ip = (P4_Exec_Token *) *ctx->ip;
}

P4_WORD_DECL(extern, JUMP);

/**
 * ... JUMPZ ...
 *
 * (S: flag -- )
 *
 * @note
 *	Conditional absolute jump; address found immediately after JUMPZ xt.
 *
 * @standard internal
 */
P4_WORD_DEFINE(JUMPZ)
{
	if (P4_POP(ctx->ds).n == 0)
		ctx->ip = (P4_Exec_Token *) *ctx->ip;
	else
		ctx->ip++;
}

P4_WORD_DECL(extern, JUMPZ);

/**
 * ...
 *
 * (S: -- a-addr ) (R: -- ip )
 *
 * @standard internal
 */
P4_WORD_DEFINE(_does)
{
	P4_Word *word;

	/* Remember the CREATEd word before :NONAME. */
	word = ctx->words;

	/* Create a noname xt. */
	P4_WORD_DO(_noname);

	/* Compile the CREATEd word's xt into noname word.
	 * The CREATEd word will put a-addr of data on stack.
	 */
	P4_PUSH_SAFE(ctx->ds).xt = word->xt;
	P4_WORD_DO(COMMA);

	/* Compile jump to DOES> code, which will do exit. */
	P4_WORD_COMPILE(ctx, JUMP);

	/* ctx->ip points to exit xt following does xt; runtime
	 * follows after exit xt.
	 *
	 *	: TYPE CREATE build-code does exit run-time ;
	 *                                    ^
	 *			ctx->ip ------|
	 */
	P4_PUSH_SAFE(ctx->ds).p = ctx->ip + 1;
	P4_WORD_DO(COMMA);

	/* Replace the created word's xt with noname xt. */
	word->xt = P4_POP(ctx->ds).xt;
}

/**
 * ... CREATE ... DOES> ...
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(DOES_GT)
{
	if (P4_IS_COMPILING(ctx)) {
		/* Setup tail end of build-code that will
		 * setup the runtime when the new word is
		 * created.
		 */
		P4_WORD_COMPILE(ctx, _does);
		P4_WORD_COMPILE(ctx, _exit);
	}
}

/**
 * ... ' word ...
 *
 * (S: "<spaces>name" -- xt )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(TICK)
{
	P4_WORD_DO(PARSE_WORD);		/* ( <spaces>name -- c-addr u */
	P4_WORD_DO(FIND_WORD);		/* ( c-addr u -- c-addr u 0 | xt 1 | xt -1 ) */

	if (P4_POP(ctx->ds).n == 0) {
		/* If word was not found then assume ' NOOP. */
		P4_POP(ctx->ds);	/* ( c-addr u -- c-addr ) */
		P4_TOP(ctx->ds).xt = P4_WORD_XT(NOOP); /* ( c-addr -- xt ) */
	}
}

/**
 * : X ... ['] word ...
 *
 * (S: "<spaces>name" -- xt )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(SQUARE_TICK)
{
	P4_WORD_DO(TICK);
}

/**
 * ... EXECUTE ...
 *
 * (S: xt -- )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(EXECUTE)
{
	P4_Exec_Token xts[2], *ip;

	if (P4_LENGTH(ctx->ds) < 1)
		p4Throw(ctx, P4_THROW_DS_UNDER);

	xts[0] = P4_POP(ctx->ds).xt;
	xts[1] = NULL;

	ip = ctx->ip;
	for (ctx->ip = xts; *ctx->ip != NULL; ) {
		(*(*ctx->ip++)->code)(ctx);
		p4OnSignal(ctx);
	}
	ctx->ip = ip;
}

/**
 * ... EMIT ...
 *
 * (S: x -- )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(EMIT)
{
	fputc((int) P4_POP_SAFE(ctx->ds).n, stdout);
}

/**
 * ... TYPE ...
 *
 * (S: c-addr u -- )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(TYPE)
{
	P4_Size length = P4_POP(ctx->ds).u;
	P4_Byte *c_addr = P4_POP(ctx->ds).s;

	for ( ; 0 < length; length--)
		fputc(*c_addr++, stdout);
}

/**
 * ... KEY ...
 *
 * (S: -- char )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(KEY)
{
	fflush(stdout);
	if (ctx->unget != EOF) {
		P4_PUSH_SAFE(ctx->ds).n = ctx->unget;
		ctx->unget = EOF;
		return;
	}

#ifdef HAVE_TCSETATTR
	P4_PUSH_SAFE(ctx->ds).n = EOF;
	if (tcsetattr(tty_fd, TCSANOW, &tty_raw) == 0)
		P4_TOP(ctx->ds).n = p4ReadByte(tty_fd);
#else
	P4_PUSH_SAFE(ctx->ds).n = p4ReadByte(tty_fd);
#endif
}

/**
 * ... KEY? ...
 *
 * (S: -- flag )
 *
 * @standard ANS-Forth 1994, Facility
 */
P4_WORD_DEFINE(KEY_QM)
{
	fflush(stdout);
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

	P4_PUSH_SAFE(ctx->ds).n = ctx->unget != EOF;
}

/**
 *  ...  .R  ...
 *
 * (S: n width -- )
 *
 * @standard extension
 */
P4_WORD_DEFINE(DOT_R)
{
	P4_Signed width = P4_POP_SAFE(ctx->ds).n;
	char buffer[sizeof (P4_Signed) * CHAR_BIT + 2];

	width -= p4SignedToString(P4_POP_SAFE(ctx->ds).n, ctx->base, buffer, sizeof (buffer));

	while (0 < width--)
		fputc(' ', stdout);

	fputs(buffer, stdout);
}

/**
 *  ...  U.R  ...
 *
 * (S: u width -- )
 *
 * @standard extension
 */
P4_WORD_DEFINE(U_DOT_R)
{
	P4_Signed width = P4_POP_SAFE(ctx->ds).n;
	char buffer[sizeof (P4_Signed) * CHAR_BIT + 2];

	(void) p4UnsignedToString(P4_POP_SAFE(ctx->ds).u, ctx->base, buffer, sizeof (buffer));

	while (0 < width--)
		fputc(' ', stdout);

	fputs(buffer, stdout);
}

/**
 * ... >IN ...
 *
 * (S: -- a-addr)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(GT_IN)
{
	P4_PUSH_SAFE(ctx->ds).p = &ctx->input.offset;
}

/**
 * ... BLK ...
 *
 * (S: -- a-addr)
 *
 * @standard ANS-Forth 1994, Block
 */
P4_WORD_DEFINE(BLK)
{
	P4_PUSH_SAFE(ctx->ds).p = &ctx->input.blk;
}

/**
 * ... CONSOLE ...
 *
 * (S: -- c-addr u)
 *
 * @note
 *	Expected use:
 *
 *		... CONSOLE ACCEPT ...
 *
 * @standard extension
 */
P4_WORD_DEFINE(CONSOLE)
{
	p4GrowDS(ctx, 2);
	P4_PUSH(ctx->ds).s = ctx->console;
	P4_PUSH(ctx->ds).u = sizeof (ctx->console);
}

/**
 * ... PAD ...
 *
 * (S: -- c-addr u)
 *
 * @note
 *	Expected use:
 *
 *		... PAD ACCEPT ...
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(PAD)
{
	P4_PUSH_SAFE(ctx->ds).s = ctx->pad;
}

/**
 * ... /PAD ...
 *
 * (S: -- u)
 *
 * @standard extension
 */
P4_WORD_DEFINE(SLASH_PAD)
{
	P4_PUSH_SAFE(ctx->ds).u = sizeof (ctx->pad);
}


/**
 * ... SOURCE ...
 *
 * (S: -- c-addr u)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(SOURCE)
{
	p4GrowDS(ctx, 2);
	P4_PUSH(ctx->ds).s = &ctx->input.buffer[ctx->input.offset];
	P4_PUSH(ctx->ds).u = ctx->input.length - ctx->input.offset;
}

/**
 * ... SOURCE-ID ...
 *
 * (S: -- a-addr)
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(SOURCE_ID)
{
	P4_PUSH_SAFE(ctx->ds).p = &ctx->input.fd;
}

/**
 * ... BASE ...
 *
 * (S: -- a-addr)
 *
 * @standard extension
 */
P4_WORD_DEFINE(BASE)
{
	P4_PUSH_SAFE(ctx->ds).p = &ctx->base;
}

/**
 * ... ACCEPT ...
 *
 * (S: c-addr +n -- n )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(ACCEPT)
{
	P4_Unsigned size = P4_POP(ctx->ds).u;
	P4_Byte *c_addr = P4_TOP(ctx->ds).s;
	P4_TOP(ctx->ds).n = p4GetLine(ctx, c_addr, size);
}

/**
 * ... REFILL ...
 *
 * (S: -- flag)
 *
 *	: REFILL
 *		SOURCE-ID 0< IF
 *			FALSE
 *		ELSE
 *			0 >IN !
 *			CONSOLE ACCEPT
 *			DUP >IN !
 *			0>
 *		THEN
 *	;
 *
 * @standard ANS-Forth 1994, Core, File
 */
P4_WORD_DEFINE(REFILL)
{
	if (P4_INPUT_IS_STR(ctx)) {
		P4_PUSH_SAFE(ctx->ds).n = 0;
	} else {
		ctx->input.offset = 0;
		ctx->input.length = p4GetLine(ctx, ctx->input.buffer, P4_INPUT_SIZE);
		P4_PUSH_SAFE(ctx->ds).n = 0 < ctx->input.length;
	}
}

/**
 * ... WORDS ...
 *
 * (S: -- )
 *
 * @standard ANS-Forth 1994, Tools
 */
P4_WORD_DEFINE(WORDS)
{
	P4_Word *word;
	P4_Unsigned column = 0;
	unsigned short window[4] = { 24, 80 };

#ifdef TIOCGWINSZ
	ioctl(0, TIOCGWINSZ, window);
#endif
	for (word = ctx->words; word != NULL; word = word->prev) {
		if (window[1] <= column + word->name.length + 1) {
			fputc('\n', stdout);
			column = 0;
		}
		column += fprintf(stdout, "%s ", word->name.string);
	}
	fputc('\n', stdout);
}

static void
p4Interpret(P4_Context *ctx)
{
	P4_Signed n;
	P4_Size length;
	P4_Unsigned ibase;
	P4_Byte *stop, *start;

	while (ctx->input.offset < ctx->input.length) {
		P4_WORD_DO(PARSE_WORD);
		P4_WORD_DO(FIND_WORD);

		if ((n = P4_POP(ctx->ds).n) == 0) {
			/* Word not found; try converting to a number. */
			length = P4_POP(ctx->ds).u;
			start = P4_POP(ctx->ds).s;
			if (length == 0)
				continue;

			length--;
			switch (*start++) {
			case '$': /* $F9 hex */
				ibase = 16;
				break;
			case '#': /* #99 decimal */
				ibase = 10;
				break;
			case '0': /* 0377 octal or 0xFF hex */
				if (tolower(*start) == 'x') {
					ibase = 16;
					length--;
					start++;
				} else {
					ibase = 8;
					length++;
					start--;
				}
				break;
			case '%': /* %1011 binary */
				ibase = 2;
				break;
			case '\\': /* \c backslash escape */
				ibase = 1;
				break;
			case '\'': /* 'c character */
				if (length == 2 && start[1] == '\'') {
					ibase = 0;
					length--;
				}
				break;
			default:
				ibase = ctx->base;
				length++;
				start--;
				break;
			}

			n = p4StringToSigned(start, &stop, ibase);

			/* Was it a numeric string in the input base? */
			if (stop - start == length) {
				P4_PUSH(ctx->ds).n = n;
				P4_WORD_DO(LITERAL);
			} else {
				if (P4_IS_COMPILING(ctx)) {
					fprintf(
						stdout, "in %s error near ",
						ctx->word == NULL ? ":NONAME" : (char *)ctx->word->name.string
					);
				}

				for (n = 0; n < length; n++)
					fputc(*start++, stdout);

				fprintf(stdout, " ?\n");
				break;
			}
		} else if (P4_IS_COMPILING(ctx) && n == -1) {
			P4_WORD_DO(COMPILE_COMMA);
		} else {
			P4_WORD_DO(EXECUTE);
		}
	}
}

/**
 * ... EVALUATE ...
 *
 * (S: i*x c-addr u -- j*x )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(EVALUATE)
{
	P4_INPUT_PUSH(&ctx->input);

	ctx->input.blk = 0;
	ctx->input.fd = P4_INPUT_STR;
	ctx->input.fp = NULL;
	ctx->input.offset = 0;
	ctx->input.length = P4_POP_SAFE(ctx->ds).u;
	ctx->input.buffer = P4_POP_SAFE(ctx->ds).s;

	p4Interpret(ctx);

	P4_INPUT_POP(&ctx->input);
}

/**
 * ... S" pathname" INCLUDED ...
 *
 * (S: i*x c-addr u -- j*x )
 *
 * @standard ANS-Forth 1994, File
 */
P4_WORD_DEFINE(INCLUDED)
{
	P4_Signed rc;
	P4_Byte *path;
	P4_Size length;

	if (P4_LENGTH(ctx->ds) < 2)
		p4Throw(ctx, P4_THROW_DS_UNDER);

	length = P4_POP(ctx->ds).u;
	path = P4_POP(ctx->ds).s;
	path[length] = '\0';
	if ((rc = p4EvalFile(ctx, path)) != 0)
		p4Throw(ctx, rc);
}

/**
 * ... ?BLOCKS ...
 *
 * (S: c-addr u -- u )
 *
 * @standard ANS-Forth 1994, Block
 */
P4_WORD_DEFINE(QM_BLOCKS)
{
	struct stat sb;

	sb.st_size = 0;
	P4_POP(ctx->ds);
	(void) stat(P4_TOP(ctx->ds).s, &sb);
	P4_TOP(ctx->ds).u = sb.st_size / P4_BLOCK_SIZE;
}

/**
 * ... BLOCK ...
 *
 * (S: u -- a-addr )
 *
 * @standard ANS-Forth 1994, Block
 */
P4_WORD_DEFINE(BLOCK)
{
	int fd;
	P4_Unsigned blk_num;

	if ((blk_num = P4_POP_SAFE(ctx->ds).u) == 0)
		blk_num = 1;

	P4_PUSH(ctx->ds).s = ctx->block.buffer;

	if (ctx->block.number == blk_num)
		return;

	if ((fd = open(ctx->block_file, O_CREAT|O_RDWR, S_IRWXU|S_IRWXG|S_IRWXO)) < 0 || flock(fd, LOCK_EX))
		p4Throw(ctx, P4_THROW_ENOENT);

	if (ctx->block.state == P4_BLOCK_DIRTY && p4BlockWrite(fd, &ctx->block)) {
		(void) close(fd);
		p4Throw(ctx, P4_THROW_BLOCK_WR);
	}

	if (p4BlockRead(fd, blk_num, &ctx->block)) {
		(void) close(fd);
		p4Throw(ctx, P4_THROW_BLOCK_RD);
	}

	(void) close(fd);
}

/**
 * ... BUFFER ...
 *
 * (S: u -- a-addr )
 *
 * @standard ANS-Forth 1994, Block
 */
P4_WORD_DEFINE(BUFFER)
{
	int fd;
	P4_Unsigned blk_num;

	if ((blk_num = P4_POP_SAFE(ctx->ds).u) == 0)
		blk_num = 1;

	P4_PUSH(ctx->ds).s = ctx->block.buffer;

	if (ctx->block.number == blk_num)
		return;

	if (ctx->block.state == P4_BLOCK_DIRTY) {
		if ((fd = open(ctx->block_file, O_CREAT|O_WRONLY, S_IRWXU|S_IRWXG|S_IRWXO)) < 0 || flock(fd, LOCK_EX))
			p4Throw(ctx, P4_THROW_ENOENT);

		if (p4BlockWrite(fd, &ctx->block)) {
			(void) close(fd);
			p4Throw(ctx, P4_THROW_BLOCK_WR);
		}
	}

	ctx->block.state = P4_BLOCK_CLEAN;
	ctx->block.number = blk_num;

	(void) close(fd);
}

/**
 * ... EMPTY-BUFFERS ...
 *
 * (S: -- )
 *
 * @standard ANS-Forth 1994, Block
 */
P4_WORD_DEFINE(EMPTY_BUFFERS)
{
	ctx->block.state = P4_BLOCK_FREE;
	ctx->block.number = 0;
}

/**
 * ... SAVE-BUFFERS ...
 *
 * (S: -- )
 *
 * @standard ANS-Forth 1994, Block
 */
P4_WORD_DEFINE(SAVE_BUFFERS)
{
	int fd;

	if (ctx->block.state == P4_BLOCK_DIRTY) {
		if ((fd = open(ctx->block_file, O_CREAT|O_WRONLY, S_IRWXU|S_IRWXG|S_IRWXO)) < 0 || flock(fd, LOCK_EX))
			p4Throw(ctx, P4_THROW_ENOENT);

		if (p4BlockWrite(fd, &ctx->block)) {
			(void) close(fd);
			p4Throw(ctx, P4_THROW_BLOCK_WR);
		}
		(void) close(fd);
	}
}

/**
 * ... FLUSH ...
 *
 * (S: -- )
 *
 * @standard ANS-Forth 1994, Block
 */
P4_WORD_DEFINE(FLUSH)
{
	P4_WORD_DO(SAVE_BUFFERS);
	P4_WORD_DO(EMPTY_BUFFERS);
}

/**
 * ... LOAD ...
 *
 * (S: u -- )
 *
 * @standard ANS-Forth 1994, Block
 */
P4_WORD_DEFINE(LOAD)
{
	P4_Unsigned blk_num;

	if ((blk_num = P4_TOP(ctx->ds).u) == 0)
		blk_num = 1;

	P4_INPUT_PUSH(&ctx->input);

	P4_WORD_DO(BLOCK);

	ctx->input.buffer = P4_POP(ctx->ds).s;
	ctx->input.length = P4_BLOCK_SIZE;
	ctx->input.blk = blk_num;
	ctx->input.offset = 0;
	ctx->input.fp = NULL;
	ctx->input.fd = P4_INPUT_STR;

	p4Interpret(ctx);

	P4_INPUT_POP(&ctx->input);
}

/**
 * ... UPDATE ...
 *
 * (S: -- )
 *
 * @standard ANS-Forth 1994, Block
 */
P4_WORD_DEFINE(UPDATE)
{
	if (0 < ctx->block.number)
		ctx->block.state = P4_BLOCK_DIRTY;
}

/**
 * ... UPDATED? ...
 *
 * (S: block -- flag )
 *
 * @standard extension
 */
P4_WORD_DEFINE(UPDATED_QM)
{
	P4_Unsigned blk_num;

	if ((blk_num = P4_TOP(ctx->ds).u) == 0)
		blk_num = 1;

	P4_PUSH_SAFE(ctx->ds).n = blk_num == ctx->block.number && ctx->block.state == P4_BLOCK_DIRTY;
}

/**
 * ... DELETE-FILE ...
 *
 * (S: name u -- errno )
 *
 * @standard ANS-Forth 1994, File
 */
P4_WORD_DEFINE(DELETE_FILE)
{
	P4_String name;

	if (P4_LENGTH(ctx->ds) < 4)
		p4Throw(ctx, P4_THROW_DS_UNDER);

	name.length = P4_POP(ctx->ds).u;
	name.string = P4_POP(ctx->ds).s;

	errno = 0;
	(void) unlink(name.string);

	P4_PUSH(ctx->ds).n = errno;
}

/**
 * ... RENAME-FILE ...
 *
 * (S: old u new u -- errno )
 *
 * @standard ANS-Forth 1994, File
 */
P4_WORD_DEFINE(RENAME_FILE)
{
	P4_String old_name, new_name;

	if (P4_LENGTH(ctx->ds) < 4)
		p4Throw(ctx, P4_THROW_DS_UNDER);

	new_name.length = P4_POP(ctx->ds).u;
	new_name.string = P4_POP(ctx->ds).s;
	old_name.length = P4_POP(ctx->ds).u;
	old_name.string = P4_POP(ctx->ds).s;

	errno = 0;
	(void) rename(old_name.string, new_name.string);

	P4_PUSH(ctx->ds).n = errno;
}

/**
 * ... USE filename ...
 *
 * (S: <spaces>filename -- )
 *
 * @standard extension
 */
P4_WORD_DEFINE(USE)
{
	P4_String path = p4ParseWord(&ctx->input);
	path.string[path.length] = '\0';
	P4_WORD_DO(FLUSH);
	free(ctx->block_file);
	ctx->block_file = strdup(path.string);
}

/**
 * ... USE filename ...
 *
 * (S: -- c-addr u )
 *
 * @standard extension
 */
P4_WORD_DEFINE(USING)
{
	p4GrowDS(ctx, 2);
	P4_PUSH(ctx->ds).s = ctx->block_file;
	P4_PUSH(ctx->ds).u = strlen(ctx->block_file);
}

/**
 * ... MAIN ...
 *
 * (S: -- ) (R: j*x -- )
 *
 * @standard internal
 */
static
P4_WORD_DEFINE(MAIN)
{
	P4_WORD_DO(LSQUARE);
	ctx->input.fd = fileno(ctx->input.fp);

	for (;;) {
		if (ctx->input.fd == 0 && P4_IS_INTERPRETING(ctx))
			fputs("ok ", stdout);

		P4_WORD_DO(REFILL);
		if (!P4_POP(ctx->ds).n)
			break;

		p4OnSignal(ctx);
		p4Interpret(ctx);
	}
}

/**
 * ... QUIT ...
 *
 * (S: -- ) (R: j*x -- )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(QUIT)
{
	/* SPECIAL case for Unix command line pipe filters
	 * allows us to say:
	 *
	 *	... | post4 -f script | ...
	 *
	 * If the last line of the source file ends with:
	 *
	 *	QUIT last_word\n<eof>
	 *
	 * Then we throw to a special version of QUIT that
	 * will restore standard input and parses the last
	 * word in the parse area _before_ resetting >IN to
	 * zero.
	 *
	 * It is up to the word executed to wait for SIGPIPE,
	 * terminate the process with <exit-code> BYE, or fall
	 * through to the interactive interpreter.
	 *
	 * QUIT normally resets the input source to standard
	 * input, so we can always read the next byte from a
	 * file to raise EOF, which is only flagged with an
	 * attempt to read beyond the end.
	 */
	if (P4_INPUT_IS_FILE(ctx) && fgetc(ctx->input.fp) == EOF) {
		P4_String last_word = p4ParseWord(&ctx->input);
		if (last_word.string[last_word.length] == '\n') {
			ctx->input.offset -= last_word.length + 1;
			LONGJMP(ctx->on_abort, P4_THROW_START);
		}
	}

	LONGJMP(ctx->on_abort, P4_THROW_QUIT);
}

/**
 * ... ABORT ...
 *
 * (S: i*x -- ) (R: j*x -- )
 *
 * @standard ANS-Forth 1994, Core
 */
P4_WORD_DEFINE(ABORT)
{
	LONGJMP(ctx->on_abort, P4_THROW_ABORT);
}

/**
 * ... xt CATCH ...
 *
 * (S: i*x xt -- j*x 0 | i*x n )
 *
 * @standard ANS-Forth 1994, Exception
 */
P4_WORD_DEFINE(CATCH)
{
	P4_Signed rc;
	P4_Input previous_source;
	P4_Unsigned ds_depth, rs_depth;

	/* P4_INPUT_PUSH not used here, since the restoration
	 * of the input source context is conditional on a
	 * THROW.
	 */
	previous_source = ctx->input;
	ds_depth = P4_LENGTH(ctx->ds);
	rs_depth = P4_LENGTH(ctx->rs);

	P4_SETJMP_PUSH(ctx, &ctx->on_throw);
	if ((rc = SETJMP(ctx->on_throw)) == 0) {
		ctx->jmp_set |= P4_JMP_THROW;
		P4_WORD_DO(EXECUTE);
	} else {
		P4_SET_DEPTH(ctx->ds, ds_depth);
		P4_SET_DEPTH(ctx->rs, rs_depth);
		ctx->input = previous_source;
	}
	P4_SETJMP_POP(ctx, &ctx->on_throw);

	P4_PUSH(ctx->ds).n = rc;
}

/**
 * ... value THROW ...
 *
 * (S: k*x n -- k*x | i*x n )
 *
 * @standard ANS-Forth 1994, Exception
 */
P4_WORD_DEFINE(THROW)
{
	p4Throw(ctx, P4_POP_SAFE(ctx->ds).n);
}

static const char help_summary[] =
"To quit send EOF, TERM interrupt signal, or BYE.\n"
"\n"
"User defined words are case sensitive, while core words are case insensitive.\n"
"\n"
"WORDS                  \\ view current list of words.\n"
"64 BYE                 \\ exit taking exit code from top of stack\n"
"1 2 + .                \\ reverse polish notation, ie. same as (1 + 2)\n"
"KEY EMIT               \\ get character stdin, write character stdout\n"
"16 OBASE ! 16 IBASE !  \\ set output and input base to hex (base 16)\n"
"IBASE @ .              \\ get input base and view using current output base\n"
".S .RS                 \\ view data and return stacks\n"
"\n"
;

/**
 * ... HELP ...
 *
 * (S: -- )
 *
 * @standard extension
 */
P4_WORD_DEFINE(HELP)
{
	fputs(help_summary, stdout);
}

/**
 * ... BYE ...
 *
 * (S: n -- )
 *
 * @standard ANS-Forth 1994, extended
 */
P4_WORD_DEFINE(BYE)
{
	P4_WORD_DO(FLUSH);
	LONGJMP(ctx->on_abort, P4_ABORT_BYE);
}

/**
 * ... CONTEXT ...
 *
 * (S: -- ctx )
 *
 * @standard extension
 */
P4_WORD_DEFINE(CONTEXT)
{
	P4_PUSH(ctx->ds).p = ctx;
}

/**
 * ... _forget ...
 *
 * @standard internal
 */
P4_WORD_DEFINE(_forget)
{
	P4_Size noname_depth;
	P4_Word *stop, *prev, *word;
	P4_Exec_Token xt = ctx->ip[-1];

	stop = ((P4_Cell *) xt->data->base)[0].w;
	noname_depth = ((P4_Cell *) xt->data->base)[1].u;

	while (noname_depth < P4_LENGTH(ctx->noname)) {
		xt = P4_POP(ctx->noname).xt;
		free(xt->data);
		free(xt);
	}

	for (word = ctx->words; word != stop; word = prev) {
		prev = word->prev;
		free(word);
	}

	ctx->words = stop;
}

/**
 * ... MARKER name ...
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(MARKER)
{
	P4_PUSH(ctx->ds).u = P4_LENGTH(ctx->noname);
	P4_PUSH(ctx->ds).w = ctx->words;

	P4_WORD_DO(CREATE);
	P4_WORD_DO(COMMA);
	P4_WORD_DO(COMMA);
	P4_WORD_DO(IMMEDIATE);

	ctx->words->xt->code = p4_do__forget;
}

P4_WORD_NAME(_lit,		NOOP,		0		);
P4_WORD_NAME(_var,		_lit,		0		);
P4_WORD_NAME(_does,		_var,		0		);
P4_WORD_NAME(_enter,		_does,		0		);
P4_WORD_NAME(_exit,		_enter,		0		);
P4_WORD_NAME(_forget,		_exit,		0		);
P4_WORD_NAME(ACCEPT,		_forget,	0		);
P4_WORD_NAME(ALIGN,		ACCEPT,		0		);
P4_WORD_NAME(ALLOCATE,		ALIGN,		0		);
P4_WORD_NAME(AND,		ALLOCATE,	0 		);
P4_WORD_NAME(BASE,		AND,		0 		);
P4_WORD_NAME(BLK,		BASE,		0		);
P4_WORD_NAME(CATCH,		BLK,		0		);
P4_WORD_NAME(CMOVE,		CATCH,		0		);
P4_WORD_NAME(CONSOLE,		CMOVE,		0		);
P4_WORD_NAME(CONTEXT,		CONSOLE,	0		);
P4_WORD_NAME(CREATE,		CONTEXT,	0		);
P4_WORD_NAME(DEPTH,		CREATE,		0		);
P4_WORD_NAME(DROP,		DEPTH,		0		);
P4_WORD_NAME(DUP,		DROP,		0		);
P4_WORD_NAME(DUMP,		DUP,		0 		);
P4_WORD_NAME(EMIT,		DUMP,		0 		);
P4_WORD_NAME(EVALUATE,		EMIT,		0		);
P4_WORD_NAME(EXECUTE,		EVALUATE,	0		);
P4_WORD_NAME(BYE,		EXECUTE,	0		);
P4_WORD_NAME(FREE,		BYE,		0		);
P4_WORD_NAME(HELP,		FREE,		0		);
P4_WORD_NAME(HERE,		HELP,		0		);
P4_WORD_NAME(IMMEDIATE,		HERE,		P4_BIT_IMM 	);
P4_WORD_NAME(INVERT,		IMMEDIATE,	0 		);
P4_WORD_NAME(INCLUDED,		INVERT,		0		);
P4_WORD_NAME(BRANCH,		INCLUDED,	0 		);
P4_WORD_NAME(BRANCHZ,		BRANCH,		0 		);
P4_WORD_NAME(JUMP,		BRANCHZ,	0 		);
P4_WORD_NAME(JUMPZ,		JUMP,		0 		);
P4_WORD_NAME(KEY,		JUMPZ,		0 		);
P4_WORD_NAME(LITERAL,		KEY,		P4_BIT_IMM	);
P4_WORD_NAME(LSHIFT,		LITERAL,	0		);
P4_WORD_NAME(MARKER,		LSHIFT,		0		);
P4_WORD_NAME(MOD,		MARKER,		0		);
P4_WORD_NAME(MOVE,		MOD,		0		);
P4_WORD_NAME(MS,		MOVE,		0		);
P4_WORD_NAME(NEGATE,		MS,		0		);
P4_WORD_NAME(OR,		NEGATE,		0 		);
P4_WORD_NAME(PARSE,		OR,		0		);
P4_WORD_NAME(PICK,		PARSE,		0		);
P4_WORD_NAME(QUIT,		PICK,		0		);
P4_WORD_NAME(REFILL,		QUIT,		0		);
P4_WORD_NAME(RESERVE,		REFILL,		0		);
P4_WORD_NAME(RESIZE,		RESERVE,	0		);
P4_WORD_NAME(ROLL,		RESIZE,		0		);
P4_WORD_NAME(RSHIFT,		ROLL,		0		);
P4_WORD_NAME(SOURCE,		RSHIFT,		0		);
P4_WORD_NAME(STATE,		SOURCE,		0		);
P4_WORD_NAME(SWAP,		STATE,		0		);
P4_WORD_NAME(THROW,		SWAP,		0		);
P4_WORD_NAME(TYPE,		THROW,		0		);
P4_WORD_NAME(UNUSED,		TYPE,		0		);
P4_WORD_NAME(WORDS,		UNUSED,		0		);
P4_WORD_NAME(XOR,		WORDS,		0		);

P4_WORD_NAME(PAD,		XOR,		0		);
P4_WORD_TEXT(SLASH_PAD,		PAD,		0,		"/PAD");

P4_WORD_NAME(BLOCK,		SLASH_PAD,	0		);
P4_WORD_TEXT(QM_BLOCKS,		BLOCK,		0,		"?BLOCKS");
P4_WORD_NAME(BUFFER,		QM_BLOCKS,	0		);
P4_WORD_TEXT(EMPTY_BUFFERS,	BUFFER,		0,		"EMPTY-BUFFERS");
P4_WORD_TEXT(SAVE_BUFFERS,	EMPTY_BUFFERS,	0,		"SAVE-BUFFERS");
P4_WORD_NAME(FLUSH,		SAVE_BUFFERS,	0		);
P4_WORD_NAME(LOAD,		FLUSH,		0		);
P4_WORD_NAME(UPDATE,		LOAD,		0		);
P4_WORD_TEXT(UPDATED_QM,	UPDATE,		0,		"UPDATED?");
P4_WORD_NAME(USE,		UPDATED_QM,	0		);
P4_WORD_NAME(USING,		USE,		0		);

P4_WORD_TEXT(DELETE_FILE,	USING,		0,		"DELETE-FILE");
P4_WORD_TEXT(RENAME_FILE,	DELETE_FILE,	0,		"RENAME-FILE");

P4_WORD_TEXT(ADD,		RENAME_FILE,	0, 		"+");
P4_WORD_TEXT(C_COMMA,		ADD,		0, 		"C,");
P4_WORD_TEXT(C_FETCH,		C_COMMA,	0, 		"C@");
P4_WORD_TEXT(C_STORE,		C_FETCH,	0,	 	"C!");
P4_WORD_TEXT(COLON,		C_STORE,	0, 		":");
P4_WORD_TEXT(COLON_NONAME,	COLON,		0, 		":NONAME");
P4_WORD_TEXT(COMMA,		COLON_NONAME,	0, 		",");
P4_WORD_TEXT(COMPILE_COMMA,	COMMA,		0, 		"COMPILE,");
P4_WORD_TEXT(CS_PICK,		COMPILE_COMMA,	0,	 	"CS-PICK");
P4_WORD_TEXT(CS_ROLL,		CS_PICK,	0,	 	"CS-ROLL");
P4_WORD_TEXT(DIV,		CS_ROLL,	0, 		"/");
P4_WORD_TEXT(DIV_MOD,		DIV,		0, 		"/MOD");
P4_WORD_TEXT(DOES_GT,		DIV_MOD,	P4_BIT_IMM,	"DOES>");
P4_WORD_TEXT(DOT_R,		DOES_GT,	0, 		".R");
P4_WORD_TEXT(DOT_S,		DOT_R,		0,		".S");
P4_WORD_TEXT(DOT_RS,		DOT_S,		0,		".RS");
P4_WORD_TEXT(FETCH,		DOT_RS,		0, 		"@");
P4_WORD_TEXT(FIND_WORD,		FETCH,		0, 		"FIND-WORD");
P4_WORD_TEXT(GT_BODY,		FIND_WORD,	0,	 	">BODY");
P4_WORD_TEXT(GT_HERE,		GT_BODY,	0,		">HERE");
P4_WORD_TEXT(GT_IN,		GT_HERE,	0,	 	">IN");
P4_WORD_TEXT(KEY_QM,		GT_IN,		0, 		"KEY?");
P4_WORD_TEXT(LSQUARE,		KEY_QM,		P4_BIT_IMM, 	"[");
P4_WORD_TEXT(MUL,		LSQUARE,	0, 		"*");
P4_WORD_TEXT(PARSE_ESCAPE,	MUL,		0,		"PARSE-ESCAPE");
P4_WORD_TEXT(PARSE_WORD,	PARSE_ESCAPE,	0,		"PARSE-WORD");
P4_WORD_TEXT(RSQUARE,		PARSE_WORD,	P4_BIT_IMM, 	"]");
P4_WORD_TEXT(RS_COPY,		RSQUARE,	0, 		"R@");
P4_WORD_TEXT(RS_GET,		RS_COPY,	0, 		"R>");
P4_WORD_TEXT(RS_PUT,		RS_GET,		0, 		">R");
P4_WORD_TEXT(RS_PICK,		RS_PUT,		0, 		"RS-PICK");
P4_WORD_TEXT(RS_ROLL,		RS_PICK,	0, 		"RS-ROLL");
P4_WORD_TEXT(SEMICOLON,		RS_ROLL,	P4_BIT_IMM,	";");
P4_WORD_TEXT(SLASH_CELL,	SEMICOLON,	0,		"/CELL");
P4_WORD_TEXT(SOURCE_ID,		SLASH_CELL,	0,		"SOURCE-ID");
P4_WORD_TEXT(STORE,		SOURCE_ID,	0, 		"!");
P4_WORD_TEXT(SUB,		STORE,		0, 		"-");
P4_WORD_TEXT(TICK,		SUB,		0, 		"'");
P4_WORD_TEXT(TICK_SEE,		TICK,		0, 		"'SEE");
P4_WORD_TEXT(TIME_DATE,		TICK_SEE,	0, 		"TIME&DATE");
P4_WORD_TEXT(TWO_DIV,		TIME_DATE,	0, 		"2/");
P4_WORD_TEXT(TWO_MUL,		TWO_DIV,	0, 		"2*");
P4_WORD_TEXT(TWO_RS_COPY,	TWO_MUL,	0,		"2R@");
P4_WORD_TEXT(TWO_RS_GET,	TWO_RS_COPY,	0,		"2R>");
P4_WORD_TEXT(TWO_RS_PUT,	TWO_RS_GET,	0,		"2>R");
P4_WORD_TEXT(U_DOT_R,		TWO_RS_PUT,	0, 		"U.R");
P4_WORD_TEXT(U_GT,		U_DOT_R,	0, 		"U>");
P4_WORD_TEXT(U_LT,		U_GT,		0, 		"U<");
P4_WORD_TEXT(ZERO_EQ,		U_LT,		0, 		"0=");
P4_WORD_TEXT(ZERO_LT,		ZERO_EQ,	0, 		"0<");
P4_WORD_NAME(ABORT,		ZERO_LT,	0		);

static const char p4_defined_words[] =
/**
 * value CONSTANT name
 *
 * (C: x "<spaces>name" -- ) // (S: -- x )
 *
 * @standard ANS-Forth 1994, Core
 */
": CONSTANT CREATE , DOES> @ ;\n"

/**
 * ... FALSE ...
 *
 * (S: -- 0 )
 *
 * @standard ANS-Forth 1994, Core
 */
"0 CONSTANT FALSE\n"

/**
 * ... TRUE ...
 *
 * (S: -- 1 )
 *
 * @standard ANS-Forth 1994, Core, extended
 */
"FALSE INVERT CONSTANT TRUE\n"

/**
 * ... BL ...
 *
 * (S: -- ' ' )
 *
 * @standard ANS-Forth 1994, Core
 */
"32 CONSTANT BL\n"

/**
 * ... /CHAR ...
 *
 * (S: -- ' ' )
 *
 * @standard extension
 */
"1 CONSTANT /CHAR\n"

#ifdef BACKSLASH
/**
 * ... \c ...
 *
 * (S: -- u )
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
 * @standard extension
 */
"7 CONSTANT \\a\n"				/* bell / alert */
"8 CONSTANT \\b\n"				/* backspace */
"9 CONSTANT \\t\n"				/* tab */
"10 CONSTANT \\n\n"				/* line-feed */
"11 CONSTANT \\v\n"				/* vertical-tab */
"12 CONSTANT \\f\n"				/* form-feed */
"13 CONSTANT \\r\n"				/* carriage-return */
"27 CONSTANT \\e\n"				/* escape */
"32 CONSTANT \\s\n"				/* space */
"127 CONSTANT \\?\n"				/* delete */
#endif

/**
 * VARIABLE name
 *
 * (C: "<spaces>name" -- ) // (S: -- a-addr )
 *
 * @standard ANS-Forth 1994, Core
 */
": VARIABLE CREATE 0 , ;\n"

/**
 * value VALUE name
 *
 * (C: x "<spaces>name" -- ) // (S: -- x )
 *
 * @note
 *	Similar definition to CONSTANT. Essentially VALUE when defined
 *	does:
 *
 *		VARIABLE name n name !
 *
 *	Referencing VALUE does:
 *
 *		name @
 *
 * @standard ANS-Forth 1994, Core
 *
 * @see
 *	TO
 */
": VALUE CREATE , DOES> @ ;\n"

/**
 *  ... ALIGNED ...
 *
 * (S: addr -- a-addr )
 *
 * @standard ANS-Forth 1994, Core
 */
": ALIGNED 1 - /CELL 1 - OR 1 + ;\n"

/**
 *  ... CHAR+ ...
 *
 * (S: c-addr1 -- c-addr2 )
 *
 * @standard ANS-Forth 1994, Core
 */
": CHAR+ /CHAR + ;\n"

/**
 *  ... CHARS ...
 *
 * (S: n1 -- n2 )
 *
 * @standard ANS-Forth 1994, Core
 */
#ifdef STRICT
": CHARS /CHAR * ;\n"
#else
": CHARS ;\n"
#endif

/**
 *  ... CELL+ ...
 *
 * (S: a-addr1 -- a-addr2 )
 *
 * @standard ANS-Forth 1994, Core
 */
": CELL+ /CELL + ;\n"

/**
 *  ... CELLS ...
 *
 * (S: n1 -- n2 )
 *
 * @standard ANS-Forth 1994, Core
 */
": CELLS /CELL * ;\n"

/**
 *  ... OVER ...
 *
 * (S: x1 x2 -- x1 x2 x1 )
 *
 * @standard ANS-Forth 1994, Core
 */
": OVER 1 PICK ;\n"

/**
 *  ... NIP ...
 *
 * (S: x1 x2 -- x2 )
 *
 * @standard ANS-Forth 1994, Core
 */
": NIP SWAP DROP ;\n"

/**
 *  ... TUCK ...
 *
 * (S: x1 x2 -- x2 x1 x2 )
 *
 * @standard ANS-Forth 1994, Core
 */
": TUCK SWAP OVER ;\n"

/**
 *  ... 2DUP ...
 *
 * (S: x1 x2 -- x1 x2 x1 x2 )
 *
 * @standard ANS-Forth 1994, Core
 */
": 2DUP OVER OVER ;\n"

/**
 *  ... 2DROP ...
 *
 * (S: x1 x2 -- )
 *
 * @standard ANS-Forth 1994, Core
 */
": 2DROP DROP DROP ;\n"

/**
 *  ... 2! ...
 *
 * (S: x y a-addr -- )
 *
 * @standard ANS-Forth 1994, Core
 */
": 2! SWAP OVER ! CELL+ ! ;\n"

/**
 *  ... 2@ ...
 *
 * (S: a-addr -- x y )
 *
 * @standard ANS-Forth 1994, Core
 */
": 2@ DUP CELL+ @ SWAP @ ;\n"

/**
 *  ... +!...
 *
 * (S: nu a-addr -- )
 *
 * @standard ANS-Forth 1994, Core
 */
": +!"
	" DUP @"			/* (S: nu a-addr nu' ) */
	" 2 ROLL"			/* (S: a-addr nu' nu ) */
	" + SWAP ! ;\n"			/* (S: -- */

/**
 *  ... 1+ ...
 *
 * (S: nu1 -- nu2 )
 *
 * @standard ANS-Forth 1994, Core
 */
": 1+ 1 + ;\n"

/**
 *  ... 1- ...
 *
 * (S: nu1 -- nu2 )
 *
 * @standard ANS-Forth 1994, Core
 */
": 1- 1 - ;\n"

/**
 *  ... 0<> ...
 *
 * (S: n -- flag )
 *
 * @standard ANS-Forth 1994, Core
 */
": 0<> 0= 0= ;\n"

/**
 *  ... 0> ...
 *
 * (S: n -- flag )
 *
 * @standard ANS-Forth 1994, Core
 */
": 0> DUP 0= SWAP 0< OR 0= ;\n"

/**
 *  ... = ...
 *
 * (S: n1 n2 -- flag )
 *
 * @standard ANS-Forth 1994, Core
 */
": = - 0= ;\n"

/**
 *  ... <> ...
 *
 * (S: n1 n2 -- flag )
 *
 * @standard ANS-Forth 1994, Core
 */
": <> = 0= ;\n"

/**
 *  ... < ...
 *
 * (S: n1 n2 -- flag )
 *
 * @standard ANS-Forth 1994, Core
 */
": < - 0< ;\n"

/**
 *  ... > ...
 *
 * (S: n1 n2 -- flag )
 *
 * @standard ANS-Forth 1994, Core
 */
": > - 0> ;\n"

/**
 *  ... <= ...
 *
 * (S: n1 n2 -- flag )
 *
 * @standard extension
 */
": <= - DUP 0= SWAP 0< OR ;\n"

/**
 *  ... >= ...
 *
 * (S: n1 n2 -- flag )
 *
 * @standard extension
 */
": >= < 0= ;\n"

/**
 *  ... WITH-IN ...
 *
 * (S: nu1 nu2 nu3 -- flag )
 *
 * @note
 *	True if nu2 <= nu1 < nu3, otherwise false.
 *
 * @standard extension
 */
": WITH-IN 2 PICK > >R >= R> AND ;\n"

/**
 *  ... WITH-OUT ...
 *
 * (S: nu1 nu2 nu3 -- flag )
 *
 * @note
 *	True if nu1 < nu2 or nu3 <= n1, otherwise false.
 *
 * @standard extension
 */
": WITH-OUT 2 PICK <= >R < R> OR ;\n"

/**
 * ... CR ...
 *
 * (S: -- )
 *
 * @standard ANS-Forth 1994, Core
 */
": CR \\r EMIT \\n EMIT ;\n"

/**
 * ... SPACE ...
 *
 * (S: -- )
 *
 * @standard ANS-Forth 1994, Core
 */
": SPACE BL EMIT ;\n"

/**
 * ... . ...
 *
 * (S: n -- )
 *
 * @standard ANS-Forth 1994, Core
 */
": . 0 .R SPACE ;\n"

/**
 *  ...  U.  ...
 *
 * (S: u -- )
 *
 * @standard ANS-Forth 1994, Core
 */
": U. 0 U.R SPACE ;\n"

/**
 *  ...  POSTPONE  ...
 *
 * (C: "<spaces>name" -- )
 *
 * @standard ANS-Forth 1994, Core
 */
": POSTPONE PARSE-WORD FIND-WORD DROP COMPILE, ; IMMEDIATE\n"

/**
 *  ...  CHAR  ...
 *
 * (S: "<spaces>name" -- char )
 *
 * @standard ANS-Forth 1994, Core
 */
": CHAR PARSE-WORD DROP C@ ;\n"

/**
 *  ...  U.  ...
 *
 * (C: "<spaces>name" -- ) // (S: -- char )
 *
 * @standard ANS-Forth 1994, Core
 */
": [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE\n"

/**
 * ... POSTPONE name ...
 *
 * (C: "<spaces>name" -- ) // (S: -- xt )
 *
 * @standard ANS-Forth 1994, Core
 */
": ['] ' POSTPONE LITERAL ; IMMEDIATE\n"

/**
 * ... BEGIN ... AGAIN
 *
 * ... BEGIN ... test UNTIL ...
 *
 * ... BEGIN ... test WHILE ... REPEAT ...
 *
 * (C: -- dest )
 *
 * @standard ANS-Forth 1994, Core
 *
 * @see
 *	A.3.2.3.2 Control-flow stack
 */
": BEGIN >HERE ; IMMEDIATE\n"

/**
 * ... BEGIN ... AGAIN
 *
 * (C: dest -- )
 *
 * @standard ANS-Forth 1994, Core
 *
 * @see
 *	A.3.2.3.2 Control-flow stack
 */
": AGAIN ['] BRANCH COMPILE, >HERE - , ; IMMEDIATE\n"

/**
 * ... BEGIN ... test UNTIL ...
 *
 * (C: dest -- ) // (S: flag -- )
 *
 * @standard ANS-Forth 1994, Core
 *
 * @see
 *	A.3.2.3.2 Control-flow stack
 */
": UNTIL ['] BRANCHZ COMPILE, >HERE - , ; IMMEDIATE\n"

/**
 * ... AHEAD ... THEN ...
 *
 * (C: -- forw )
 *
 * @standard ANS-Forth 1994, Core
 *
 * @see
 *	A.3.2.3.2 Control-flow stack
 */
": AHEAD ['] BRANCH COMPILE, >HERE 0 , ; IMMEDIATE\n"

/**
 * ... test IF ... THEN ...
 *
 * ... test IF ... ELSE ... THEN ...
 *
 * (C: -- forw ) // (S: flag -- )
 *
 * @standard ANS-Forth 1994, Core
 *
 * @see
 *	A.3.2.3.2 Control-flow stack
 */
": IF ['] BRANCHZ COMPILE, >HERE 0 , ; IMMEDIATE\n"

/**
 * ... AHEAD ... THEN ...
 *
 * ... test IF ... THEN ...
 *
 * ... test IF ... ELSE ... THEN ...
 *
 * (C: forw -- )
 *
 * @standard ANS-Forth 1994, Core
 *
 * @see
 *	A.3.2.3.2 Control-flow stack
 */
": THEN"				/* C: forw_off */
	" >HERE SWAP"			/* C: here_off forw_off */
	" -"				/* C: pos_off */
	" HERE OVER"			/* C: pos_off addr pos_off */
	" -"				/* C: pos_off forw_addr */
	" !"				/* C: -- */
	" ; IMMEDIATE\n"

/**
 * ... test IF ... ELSE ... THEN ...
 *
 * (C: forw1 -- forw2 )
 *
 * @standard ANS-Forth 1994, Core
 *
 * @see
 *	A.3.2.3.2 Control-flow stack
 */
": ELSE"				/* C: forw1 */
	" POSTPONE AHEAD"		/* C: forw1 forw2 */
	" 1 CS-ROLL"			/* C: forw2 forw1 */
	" POSTPONE THEN"		/* C: forw2 */
	" ; IMMEDIATE\n"

/**
 * ... BEGIN ... test WHILE ... REPEAT ...
 *
 * (C: dest -- forw dest ) // (S: flag -- )
 *
 * ... BEGIN ... test WHILE ... test WHILE ... REPEAT THEN ...
 *
 * ... BEGIN ... test WHILE ... test WHILE ... AGAIN THEN THEN ...
 *
 * Multiple WHILE possible to provide short-circuit testing, but each
 * additional WHILE needs a THEN in order to resolve each forward
 * reference remaining on the stack.
 *
 * @standard ANS-Forth 1994, Core
 *
 * @see
 *	A.3.2.3.2 Control-flow stack
 */
": WHILE"				/* C: dest */
	" POSTPONE IF"			/* C: dest forw */
	" 1 CS-ROLL"			/* C: forw dest */
	" ; IMMEDIATE\n"

/**
 * ... BEGIN ... test WHILE ... REPEAT ...
 *
 * (C: forw dest -- )
 *
 * @standard ANS-Forth 1994, Core
 *
 * @see
 *	A.3.2.3.2 Control-flow stack
 */
": REPEAT"				/* C: forw dest */
	" POSTPONE AGAIN"		/* C: forw */
	" POSTPONE THEN"		/* C: -- */
	" ; IMMEDIATE\n"

/**
 * ... x TO name ...
 *
 * (S: x "<spaces>name" -- )
 *
 * @note
 *	This definitioon relies on how DOES> is compiled. Semantically
 *	similar to:
 *
 *		x name !
 *
 * @standard ANS-Forth 1994, Core, Local
 *
 * @see
 *	TO
 */
": TO"
	" '"				/* S: xt */
	" >BODY @ >BODY"		/* S: a-addr */
	" STATE @"			/* S: a-addr bool */
	" IF"				/* S: a-addr */
	 " POSTPONE LITERAL"		/* S: -- */
	 " ['] ! COMPILE,"
	" ELSE"
	 " !"				/* S: -- */
	" THEN"
	" ; IMMEDIATE\n"

/**
 * : X ... test IF ... EXIT THEN ... ;
 *
 * (S: -- ) (R: word_caller exit_caller -- )
 *
 * @standard ANS-Forth 1994, Core
 */
": EXIT R> DROP R> CONTEXT ! ;\n"

/**
 *  ... \ comment to end of line
 *
 * (S: "ccc<eol>" -- )
 *
 * @standard ANS-Forth 1994, Core, Block
 */
": \\"
	" BLK @"
	" IF"				/* Block input source? */
	 " >IN @ $3F OR 1+ >IN !"	/*   Advance >IN to next "line" in 16x64 block. */
	" ELSE"				/* Streaming input... */
	 " \\n PARSE 2DROP"		/*   Skip up to and including newline. */
	" THEN"
	" ; IMMEDIATE\n"

/**
 *  ... ( comment) ...
 *
 * (S: "ccc<paren>" -- )
 *
 * @standard ANS-Forth 1994, Core, File
 */
": ("
	" BEGIN"
	 " [CHAR] ) DUP PARSE"			/* S: paren caddr u */
	 " + C@" 				/* S: paren char */
	 " ="					/* S: flag */
	 " IF EXIT THEN"
	 " REFILL 0="				/* S: flag */
	" UNTIL"
	" ; IMMEDIATE\n"

/**
 * ... .( ccc) ...
 *
 * (S: "ccc<paren>" -- )
 *
 * @standard ANS-Forth 1994, Core, extended
 */
": .("
	" BEGIN"
	 " [CHAR] ) DUP PARSE-ESCAPE"		/* S: paren caddr u */
	 " 2DUP TYPE"				/* S: paren caddr u */
	 " + C@" 				/* S: paren char */
	 " ="					/* S: flag */
	 " IF EXIT THEN"
	 " REFILL 0="				/* S: flag */
	" UNTIL"
	" ; IMMEDIATE\n"

/**
 * ... limit first DO ... LOOP ...
 *
 * (C: -- count dest ) // (S: limit first -- ) (R: -- limit first )
 *
 * @standard ANS-Forth 1994, Core
 */
": DO"					/* C: -- // S: limit first R: -- */
	" ['] 2>R COMPILE,"		/* C: -- // S: -- R: limit first */
	" 0"				/* C: 0 */
	" POSTPONE BEGIN"		/* C: 0 dest */
	" ; IMMEDIATE\n"

/**
 * ... limit first ?DO ... LOOP ...
 *
 * (C: -- count dest ) // (S: limit first -- ) (R: -- limit first )
 *
 * @standard ANS-Forth 1994, Core
 */
": ?DO"					/* C: -- // S: limit first R: -- */
	" ['] 2>R COMPILE,"		/* C: -- // S: -- R: limit first */
	" ['] 2R@ COMPILE,"		/* C: -- // S: limit first R: limit first */
	" ['] <> COMPILE,"		/* C: -- // S: flag R: limit first */
	" POSTPONE IF 1"		/* C: forw 1 // S: -- R: limit first */
	" POSTPONE BEGIN"		/* C: forw 1 dest */
	" ; IMMEDIATE\n"

/**
 * : X ... limit first DO ... test IF ... UNLOOP EXIT THEN ... LOOP ... ;
 *
 * (S: --  ) (R: limit index ip -- ip )
 *
 * @standard ANS-Forth 1994, Core
 */
": UNLOOP R> 2R> 2DROP >R ;\n"

/**
 * ... limit first DO ... IF ... LEAVE THEN ... LOOP ...
 *
 * (C: forw1 ... count dest -- forw1 ... forwN count' dest )
 * // (S: -- ) (R: loop-sys -- )
 *
 * @standard ANS-Forth 1994, Core
 *
 * @note
 *	This code assumes that LEAVE appears only within an
 *	IF-ELSE-THEN block, which while compiling has a forw
 *	reference on the compilation stack.
 */
": LEAVE"				/* C: forw1 ... count dest forw R: --  */
	" 2>R >R"			/* C: forw1 ... R: dest forw count */
	" POSTPONE AHEAD"		/* C: forw1 ... forwN R: dest forw count */
	" R>"				/* C: forw1 ... forwN count R: dest forw */
	" 1+"				/* C: forw1 ... forwN count' R: dest forw */
	" 2R>"				/* C: forw1 ... forwN count' dest forw R: -- */
	" ; IMMEDIATE\n"

/**
 * ... limit first DO ... LOOP ...
 *
 * (S: n --  ) (R: limit index ip -- limit index' ip )
 *
 * @standard internal
 */
": _loop"				/* R: limit index ip S: n */
	" R> 2R>"			/* R: -- S: n ip limit index */
	" 3 ROLL"			/* R: -- S: ip limit index n */
	" +"				/* R: -- S: ip limit index' */
	" 2DUP 2>R"			/* R: limit index' S: ip limit index' */
	" ="				/* R: limit index' S: ip flag */
	" SWAP >R"			/* R: limit index' ip S: flag */
	" ;\n"

/**
 * ... limit first DO ... LOOP ...
 *
 * (C: forw1 ... forwN count dest -- ) // (S: n -- ) (R: loop-sys1 -- | loop-sys2 )
 *
 * @standard ANS-Forth 1994, Core
 */
": +LOOP"				/* C: forw1 ... forwN count dest */
	/* Loop increment and test. */
	" ['] _loop COMPILE,"
	" POSTPONE UNTIL"		/* C: forw1 ... forwN count */

	/* Resolve LEAVE forward references. */
	" BEGIN"			/* C: forw1 ... forwN count */
	 " DUP 0<> WHILE"		/* C: forw1 ... forwN count flag */
	 " 1-"				/* C: forw1 ... forwN count' */
	 " SWAP"			/* C: forw1 ... count' forwN */
	 " POSTPONE THEN"		/* C: forw1 ... count' */
	" REPEAT"			/* C: count' */
	" DROP"				/* C: -- */

	/* LEAVE branches to just after UNTIL and before UNLOOP. */
	" ['] UNLOOP COMPILE,"

	" ; IMMEDIATE\n"

/**
 * ... limit first DO ... LOOP ...
 *
 * (C: dest -- ) // (S: --  ) (R: loop-sys1 -- | loop-sys2 )
 *
 * @standard ANS-Forth 1994, Core
 */
": LOOP 1 POSTPONE LITERAL POSTPONE +LOOP ; IMMEDIATE\n"

/**
 * ... limit first DO ... LOOP ...
 *
 * (S: -- index ) (R: limit index ip -- limit index ip )
 *
 * @standard ANS-Forth 1994, Core
 */
": I 1 RS-PICK R> ;\n"

/**
 * ... limit first DO ... LOOP ...
 *
 * (S: -- index1 ) (R: limit1 index1 limit2 index2 ip -- limit1 index1 limit2 index2 ip )
 *
 * @standard ANS-Forth 1994, Core
 */
": J 3 RS-PICK R> ;\n"

/**
 * ... _slit ...
 *
 * (S: -- c-addr u )
 *
 * @standard internal
 *
 * @note
 *	The caller's return address is used to find and compute the
 *	address and length of the string stored within the word.
 *	It is then modified point to just after the string.
 */
": _slit"					/* S: -- R: ip */
	" R@"					/* S: ip R: ip */
	" @"					/* S: u R: ip */
	" R> CELL+"				/* S: u caddr R: -- */
	" SWAP 2DUP 1+ ALIGNED +"		/* S: caddr u ip' R: -- */
	" >R"					/* S: caddr u R: ip' */
	" ;\n"

/**
 * ... SLITERAL ...
 *
 * (C: c-addr u -- ) // (S: -- c-addr u )
 *
 * @standard ANS-Forth 1994, String, extended
 *
 * @note
 *	The string saved into the word is NUL terminated
 *	so it can be used by C string functions.
 */
": SLITERAL" 					/* C: caddr u */
	" DUP >R"				/* C: caddr u R: u */
	" ['] _slit COMPILE, DUP ,"		/* C: caddr u R: u */
	" DUP 1+ ALIGNED"			/* C: caddr u u' R: u */
	" RESERVE DUP >R SWAP"			/* C: caddr a-addr u R: u a-addr */
	" MOVE"					/* C: -- R: u a-addr */
	" 0 2R> + C!"				/* C: -- R: -- */
	" ; IMMEDIATE\n"

/**
 * ... S" ccc" ...
 *
 * (C: "ccc<quote>" -- ) // (S: -- c-addr u )
 *
 * @standard ANS-Forth 1994, Core, File, extended
 */
": S\""
	" [CHAR] \" PARSE-ESCAPE"		/* S: caddr u */
	" STATE @"				/* S: caddr u flag */
	" IF POSTPONE SLITERAL THEN"		/* S: caddr u | -- */
	" ; IMMEDIATE\n"

/**
 * ... ." ccc" ...
 *
 * (S: "ccc<quote>" -- )
 *
 * @standard ANS-Forth 1994, Core, extended
 */
": .\" POSTPONE S\" ['] TYPE COMPILE, ; IMMEDIATE\n"

/**
 * ... MAX ...
 *
 * (S: n1 n2 -- n3 )
 *
 * @standard ANS-Forth 1994, Core
 */
": MAX 2DUP < IF SWAP THEN DROP ;\n"

/**
 * ... MIN ...
 *
 * (S: n1 n2 -- n3 )
 *
 * @standard ANS-Forth 1994, Core
 */
": MIN 2DUP > IF SWAP THEN DROP ;\n"

/**
 * ... ABS ...
 *
 * (S: n -- u )
 *
 * @standard ANS-Forth 1994, Core
 */
": ABS DUP 0< IF NEGATE THEN ;\n"

/**
 * ... ROT ...
 *
 * (S: a b c -- b c a )
 *
 * @standard ANS-Forth 1994, Core
 */
": ROT 2 ROLL ;\n"

/**
 * ... ?DUP ...
 *
 * (S: x -- 0 | x x )
 *
 * @standard ANS-Forth 1994, Core
 */
": ?DUP DUP 0<> IF DUP THEN ;\n"

/**
 * ... SPACES ...
 *
 * (S: n -- )
 *
 * @standard ANS-Forth 1994, Core
 */
": SPACES 0 ?DO SPACE LOOP ;\n"

/**
 * ... FIND ...
 *
 * (S: c-addr -- c-addr 0 | xt 1 | xt -1)
 *
 * @standard ANS-Forth 1994, Core
 */
": FIND"				/* S: c- */
	" DUP C@"			/* S: c- u */
	" SWAP CHAR+ SWAP"		/* S: c-' u */
	" FIND-WORD"			/* S: c-' u 0 | xt 1 | xt -1 */
	" DUP 0= IF NIP THEN"		/* S: c- 0 | xt 1 | xt -1 */
	" ;\n"

/**
 * ... AT-XY ...
 *
 * (S: column row -- )
 *
 * @standard ANS-Forth 1994, Facility
 *
 * @note
 *	ANSI / VT100 terminal assumed.
 */
": AT-XY"
	" S\" \033[\" TYPE"
	" 1+ 0 U.R"
	" [CHAR] ; EMIT"
	" 1+ 0 U.R"
	" [CHAR] H EMIT"
	" ;\n"

/**
 * ... PAGE ...
 *
 * (S: -- )
 *
 * @standard ANS-Forth 1994, Facility
 *
 * @note
 *	ANSI / VT100 terminal assumed.
 */
": PAGE 0 0 AT-XY S\" \033[0J\" TYPE ;\n"

/**
 * ... ? ...
 *
 * (S: a-addr -- )
 *
 * @standard ANS-Forth 1994, Tools
 */
": ?"
	" 16 BASE !"
	" DUP .\" 0x\" . SPACE SPACE @"
	" DUP .\" 0x\" ."
	" DUP 10 BASE ! [CHAR] # EMIT ."
	" 8 BASE ! [CHAR] 0 EMIT ."
	" 10 BASE ! CR ;\n"

/**
 * ... SEE ...
 *
 * (S: "<spaces>name" -- )
 *
 * @standard ANS-Forth 1994, Tools
 */
": SEE"
	" PARSE-WORD FIND-WORD"
	" IF 'SEE ELSE 2DROP THEN"
	" ; IMMEDIATE\n"

/**
 * ... FILL ...
 *
 * (S: c-addr u char -- )
 *
 * @standard ANS-Forth 1994, String
 */
": FILL"
	" 2 PICK"   			/* S: c- u c c- */
	" ! 1- OVER"			/* S: c- u' c- */
	" 1+ SWAP"  			/* S: c- c-' u */
	" CMOVE"    			/* S: -- */
	" ;\n"

/**
 * ... BLANK ...
 *
 * (S: c-addr u -- )
 *
 * @standard ANS-Forth 1994, String
 */
": BLANK BL FILL ;\n"

/**
 * ... INCLUDE filename ...
 *
 * (S: "<spaces>filename" -- )
 *
 * @standard extension
 */
": INCLUDE PARSE-WORD INCLUDED ;\n"

/**
 * ... SCR ...
 *
 * (S: -- a-addr )
 *
 * @standard ANS-Forth 1994, Block
 */
"VARIABLE SCR 0 SCR !\n"

/**
 * ... LIST ...
 *
 * (S: u -- )
 *
 * @standard ANS-Forth 1994, Block
 */
": LIST"					/* S: u */
	" DUP SCR !"				/* S: u */
	" BLOCK"				/* S: a-addr */
	" 16 0 DO"
	 " I 2 .R"
	 " [CHAR] | EMIT"
	 " DUP 64 TYPE"				/* S: a-addr */
	 " [CHAR] | EMIT CR"
	 " 64 CHARS +"				/* S: a-addr' */
	" LOOP DROP"				/* S: -- */
	" ;\n"

/**
 * ... LIST+ ...
 *
 * (S: -- )
 *
 * @standard extension
 */
": LIST+ SCR @ 1+ LIST ;\n"

/**
 * ... THRU ...
 *
 * (S: start end -- )
 *
 * @standard ANS-Forth 1994, Block
 */
": THRU"					/* S: start end */
	" 1+ SWAP"				/* S: end' start */
	" DO"					/* S: -- */
	 " I LOAD"
	" LOOP"
	" ;\n"

/**
 * ... START word ...
 *
 * Special case of QUIT that emptys the return stack, set
 * the input source to standard input, but _not_ set >IN
 * to zero, then execute the next word. Once completed,
 * >IN is set to zero and the remainder of QUIT semantics
 * are done.
 *
 * @standard extension
 */
": START -5656 THROW ;\n"

/**
 * : X ... test ABORT" message" ...
 *
 * (C: "ccc<quote>" -- ) // (S: i*x x1 --  | i*x ) ( R: j*x --  | j*x )
 *
 * @standard ANS-Forth 1994, extended
 */
": ABORT\" POSTPONE IF POSTPONE .\" -2 ['] THROW COMPILE, POSTPONE THEN ; IMMEDIATE\n"
;

static P4_Word *p4_core_words;

/***********************************************************************
 *** Core
 ***********************************************************************/

void
p4Fini(void)
{
#ifdef HAVE_TCSETATTR
	if (0 <= tty_fd)
		(void) tcsetattr(tty_fd, TCSADRAIN, &tty_saved);
#endif
}

/**
 */
void
p4Init(void)
{
	char *p;
	unsigned i;

	setvbuf(stdout, NULL, _IOLBF, 0);
	setvbuf(stderr, NULL, _IOLBF, 0);

	/* Remember the split between the program and its static
	 * data and data dynamically allocated later. This can be
	 * used to distinguish between core and colon words.
	 */
	p4_program_end = sbrk(0);

	/* Setup base 36 number conversion table. */
	memset(base36, 0xFF, sizeof (base36));
	for (i = 0, p = base36_digits; *p != '\0'; p++, i++)
		base36[toupper(*p)] = i;
	for (i = 0, p = base36_digits; *p != '\0'; p++, i++)
		base36[tolower(*p)] = i;

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
		 * set of patterns. ECHO is turned back one for
		 * line input like REFILL or ACCEPT.
		 */
//		(void) tcsetattr(tty_fd, TCSADRAIN, &tty_raw);
	}
#endif /* HAVE_TCGETATTR */
}

int
p4Evaluate(P4_Context *ctx)
{
	int rc = -1;
	P4_Byte buffer[P4_INPUT_SIZE];

	P4_SETJMP_PUSH(ctx, &ctx->on_abort);

	switch (rc = SETJMP(ctx->on_abort)) {
	case P4_ABORT_BYE:
		break;

	case P4_THROW_ABORT:
	case P4_THROW_ABORT_MSG:
		P4_RESET(ctx->ds);
		/*@fallthrough*/

	case P4_THROW_QUIT:
	case P4_THROW_START:
		P4_RESET(ctx->rs);

		if (P4_INPUT_IS_FILE(ctx))
			fclose(ctx->input.fp);

		ctx->input.blk = 0;
		ctx->input.fp = stdin;
		ctx->input.fd = fileno(stdin);

		if (rc == P4_THROW_START)
			p4Interpret(ctx);
		/*@fallthrough*/

	default:
		ctx->jmp_set |= P4_JMP_ABORT;
		ctx->input.buffer = buffer;
		ctx->input.offset = 0;

		P4_WORD_DO(MAIN);
		rc = 0;
	}

	P4_SETJMP_POP(ctx, &ctx->on_abort);

	return rc;
}

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
int
p4EvalFd(P4_Context *ctx, P4_Signed fd)
{
	int rc;

	if (ctx == NULL || fd < 0) {
		errno = EFAULT;
		return -9;
	}

	P4_INPUT_PUSH(&ctx->input);
	rc = p4Evaluate(ctx);
	P4_INPUT_POP(&ctx->input);

	return rc;
}

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
int
p4EvalFile(P4_Context *ctx, const char *file)
{
	int rc;

	if (ctx == NULL || file == NULL) {
		errno = EFAULT;
		return -9;
	}

	P4_INPUT_PUSH(&ctx->input);

	if ((ctx->input.fp = fopen(file, "r")) == NULL) {
		rc = errno == ENOENT ? P4_THROW_ENOENT : P4_THROW_EIO;
	} else {
		rc = p4Evaluate(ctx);
		if (P4_INPUT_IS_FILE(ctx))
			(void) fclose(ctx->input.fp);
	}

	P4_INPUT_POP(&ctx->input);

	return rc;
}

int
p4EvalString(P4_Context *ctx, const char *string)
{
	int rc;

	P4_SETJMP_PUSH(ctx, &ctx->on_abort);

	if ((rc = SETJMP(ctx->on_abort)) == 0) {
		ctx->jmp_set |= P4_JMP_ABORT;

		P4_PUSH(ctx->ds).p = (P4_Pointer) string;
		P4_PUSH(ctx->ds).u = strlen(string);
		P4_WORD_DO(EVALUATE);
	}

	P4_SETJMP_POP(ctx, &ctx->on_abort);

	return rc;
}

/**
 * Create a new interpreter context.
 *
 * @return
 *	A pointer to an allocated P4_Context structure.
 */
P4_Context *
p4Create(void)
{
	P4_Context *ctx;

	if ((ctx = calloc(1, sizeof (*ctx))) == NULL)
		goto error0;

	if (p4ArrayAssign(&ctx->rs, P4_STACK_SIZE))
		goto error1;

	if (p4ArrayAssign(&ctx->ds, P4_STACK_SIZE))
		goto error1;

	if (p4ArrayAssign(&ctx->noname, P4_STACK_SIZE))
		goto error1;

	if ((ctx->block_file = strdup(P4_BLOCK_FILE)) == NULL)
		goto error1;

	ctx->base = 10;
	ctx->unget = EOF;
	ctx->input.fp = stdin;
	ctx->input.fd = fileno(stdin);
	ctx->input.buffer = ctx->console;

	if (p4_core_words == NULL) {
		/* Use first context to compile the globally defined core words.  */
		ctx->words = &p4_word_ABORT;
		(void) p4EvalString(ctx, p4_defined_words);
		p4_core_words = ctx->words;
	}

	ctx->words = p4_core_words;

	return ctx;
error1:
	p4Free(ctx);
error0:
	return NULL;
}

/**
 * @param _ctx
 *	A pointer to an allocated P4_Context structure to free.
 */
void
p4Free(void *_ctx)
{
	P4_Exec_Token xt;
	P4_Word *prev, *word;
	P4_Context *ctx = _ctx;

	if (ctx != NULL) {
		if (ctx->input.fp != NULL && ctx->input.fp != stdin)
			fclose(ctx->input.fp);

		while (!P4_IS_EMPTY(ctx->noname)) {
			xt = P4_POP(ctx->noname).xt;
			free(xt->data);
			free(xt);
		}

		for (word = ctx->words; word != p4_core_words; word = prev) {
			prev = word->prev;
			free(word);
		}

		p4ArrayRelease(&ctx->noname);
		p4ArrayRelease(&ctx->ds);
		p4ArrayRelease(&ctx->rs);
		free(ctx->block_file);
		free(ctx);
	}
}

#ifdef TEST
/***********************************************************************
 *** Main
 ***********************************************************************/

static char usage[] =
"usage: " P4_NAME " [-e string]... [-f file]... [args ...]\n"
"\n"
"-e string\tevaluate string\n"
"-f file\t\tevaluate input file\n"
"\n"
P4_NAME "/" P4_VERSION " " P4_COPYRIGHT "\n"
"Built " P4_BUILT "\n"
;

static P4_Context *ctx;

static void
sig_int(int signum)
{
	ctx->signal = signum;
}

int
main(int argc, char **argv)
{
	char *arg;
	int argi, rc;

	p4Init();
	rc = EXIT_FAILURE;
	(void) atexit(p4Fini);
	(void) signal(SIGINT, sig_int);
	(void) signal(SIGQUIT, sig_int);
//	(void) signal(SIGSEGV, sig_int);

	if ((ctx = p4Create()) == NULL) {
		fprintf(stderr, "initialisation error\n");
		goto error0;
	}

	for (argi = 1; argi < argc; argi++) {
		if (argv[argi][0] != '-' || (argv[argi][1] == '-' && argv[argi][2] == '\0'))
			break;

		switch (argv[argi][1]) {
		case 'e':
			arg = argv[argi][2] == '\0' ? argv[++argi] : &argv[argi][2];
			if (arg != NULL && p4EvalString(ctx, arg) == 1)
				goto error2;
			continue;

		case 'f':
			arg = argv[argi][2] == '\0' ? argv[++argi] : &argv[argi][2];
			if (arg != NULL && p4EvalFile(ctx, arg) == 1)
				goto error2;
			continue;
		}

		if (argv[argi] == NULL)
			fprintf(stderr, "missing option argument\n%s", usage);
		else
			fprintf(stderr, "invalid option -%c\n%s", argv[argi][1], usage);
		goto error1;
	}

//	p4Arguments(ctx, argc - argi, argv + argi);

	(void) p4Evaluate(ctx);
error2:
	rc = P4_POP_SAFE(ctx->ds).n;
error1:
	p4Free(ctx);
error0:
	return rc;
}

#endif /* TEST */
