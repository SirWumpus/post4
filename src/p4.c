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
 *	The ASCII value that the backslash escape character represents.
 */
int
p4CharLiteral(int ch)
{
	switch (ch) {
	case 'a': return '\007';	/* bell */
	case 'b': return '\010';	/* backspace */
	case 'e': return '\033';	/* escape */
	case 'f': return '\014';	/* formfeed */
	case 'n': return '\012';	/* linefeed */
	case 'r': return '\015';	/* carriage-return */
	case 's': return '\040';	/* space */
	case 't': return '\011';	/* tab */
	case 'v': return '\013';	/* vertical tab */
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
 * @return
 *	A number.
 */
P4_Signed
p4StringToSigned(const P4_Byte *s, P4_Byte **stop, P4_Unsigned base)
{
	P4_Signed num;
	int digit, sign;

	if (s == NULL)
		return 0;

	sign = 1;
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
P4_WORD_DECL(extern, BRANCH);
P4_WORD_DECL(extern, BRANCHZ);

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

/***********************************************************************
 *** Input / Ouput
 ***********************************************************************/

P4_Signed
p4GetC(P4_Context *ctx)
{
	unsigned char ch;

	if (ctx->input.fd == -1)
		return ctx->input.offset < ctx->input.length ? ctx->input.buffer[ctx->input.offset++] : EOF;

	if (ctx->input.fp != NULL)
		return fgetc(ctx->input.fp);

	return read(ctx->input.fd, &ch, sizeof (ch)) == sizeof (ch) ? ch : EOF;
}

P4_Unsigned
p4GetLine(P4_Context *ctx, P4_Byte *line, P4_Size size)
{
	P4_Signed ch;
	P4_Unsigned i;

	if (line == NULL || size == 0)
		return 0;

	for (i = 0, --size; i < size; ) {
		if ((ch = p4GetC(ctx)) == EOF)
			break;

		line[i++] = (P4_Byte) ch;

		if (ch == '\n')
			break;
	}

	line[i] = '\0';

	return i;
}

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

/***********************************************************************
 *** Core Words
 ***********************************************************************/

P4_WORD_DECL(extern, THROW);

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

struct p4_xt p4_xt_NOOP = { p4_do_NOOP, NULL };
P4_Word p4_word_NOOP = { 0, { sizeof ("NOOP")-1, "NOOP" }, NULL, P4_WORD_XT(NOOP) };

/**
 * ... TRUE ...
 *
 * (S: -- flag)
 *
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * ... ALLOCATE ...
 *
 * (S: u -- a-addr ior )
 *
 * ior equals zero (0) on success.
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(ALLOCATE)
{
	if (P4_LENGTH(ctx->ds) < 1) {
		P4_PUSH_SAFE(ctx->ds).n = -4;
		P4_WORD_DO(THROW);
	} else {
		P4_TOP(ctx->ds).p = calloc(1, P4_TOP(ctx->ds).u * P4_CELL);
		P4_PUSH_SAFE(ctx->ds).n = (P4_TOP(ctx->ds).p == NULL);
	}
}

/**
 * ... seconds SLEEP ...
 *
 * (S: u -- u )
 *
 * @standard extension
 */
P4_WORD_DEFINE(SLEEP)
{
	P4_Unsigned seconds = P4_POP_SAFE(ctx->ds).u;
	P4_PUSH(ctx->ds).u = sleep(seconds);
}

/**
 * ... FREE ...
 *
 * (S: a-addr -- ior )
 *
 * ior equals zero (0) on success.
 *
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(DOT_DS)
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 *
 * @note
 *	: DUP >R R@ R> ;
 *	: DUP 0 PICK ;
 */
P4_WORD_DEFINE(DUP)
{
	P4_Cell top;

	if (P4_LENGTH(ctx->ds) < 1) {
		P4_PUSH_SAFE(ctx->ds).n = -4;
		P4_WORD_DO(THROW);
	} else {
		top = P4_TOP(ctx->ds);
		P4_PUSH_SAFE(ctx->ds) = top;
	}
}

/**
 * ... DROP ...
 *
 * (S: x -- )
 *
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(ZERO_EQ)
{
	P4_TOP(ctx->ds).u = (P4_TOP(ctx->ds).u == 0);
}

/**
 * ... 0< ...
 *
 * (S: n -- flag)
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(ZERO_LT)
{
	P4_TOP(ctx->ds).n = (P4_TOP(ctx->ds).n < 0);
}

/**
 * ... U> ...
 *
 * (S: u1 u2 -- flag)
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(U_GT)
{
	P4_Unsigned top = P4_POP_SAFE(ctx->ds).u;
	P4_TOP(ctx->ds).u = (P4_TOP(ctx->ds).u > top);
}

/**
 * ... U< ...
 *
 * (S: u1 u2 -- flag)
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(U_LT)
{
	P4_Unsigned top = P4_POP_SAFE(ctx->ds).u;
	P4_TOP(ctx->ds).u = (P4_TOP(ctx->ds).u < top);
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(SWAP)
{
	P4_Cell tmp;

	if (P4_LENGTH(ctx->ds) < 2) {
		P4_PUSH_SAFE(ctx->ds).n = -4;
		P4_WORD_DO(THROW);
	} else {
		/* (S: x y -- y x ) */
		tmp = P4_TOP(ctx->ds);
		P4_TOP(ctx->ds) = P4_PEEK(ctx->ds, -1);
		P4_POKE(ctx->ds, -1) = tmp;
	}
}

/**
 * ... DEPTH ...
 *
 * (S: -- u )
 *
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(RS_GET)
{
	P4_PUSH_SAFE(ctx->ds) = P4_POP_SAFE(ctx->rs);
}

/**
 * ... R@ ...
 *
 * (S:  -- x) (R: x -- x)
 *
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(TWO_RS_COPY)
{
	if (P4_LENGTH(ctx->rs) < 2) {
		P4_PUSH_SAFE(ctx->ds).n = -6;
		P4_WORD_DO(THROW);
	} else {
		p4GrowDS(ctx, 2);
		P4_PUSH(ctx->ds).u = P4_PEEK(ctx->rs, -1).u;
		P4_PUSH(ctx->ds).u = P4_TOP(ctx->rs).u;
	}
}

/**
 *  ... 2R> ...
 *
 * (S: -- x1 x2 ) ( R: x1 x2 -- )
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(TWO_RS_GET)
{
	if (P4_LENGTH(ctx->rs) < 2) {
		P4_PUSH_SAFE(ctx->ds).n = -6;
		P4_WORD_DO(THROW);
	} else {
		p4GrowDS(ctx, 2);
		P4_PUSH(ctx->ds).u = P4_POP(ctx->rs).u;
		P4_PUSH(ctx->ds).u = P4_POP(ctx->rs).u;
		P4_WORD_DO(SWAP);
	}
}

/**
 *  ... 2>R ...
 *
 * (S: x1 x2 -- ) ( R:  -- x1 x2 )
 *
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * ... FILL ...
 *
 * (S: c-addr u char -- )
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(FILL)
{
	P4_Byte ch = P4_POP_SAFE(ctx->ds).u;
	P4_Size length = P4_POP_SAFE(ctx->ds).u;
	P4_Byte *caddr = P4_POP_SAFE(ctx->ds).s;
	(void) memset(caddr, ch, length);
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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

	if (name.length == 0) {
		P4_PUSH(ctx->ds).n = -16;
		P4_WORD_DO(THROW);
	}

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
	ctx->ip = P4_POP_SAFE(ctx->rs).p;
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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

	/* p4Evaluate(ctx, ":NONAME , ' JUMP , CELL ,") */

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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(EXECUTE)
{
	P4_Exec_Token xts[2];

	xts[0] = P4_POP(ctx->ds).xt;
	xts[1] = NULL;

	for (ctx->ip = xts; *ctx->ip != NULL; ) {
		(*(*ctx->ip++)->code)(ctx);

		if (ctx->sig_int) {
			P4_PUSH(ctx->ds).n = ctx->sig_int;
			ctx->sig_int = 0;
			P4_WORD_DO(THROW);
		}
	}
}

/**
 * ... EMIT ...
 *
 * (S: x -- )
 *
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(KEY)
{
	P4_PUSH_SAFE(ctx->ds).n = (P4_Signed) fgetc(stdin);
}

/**
 *  ...  .#  ...
 *
 * (S: n -- )
 *
 * @standard extension
 */
P4_WORD_DEFINE(DOT_HASH)
{
	char buffer[sizeof (P4_Signed) * CHAR_BIT + 2];
	(void) p4SignedToString(P4_POP_SAFE(ctx->ds).n, ctx->obase, buffer, sizeof (buffer));
	fputs(buffer, stdout);
}

/**
 *  ...  U.#  ...
 *
 * (S: u -- )
 *
 * @standard extension
 */
P4_WORD_DEFINE(U_DOT_HASH)
{
	char buffer[sizeof (P4_Unsigned) * CHAR_BIT + 2];

	(void) p4UnsignedToString(P4_POP_SAFE(ctx->ds).u, ctx->obase, buffer, sizeof (buffer));
	fputs(buffer, stdout);
}

/**
 * ... >IN ...
 *
 * (S: -- a-addr)
 *
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * ... SOURCE ...
 *
 * (S: -- c-addr u)
 *
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(SOURCE_ID)
{
	P4_PUSH_SAFE(ctx->ds).p = &ctx->input.fd;
}

/**
 * ... IBASE ...
 *
 * (S: -- a-addr)
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(IBASE)
{
	P4_PUSH_SAFE(ctx->ds).p = &ctx->ibase;
}

/**
 * ... OBASE ...
 *
 * (S: -- a-addr)
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(OBASE)
{
	P4_PUSH_SAFE(ctx->ds).p = &ctx->obase;
}

/**
 * ... ACCEPT ...
 *
 * (S: c-addr +n -- n )
 *
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(REFILL)
{
	if (0 <= ctx->input.fd) {
		ctx->input.offset = 0;
		ctx->input.length = p4GetLine(ctx, ctx->input.buffer, P4_INPUT_SIZE);
		P4_PUSH_SAFE(ctx->ds).n = 0 < ctx->input.length && !feof(ctx->input.fp) && !ferror(ctx->input.fp);
	} else {
		P4_PUSH_SAFE(ctx->ds).n = 0;
	}
}

/**
 * ... WORDS ...
 *
 * (S: -- )
 *
 * @standard extension
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
			n = p4StringToSigned(start, &stop, ctx->ibase);

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
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(EVALUATE)
{
	P4_INPUT_PUSH(&ctx->input);

	ctx->input.blk = 0;
	ctx->input.fd = -1;
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
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(INCLUDED)
{
	P4_Byte *path;
	P4_Size length;

	if (P4_LENGTH(ctx->ds) < 2) {
		P4_PUSH(ctx->ds).n = -4;
		P4_WORD_DO(THROW);
	} else {
		length = P4_POP(ctx->ds).u;
		path = P4_POP(ctx->ds).s;
		path[length] = '\0';
		(void) p4EvalFile(ctx, path);
	}
}

/**
 * ... MAIN ...
 *
 * (S: -- ) (R: j*x -- )
 *
 * @standard extension
 */
static
P4_WORD_DEFINE(MAIN)
{
	ctx->ip = NULL;
	P4_WORD_DO(LSQUARE);
	ctx->input.fd = fileno(ctx->input.fp);

	for (;;) {
		P4_RESET(ctx->rs);

		if (ctx->input.fd == 0 && P4_IS_INTERPRETING(ctx))
			fputs("ok\n", stdout);

		P4_WORD_DO(REFILL);

		if (ctx->sig_int) {
			P4_PUSH(ctx->ds).n = ctx->sig_int;
			ctx->sig_int = 0;
			P4_WORD_DO(THROW);
		}

		if (!P4_POP_SAFE(ctx->ds).n)
			break;

		p4Interpret(ctx);
	}
}

/**
 * ... QUIT ...
 *
 * (S: -- ) (R: j*x -- )
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(QUIT)
{
	P4_Unsigned jmp_set = ctx->jmp_set;

	if (ctx->jmp_set & P4_JMP_QUIT)
		LONGJMP(ctx->on_quit, -56);
	else if (SETJMP(ctx->on_quit) == 0)
		ctx->jmp_set |= P4_JMP_QUIT;

	if (ctx->input.fp != NULL && ctx->input.fp != stdin)
		fclose(ctx->input.fp);
	ctx->input.fp = stdin;

	P4_WORD_DO(MAIN);

	ctx->jmp_set = jmp_set;
}

/**
 * ... ABORT ...
 *
 * (S: i*x -- ) (R: j*x -- )
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(ABORT)
{
	int rc;
	P4_Unsigned jmp_set = ctx->jmp_set;

	if (ctx->jmp_set & P4_JMP_ABORT)
		LONGJMP(ctx->on_abort, -1);
	else if ((rc = SETJMP(ctx->on_abort)) == 0)
		ctx->jmp_set |= P4_JMP_ABORT;

	/* Avoid double LONGJMP. */
	ctx->jmp_set &= ~P4_JMP_QUIT;

	if (rc != 1) {
		P4_RESET(ctx->ds);
		P4_WORD_DO(QUIT);
	}

	ctx->jmp_set = jmp_set;
}

/**
 * ... xt CATCH ...
 *
 * (S: i*x xt -- j*x 0 | i*x n )
 *
 * @standard ANS-Forth 1994
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

const char *p4_exceptions[] = {
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

/**
 * ... value THROW ...
 *
 * (S: k*x n -- k*x | i*x n )
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(THROW)
{
	P4_Signed rc = P4_POP_SAFE(ctx->ds).n;

	if (rc != 0) {
		if (ctx->jmp_set & P4_JMP_THROW)
			LONGJMP(ctx->on_throw, rc);

		if (rc != -1) {
			printf("%ld thrown: %s\n", rc, p4_exceptions[-rc]);
			if (ctx->state) {
				printf("compiling %s\n", (ctx->word == NULL) ? ":NONAME" : (char *)ctx->word->name.string);
			} else if (ctx->ip != NULL) {
				P4_Word *word = p4FindXt(ctx, ctx->ip[-1]);
				printf("executing %s\n", (word == NULL) ? ":NONAME" : (char *)word->name.string);
			}
		}

		if (ctx->jmp_set & P4_JMP_ABORT)
			LONGJMP(ctx->on_abort, rc);
	}
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
	LONGJMP(ctx->on_abort, 1);
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
 * @standard ANS-Forth 1994
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
P4_WORD_NAME(ACCEPT,		_exit,		0		);
P4_WORD_NAME(ALIGN,		ACCEPT,		0		);
P4_WORD_NAME(ALLOCATE,		ALIGN,		0		);
P4_WORD_NAME(AND,		ALLOCATE,	0 		);
P4_WORD_NAME(BLK,		AND,		0		);
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
P4_WORD_NAME(BYE,		EVALUATE,	0		);
P4_WORD_NAME(FILL,		BYE,		0 		);
P4_WORD_NAME(FREE,		FILL,		0		);
P4_WORD_NAME(HELP,		FREE,		0		);
P4_WORD_NAME(HERE,		HELP,		0		);
P4_WORD_NAME(IBASE,		HERE,		0		);
P4_WORD_NAME(IMMEDIATE,		IBASE,		P4_BIT_IMM 	);
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
P4_WORD_NAME(NEGATE,		MOVE,		0		);
P4_WORD_NAME(OBASE,		NEGATE,		0		);
P4_WORD_NAME(OR,		OBASE,		0 		);
P4_WORD_NAME(PARSE,		OR,		0		);
P4_WORD_NAME(PICK,		PARSE,		0		);
P4_WORD_NAME(QUIT,		PICK,		0		);
P4_WORD_NAME(REFILL,		QUIT,		0		);
P4_WORD_NAME(RESERVE,		REFILL,		0		);
P4_WORD_NAME(RESIZE,		RESERVE,	0		);
P4_WORD_NAME(ROLL,		RESIZE,		0		);
P4_WORD_NAME(RSHIFT,		ROLL,		0		);
P4_WORD_NAME(SLEEP,		RSHIFT,		0		);
P4_WORD_NAME(SOURCE,		SLEEP,		0		);
P4_WORD_NAME(STATE,		SOURCE,		0		);
P4_WORD_NAME(SWAP,		STATE,		0		);
P4_WORD_NAME(THROW,		SWAP,		0		);
P4_WORD_NAME(TYPE,		THROW,		0		);
P4_WORD_NAME(UNUSED,		TYPE,		0		);
P4_WORD_NAME(WORDS,		UNUSED,		0		);
P4_WORD_NAME(XOR,		WORDS,		0		);
P4_WORD_TEXT(ADD,		XOR,		0, 		"+");
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
P4_WORD_TEXT(DOT_HASH,		DOES_GT,	0, 		".#");
P4_WORD_TEXT(DOT_DS,		DOT_HASH,	P4_BIT_IMM,	".S");
P4_WORD_TEXT(DOT_RS,		DOT_DS,		P4_BIT_IMM,	".RS");
P4_WORD_TEXT(FETCH,		DOT_RS,		0, 		"@");
P4_WORD_TEXT(FIND_WORD,		FETCH,		0, 		"FIND-WORD");
P4_WORD_TEXT(GT_BODY,		FIND_WORD,	0,	 	">BODY");
P4_WORD_TEXT(GT_HERE,		GT_BODY,	0,		">HERE");
P4_WORD_TEXT(GT_IN,		GT_HERE,	0,	 	">IN");
P4_WORD_TEXT(LSQUARE,		GT_IN,		P4_BIT_IMM, 	"[");
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
P4_WORD_TEXT(TWO_DIV,		TICK_SEE,	0, 		"2/");
P4_WORD_TEXT(TWO_MUL,		TWO_DIV,	0, 		"2*");
P4_WORD_TEXT(TWO_RS_COPY,	TWO_MUL,	0,		"2R@");
P4_WORD_TEXT(TWO_RS_GET,	TWO_RS_COPY,	0,		"2R>");
P4_WORD_TEXT(TWO_RS_PUT,	TWO_RS_GET,	0,		"2>R");
P4_WORD_TEXT(U_DOT_HASH,	TWO_RS_PUT,	0, 		"U.#");
P4_WORD_TEXT(U_GT,		U_DOT_HASH,	0, 		"U>");
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
 * @standard ANS-Forth 1994
 */
": CONSTANT CREATE , DOES> @ ;\n"

/**
 * ... TRUE ...
 *
 * (S: -- 1 )
 *
 * @standard ANS-Forth 1994
 */
"1 CONSTANT TRUE\n"

/**
 * ... FALSE ...
 *
 * (S: -- 0 )
 *
 * @standard ANS-Forth 1994
 */
"0 CONSTANT FALSE\n"

/**
 * ... BL ...
 *
 * (S: -- ' ' )
 *
 * @standard ANS-Forth 1994
 */
"32 CONSTANT BL\n"

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

/**
 * VARIABLE name
 *
 * (C: "<spaces>name" -- ) // (S: -- a-addr )
 *
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 */
": ALIGNED 1 - /CELL 1 - OR 1 + ;\n"

/**
 *  ... CHAR+ ...
 *
 * (S: c-addr1 -- c-addr2 )
 *
 * @standard ANS-Forth 1994
 */
": CHAR+ 1 + ;\n"

/**
 *  ... CHARS ...
 *
 * (S: n1 -- n2 )
 *
 * @standard ANS-Forth 1994
 */
": CHARS ;\n"

/**
 *  ... CELL+ ...
 *
 * (S: a-addr1 -- a-addr2 )
 *
 * @standard ANS-Forth 1994
 */
": CELL+ /CELL + ;\n"

/**
 *  ... CELLS ...
 *
 * (S: n1 -- n2 )
 *
 * @standard ANS-Forth 1994
 */
": CELLS /CELL * ;\n"

/**
 *  ... OVER ...
 *
 * (S: x1 x2 -- x1 x2 x1 )
 *
 * @standard ANS-Forth 1994
 */
": OVER 1 PICK ;\n"

/**
 *  ... NIP ...
 *
 * (S: x1 x2 -- x2 )
 *
 * @standard ANS-Forth 1994
 */
": NIP SWAP DROP ;\n"

/**
 *  ... TUCK ...
 *
 * (S: x1 x2 -- x2 x1 x2 )
 *
 * @standard ANS-Forth 1994
 */
": TUCK SWAP OVER ;\n"

/**
 *  ... 2DUP ...
 *
 * (S: x1 x2 -- x1 x2 x1 x2 )
 *
 * @standard ANS-Forth 1994
 */
": 2DUP OVER OVER ;\n"

/**
 *  ... 2DROP ...
 *
 * (S: x1 x2 -- )
 *
 * @standard ANS-Forth 1994
 */
": 2DROP DROP DROP ;\n"

/**
 *  ... 2! ...
 *
 * (S: x y a-addr -- )
 *
 * @standard ANS-Forth 1994
 */
": 2! SWAP OVER ! CELL+ ! ;\n"

/**
 *  ... 2@ ...
 *
 * (S: a-addr -- x y )
 *
 * @standard ANS-Forth 1994
 */
": 2@ DUP CELL+ @ SWAP @ ;\n"

/**
 *  ... +!...
 *
 * (S: nu a-addr -- )
 *
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 */
": 1+ 1 + ;\n"

/**
 *  ... 1- ...
 *
 * (S: nu1 -- nu2 )
 *
 * @standard ANS-Forth 1994
 */
": 1- 1 - ;\n"

/**
 *  ... 0<> ...
 *
 * (S: n -- flag )
 *
 * @standard ANS-Forth 1994
 */
": 0<> 0= 0= ;\n"

/**
 *  ... 0> ...
 *
 * (S: n -- flag )
 *
 * @standard ANS-Forth 1994
 */
": 0> DUP 0= SWAP 0< OR 0= ;\n"

/**
 *  ... = ...
 *
 * (S: n1 n2 -- flag )
 *
 * @standard ANS-Forth 1994
 */
": = - 0= ;\n"

/**
 *  ... <> ...
 *
 * (S: n1 n2 -- flag )
 *
 * @standard ANS-Forth 1994
 */
": <> = 0= ;\n"

/**
 *  ... < ...
 *
 * (S: n1 n2 -- flag )
 *
 * @standard ANS-Forth 1994
 */
": < - 0< ;\n"

/**
 *  ... > ...
 *
 * (S: n1 n2 -- flag )
 *
 * @standard ANS-Forth 1994
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
": >= <= 0= ;\n"

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
 *  ... \ comment to end of line
 *
 * (S: "ccc<eol>" -- )
 *
 * @standard ANS-Forth 1994
 */
": \\ \\n PARSE 2DROP ; IMMEDIATE\n"

/**
 * ... CR ...
 *
 * (S: -- )
 *
 * @standard ANS-Forth 1994
 */
": CR \\r EMIT \\n EMIT ;\n"

/**
 * ... SPACE ...
 *
 * (S: -- )
 *
 * @standard ANS-Forth 1994
 */
": SPACE BL EMIT ;\n"

/**
 * ... . ...
 *
 * (S: n -- )
 *
 * @standard ANS-Forth 1994
 */
": . .# SPACE ;\n"

/**
 *  ...  U.  ...
 *
 * (S: u -- )
 *
 * @standard ANS-Forth 1994
 */
": U. U.# SPACE ;\n"

/**
 *  ...  POSTPONE  ...
 *
 * (C: "<spaces>name" -- )
 *
 * @standard ANS-Forth 1994
 */
": POSTPONE PARSE-WORD FIND-WORD DROP COMPILE, ; IMMEDIATE\n"

/**
 *  ...  CHAR  ...
 *
 * (S: "<spaces>name" -- char )
 *
 * @standard ANS-Forth 1994
 */
": CHAR PARSE-WORD DROP C@ ;\n"

/**
 *  ...  U.  ...
 *
 * (C: "<spaces>name" -- ) // (S: -- char )
 *
 * @standard ANS-Forth 1994
 */
": [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE\n"

/**
 * ... POSTPONE name ...
 *
 * (C: "<spaces>name" -- ) // (S: -- xt )
 *
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 */
": EXIT R> DROP R> CONTEXT ! ;\n"

/**
 *  ... ( comment) ...
 *
 * (S: "ccc<paren>" -- )
 *
 * @standard ANS-Forth 1994, file
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
 * @standard ANS-Forth 1994, extended
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 */
": UNLOOP R> 2R> 2DROP >R ;\n"

/**
 * ... limit first DO ... LEAVE ... LOOP ...
 *
 * (C: forw1 ... count dest -- forw1 ... forwN count' dest )
 * // (S: -- ) (R: loop-sys -- )
 *
 * @standard ANS-Forth 1994
 */
": LEAVE"				/* C: forw1 ... count dest R: --  */
	" >R >R"			/* C: forw1 ... R: dest count */
	" POSTPONE AHEAD"		/* C: forw1 ... forwN R: dest count */
	" R>"				/* C: forw1 ... forwN count R: dest */
	" 1+"				/* C: forw1 ... forwN count' R: dest */
	" R>"				/* C: forw1 ... forwN count' dest R: -- */
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
 * @standard ANS-Forth 1994
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
 * @standard ANS-Forth 1994
 */
": LOOP 1 POSTPONE LITERAL POSTPONE +LOOP ; IMMEDIATE\n"

/**
 * ... limit first DO ... LOOP ...
 *
 * (S: -- index ) (R: limit index ip -- limit index ip )
 *
 * @standard ANS-Forth 1994
 */
": I 1 RS-PICK R> ;\n"

/**
 * ... limit first DO ... LOOP ...
 *
 * (S: -- index1 ) (R: limit1 index1 limit2 index2 ip -- limit1 index1 limit2 index2 ip )
 *
 * @standard ANS-Forth 1994
 */
": J 3 RS-PICK R> ;\n"

/**
 * ... SLITERAL ...
 *
 * (C: c-addr u -- ) // (S: -- c-addr u )
 *
 * @standard ANS-Forth 1994
 */
": _slit R@ 3 CELLS + R@ @ R> CELL+ >R ;\n"	/* (S: -- c-addr u ) (R: ip -- ip+1 ) */

": SLITERAL" 					/* (C: c-addr u -- ) // (S: -- c-addr u ) */
	" ['] _slit COMPILE, DUP ,"		/* (C: c-addr u ) */
	" ['] BRANCH COMPILE,"
	" DUP ALIGNED DUP CELL+ ,"		/* (C: c-addr u u' ) */
	" RESERVE SWAP"				/* (C: c-addr a-addr u ) */
	" MOVE ; IMMEDIATE\n"			/* (C: -- ) */

/**
 * ... S" ccc" ...
 *
 * (C: "ccc<quote>" -- ) // (S: -- c-addr u )
 *
 * @standard ANS-Forth 1994, extended
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
 * @standard ANS-Forth 1994, extended
 */
": .\" POSTPONE S\" ['] TYPE COMPILE, ; IMMEDIATE\n"

/**
 * ... MAX ...
 *
 * (S: n1 n2 -- n3 )
 *
 * @standard ANS-Forth 1994
 */
": MAX 2DUP < IF SWAP THEN DROP ;\n"

/**
 * ... MIN ...
 *
 * (S: n1 n2 -- n3 )
 *
 * @standard ANS-Forth 1994
 */
": MIN 2DUP > IF SWAP THEN DROP ;\n"

/**
 * ... ABS ...
 *
 * (S: n -- u )
 *
 * @standard ANS-Forth 1994
 */
": ABS DUP 0< IF NEGATE THEN ;\n"

/**
 * ... ROT ...
 *
 * (S: a b c -- b c a )
 *
 * @standard ANS-Forth 1994
 */
": ROT 2 ROLL ;\n"

/**
 * ... ?DUP ...
 *
 * (S: x -- 0 | x x )
 *
 * @standard ANS-Forth 1994, extended
 */
": ?DUP DUP 0<> IF DUP THEN ;\n"

/**
 * ... SPACES ...
 *
 * (S: n -- )
 *
 * @standard ANS-Forth 1994, extended
 */
": SPACES 0 ?DO SPACE LOOP ;\n"

/**
 * ... ? ...
 *
 * (S: a-addr -- )
 *
 * @standard ANS-Forth 1994
 */
": ?"
	" 16 OBASE !"
	" DUP .\" 0x\" . SPACE SPACE @"
	" DUP .\" 0x\" ."
	" DUP 10 OBASE ! ."
	" 8 OBASE ! [CHAR] 0 EMIT ."
	" 10 OBASE ! CR ;\n"

/**
 * ... SEE ...
 *
 * (S: "<spaces>name" -- )
 *
 * @standard ANS-Forth 1994
 */
": SEE"
	" PARSE-WORD FIND-WORD"
	" IF 'SEE ELSE 2DROP THEN"
	" ; IMMEDIATE\n"

/**
 * ... BLANK ...
 *
 * (S: c-addr u -- )
 *
 * @standard ANS-Forth 1994
 */
": BLANK"
	" 1- SWAP BL OVER"			/* (S: u c-addr ' ' c-addr ) */
	" ! DUP 1+"				/* (S: u c-addr0 c-addr1 ) */
	" 2 ROLL"				/* (S: c-addr0 c-addr1 u ) */
	" CMOVE ;\n"				/* (S: -- ) */

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
}

static int
p4Evaluate(P4_Context *ctx)
{
	int rc = -1;

	P4_SETJMP_PUSH(ctx, &ctx->on_abort);
	if ((rc = SETJMP(ctx->on_abort)) == 0) {
		ctx->jmp_set |= P4_JMP_ABORT;

		P4_SETJMP_PUSH(ctx, &ctx->on_quit);
		if ((rc = SETJMP(ctx->on_quit)) == 0) {
			ctx->jmp_set |= P4_JMP_QUIT;

			ctx->input.offset = 0;
			P4_RESET(ctx->ds);
			P4_WORD_DO(MAIN);
		}

		P4_SETJMP_POP(ctx, &ctx->on_quit);
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
	P4_Byte buffer[P4_INPUT_SIZE];

	if (ctx == NULL || fd < 0) {
		errno = EFAULT;
		return -9;
	}

	P4_INPUT_PUSH(&ctx->input);
	ctx->input.buffer = buffer;

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
	P4_Byte buffer[P4_INPUT_SIZE];

	if (ctx == NULL || file == NULL) {
		errno = EFAULT;
		return -9;
	}

	P4_INPUT_PUSH(&ctx->input);
	ctx->input.buffer = buffer;

	if ((ctx->input.fp = fopen(file, "r")) == NULL) {
		rc = errno == ENOENT ? -38 : -37;
	} else {
		rc = p4Evaluate(ctx);
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

		P4_SETJMP_PUSH(ctx, &ctx->on_quit);
		if ((rc = SETJMP(ctx->on_quit)) == 0) {
			ctx->jmp_set |= P4_JMP_QUIT;

			P4_PUSH(ctx->ds).p = (P4_Pointer) string;
			P4_PUSH(ctx->ds).u = strlen(string);
			P4_WORD_DO(EVALUATE);
		}
		P4_SETJMP_POP(ctx, &ctx->on_quit);
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

#ifdef HAVE_TCGETATTR
	if (tcgetattr(fileno(stdin), &ctx->saved_tty))
		goto error1;
#endif
	ctx->ibase = 10;
	ctx->obase = 10;
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
		free(ctx);
	}
}

#ifdef TEST
/***********************************************************************
 *** Main
 ***********************************************************************/

#include <signal.h>

static char usage[] =
"usage: " P4_NAME " [-e expr][file ...] >output\n"
"\n"
"-e expr\t\tevaluate string\n"
"file ...\tevaluate input file\n"
"\n"
P4_NAME "/" P4_VERSION " " P4_COPYRIGHT "\n"
"Built " P4_BUILT "\n"
;

static P4_Context *ctx;

static void
sig_int(int signum)
{
	if (signum == SIGINT)
		ctx->sig_int = -28;
}

int
main(int argc, char **argv)
{
	char *arg;
	int argi, rc;

	p4Init();
	rc = EXIT_FAILURE;

	(void) signal(SIGINT, sig_int);

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
		}

		if (argv[argi] == NULL)
			fprintf(stderr, "missing option argument\n%s", usage);
		else
			fprintf(stderr, "invalid option -%c\n%s", argv[argi][1], usage);
		goto error1;
	}

	for ( ; argi < argc; argi++) {
		switch (p4EvalFile(ctx, argv[argi])) {
		case -1:
			fprintf(stderr, "compile error: %s\n", argv[argi]);
			goto error1;
		case 1:
			goto error2;
		}
	}

	P4_WORD_DO(ABORT);
error2:
	rc = P4_POP_SAFE(ctx->ds).n;
error1:
	p4Free(ctx);
error0:
	return rc;
}

#endif /* TEST */
