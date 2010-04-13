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

void
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
P4_Size
p4IntegerToString(P4_Signed number, P4_Unsigned base, P4_Byte *buffer, P4_Size size)
{
	int i, sign;
	P4_Unsigned value;

	if (size == 0)
		return 0;

	i = 0;
	size--;

	if (i < size) {
		sign = 1;
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

		buffer[i++] = base36_digits[value % base];
		value /= base;

		while (0 < value && i < size) {
			ldiv_t qr = ldiv(value, base);
			buffer[i++] = base36_digits[qr.rem];
			value = qr.quot;
		}

		if (sign == -1 && i < size)
			buffer[i++] = '-';
	}

	buffer[i] = '\0';
	p4StringReverse(buffer, i);

	return i;
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
p4StringToInteger(const P4_Byte *s, P4_Byte **stop, P4_Unsigned base)
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
 * @param length
 *	Number of table cells required.
 *
 * @param extra
 *	Number of addtional table cells to append to the table when
 *	there is insufficient space remaining to satisfy the length
 *	required.
 */
void
p4ArrayGrow(P4_Array *table, size_t length, size_t extra)
{
	P4_Cell *replace;
	P4_Unsigned depth;

	if (table->size < (depth = P4_LENGTH_(table)) + length) {
		table->size += length + extra;

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
	P4_PUSH_(table) = cell;
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

P4_Unsigned
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

	for (ip = (P4_Exec_Token *) xt->data->base; *ip != &p4_xt__exit; ip++) {
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
		} else if (*ip == &p4_xt_JUMP) {
			fprintf(fp, "%s 0x%.8lx ", found->name.string, (P4_Unsigned) *++ip);
			ip = (P4_Exec_Token *) *ip - 1;
		} else if (*ip == &p4_xt_BRANCH || *ip == &p4_xt_BRANCHZ || strcasecmp(found->name.string, "SLIT") == 0) {
			fprintf(fp, "%s %ld ", found->name.string, (P4_Signed) *++ip);
		} else if (*ip == &p4_xt__does) {
			fprintf(fp, "DOES> ");
			ip++;
		} else if (*ip == &p4_xt__lit) {
			if ((found = p4FindXt(ctx, *++ip)) == NULL)
				fprintf(fp, "lit 0x%lx ", (P4_Unsigned) *ip);
			else
				fprintf(fp, "[!] %s ", found->name.string);
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
	if (ctx->source_id == -1)
		return ctx->input.offset < ctx->input.length ? ctx->input.buffer[ctx->input.offset++] : EOF;

	return fgetc(ctx->input.fp);
}

P4_Signed
p4GetLine(P4_Context *ctx, P4_Byte *line, P4_Size size)
{
	P4_Signed ch, i;

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

P4_Signed
p4InputLine(FILE *fp, P4_Byte *line, P4_Size size)
{
	P4_Signed i;

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
P4_Word p4_word_NOOP = { 0, { sizeof ("NOOP")-1, "NOOP" }, NULL, &p4_xt_NOOP };

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
	P4_TOP(ctx->ds).p = malloc(P4_TOP(ctx->ds).u * P4_CELL);
	P4_PUSH_SAFE(ctx->ds).n = (P4_TOP(ctx->ds).p == NULL);
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
 * ... SEE ...
 *
 * (S: "<spaces>name" -- )
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(SEE)
{
	P4_WORD_DO(PARSE_WORD);
	P4_WORD_DO(FIND_WORD);

	if (P4_POP(ctx->ds).n == 0) {
		P4_POP(ctx->ds);
		P4_POP(ctx->ds);
	} else {
		p4See(stdout, ctx, P4_POP(ctx->ds).xt);
	}
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
 * (CS: xu ... x1 x0 -- xu ... x1 x0 xu ) (S: u -- )
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(CS_PICK)
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
 * (CS: xu xu-1 ... x0 u -- xu-1 ... x0 xu ) (S: u -- )
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(CS_ROLL)
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
	P4_Cell top = P4_TOP(ctx->ds);
	P4_PUSH_SAFE(ctx->ds) = top;
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
	P4_TOP(ctx->ds).n = (P4_TOP(ctx->ds).n == 0);
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
 * ... CELL ...
 *
 * (S:  -- u)
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(CELL)
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

	if (!P4_IS_EMPTY(ctx->ds)) {
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
 * (S: -- u )
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
	P4_Pointer dst = P4_POP_SAFE(ctx->ds).p;
	P4_Pointer src = P4_POP_SAFE(ctx->ds).p;
	memcpy(dst, src, size);
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
 * ... IP>R ...
 *
 * (S: -- ) (R: -- a-addr )
 *
 * @standard extension
 */
P4_WORD_DEFINE(IP_TO_R)
{
	P4_PUSH_SAFE(ctx->rs).p = ctx->ip;
}

/**
 * ... R>IP ...
 *
 * (S: -- ) (R: a-addr -- )
 *
 * @standard extension
 */
P4_WORD_DEFINE(R_TO_IP)
{
	ctx->ip = P4_POP_SAFE(ctx->rs).p;
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
		P4_TOP(ctx->ds).f = &p4_do_NOOP; /* ( c-addr -- xt ) */
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

	for (ctx->ip = xts; *ctx->ip != NULL; )
		(*(*ctx->ip++)->code)(ctx);
}

/**
 * ... POSTPONE word ...
 *
 * (C: "<spaces>name" -- )
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(POSTPONE)
{
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
 *  ...  .  ...
 *
 * (S: x -- )
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(DOT)
{
	char buffer[sizeof (P4_Signed) * 8 + 1];

	(void) p4IntegerToString(P4_POP_SAFE(ctx->ds).n, ctx->obase, buffer, sizeof (buffer));
	fputs(buffer, stdout);
	fputc(' ', stdout);
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
	P4_PUSH(ctx->ds).p = &ctx->input.buffer[ctx->input.offset];
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
	P4_PUSH_SAFE(ctx->ds).p = &ctx->source_id;
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
	if (0 <= ctx->source_id) {
		ctx->input.offset = 0;
		ctx->input.length = p4InputLine(ctx->input.fp, ctx->input.buffer, P4_INPUT_SIZE);
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

/**
 * ... EVALUATE ...
 *
 * (S: i*x c-addr u -- j*x )
 *
 * @standard ANS-Forth 1994
 */
P4_WORD_DEFINE(EVALUATE)
{
	P4_Signed n;
	P4_Size length;
	P4_Unsigned in_off;
	P4_Byte *stop, *start;
	P4_Input previous_source;

	previous_source = ctx->input;
	ctx->input.length = P4_POP_SAFE(ctx->ds).u;
	ctx->input.buffer = P4_POP_SAFE(ctx->ds).s;
	ctx->input.offset = 0;
	ctx->source_id = -1;

	while (ctx->input.offset < ctx->input.length) {
		in_off = ctx->input.offset;
		P4_WORD_DO(PARSE_WORD);
		P4_WORD_DO(FIND_WORD);

		if ((n = P4_POP(ctx->ds).n) == 0) {
			/* Word not found; try converting to a number. */
			length = P4_POP(ctx->ds).u;
			start = P4_POP(ctx->ds).s;
			if (length == 0)
				continue;
			n = p4StringToInteger(start, &stop, ctx->ibase);

			/* Was it a numeric string in the input base? */
			if (stop - start == length) {
				P4_PUSH(ctx->ds).n = n;
				P4_WORD_DO(LITERAL);
			} else {
				for (n = 0; n < length; n++)
					fputc(*start++, stdout);
				fprintf(stdout, " ? (>IN=" P4_UNSIGNED_FMT ")\n", in_off);
				break;
			}
		} else if (P4_IS_COMPILING(ctx) && n == -1) {
			P4_WORD_DO(COMPILE_COMMA);
		} else {
			P4_WORD_DO(EXECUTE);
		}
	}

	ctx->input = previous_source;
	ctx->source_id = fileno(ctx->input.fp);
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
	P4_WORD_DO(LSQUARE);
	ctx->source_id = fileno(ctx->input.fp);

	for (;;) {
		P4_RESET(ctx->rs);

		if (ctx->source_id == 0 && P4_IS_INTERPRETING(ctx))
			fputs("ok\n", stdout);

		P4_WORD_DO(REFILL);
		if (!P4_POP_SAFE(ctx->ds).n)
			break;

		P4_WORD_DO(SOURCE);
		P4_WORD_DO(EVALUATE);
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
	if (ctx->jmp_set & P4_JMP_QUIT)
		longjmp(ctx->on_quit, -56);
	else if (setjmp(ctx->on_quit) == 0)
		ctx->jmp_set |= P4_JMP_QUIT;

	if (ctx->input.fp != NULL && ctx->input.fp != stdin)
		fclose(ctx->input.fp);
	ctx->input.fp = stdin;

	P4_WORD_DO(MAIN);

	ctx->jmp_set &= ~P4_JMP_QUIT;
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

	if (ctx->jmp_set & P4_JMP_ABORT)
		longjmp(ctx->on_abort, -1);
	else if ((rc = setjmp(ctx->on_abort)) == 0)
		ctx->jmp_set |= P4_JMP_ABORT;

	/* Avoid double longjmp. */
	ctx->jmp_set &= ~P4_JMP_QUIT;

	if (rc != 1) {
		P4_RESET(ctx->ds);
		P4_WORD_DO(QUIT);
	}

	ctx->jmp_set &= ~P4_JMP_ABORT;
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
	jmp_buf prev_frame;
	P4_Unsigned jmp_set;
	P4_Input previous_source;
	P4_Unsigned ds_depth, rs_depth;

	jmp_set = ctx->jmp_set;
	memcpy(&prev_frame, &ctx->on_throw, sizeof (prev_frame));
	previous_source = ctx->input;
	ds_depth = P4_LENGTH(ctx->ds);
	rs_depth = P4_LENGTH(ctx->rs);

	if ((rc = setjmp(ctx->on_throw)) == 0) {
		ctx->jmp_set |= P4_JMP_THROW;
		P4_WORD_DO(EXECUTE);
	} else {
		P4_SET_DEPTH(ctx->ds, ds_depth);
		P4_SET_DEPTH(ctx->rs, rs_depth);
		ctx->source_id = fileno(previous_source.fp);
		ctx->input = previous_source;
	}

	P4_PUSH(ctx->ds).n = rc;
	memcpy(&ctx->on_throw, &prev_frame, sizeof (prev_frame));
	ctx->jmp_set = jmp_set;
}

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
			longjmp(ctx->on_throw, rc);

		if (rc != -1)
			fprintf(stdout, "%ld thrown\n", rc);
		longjmp(ctx->on_abort, rc);
	}
}

static const char help_summary[] =
"To quit send EOF, interrupt signal, or BYE.\n"
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
	longjmp(ctx->on_abort, 1);
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
P4_WORD_NAME(CATCH,		AND,		0		);
P4_WORD_NAME(CELL,		CATCH,		0		);
P4_WORD_NAME(CMOVE,		CELL,		0		);
P4_WORD_NAME(CONSOLE,		CMOVE,		0		);
P4_WORD_NAME(CREATE,		CONSOLE,	0		);
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
P4_WORD_NAME(BRANCH,		INVERT,		0 		);
P4_WORD_NAME(BRANCHZ,		BRANCH,		0 		);
P4_WORD_NAME(JUMP,		BRANCHZ,	0 		);
P4_WORD_NAME(JUMPZ,		JUMP,		0 		);
P4_WORD_NAME(KEY,		JUMPZ,		0 		);
P4_WORD_NAME(LITERAL,		KEY,		P4_BIT_IMM	);
P4_WORD_NAME(LSHIFT,		LITERAL,	0		);
P4_WORD_NAME(MOD,		LSHIFT,		0		);
P4_WORD_NAME(MOVE,		MOD,		0		);
P4_WORD_NAME(NEGATE,		MOVE,		0		);
P4_WORD_NAME(OBASE,		NEGATE,		0		);
P4_WORD_NAME(OR,		OBASE,		0 		);
P4_WORD_NAME(PARSE,		OR,		0		);
P4_WORD_NAME(PICK,		PARSE,		0		);
P4_WORD_NAME(POSTPONE,		PICK,		P4_BIT_IMM	);
P4_WORD_NAME(QUIT,		POSTPONE,	0		);
P4_WORD_NAME(RESERVE,		QUIT,		0		);
P4_WORD_NAME(RESIZE,		RESERVE,	0		);
P4_WORD_NAME(ROLL,		RESIZE,		0		);
P4_WORD_NAME(RSHIFT,		ROLL,		0		);
P4_WORD_NAME(SEE,		RSHIFT,		0		);
P4_WORD_NAME(SOURCE,		SEE,		0		);
P4_WORD_NAME(STATE,		SOURCE,		0		);
P4_WORD_NAME(SWAP,		STATE,		0		);
P4_WORD_NAME(THROW,		SWAP,		0		);
P4_WORD_NAME(TYPE,		THROW,		0		);
P4_WORD_NAME(WORDS,		TYPE,		0		);
P4_WORD_NAME(XOR,		WORDS,		0		);
P4_WORD_TEXT(IP_TO_R,		XOR,		P4_BIT_IMM,	"IP>R");
P4_WORD_TEXT(R_TO_IP,		IP_TO_R,	P4_BIT_IMM,	"R>IP");
P4_WORD_TEXT(ADD,		R_TO_IP,	0, 		"+");
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
P4_WORD_TEXT(DOT,		DOES_GT,	0, 		".");
P4_WORD_TEXT(DOT_DS,		DOT,		P4_BIT_IMM,	".S");
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
P4_WORD_TEXT(SEMICOLON,		RS_PUT,		P4_BIT_IMM,	";");
P4_WORD_TEXT(SOURCE_ID,		SEMICOLON,	0,		"SOURCE-ID");
P4_WORD_TEXT(STORE,		SOURCE_ID,	0, 		"!");
P4_WORD_TEXT(SUB,		STORE,		0, 		"-");
P4_WORD_TEXT(TICK,		SUB,		0, 		"'");
P4_WORD_TEXT(TWO_DIV,		TICK,		0, 		"2/");
P4_WORD_TEXT(TWO_MUL,		TWO_DIV,	0, 		"2*");
P4_WORD_TEXT(ZERO_EQ,		TWO_MUL,	0, 		"0=");
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
"20 CONSTANT BL\n"

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
"20 CONSTANT \\s\n"				/* space */
"27 CONSTANT \\e\n"				/* escape */
"127 CONSTANT \\?\n"				/* delete */

/**
 * VARIABLE name
 *
 * (C: "<spaces>name" -- ) // (S: -- a-addr )
 *
 * @standard ANS-Forth 1994
 */
": VARIABLE CREATE 0 , ;\n"

": ALIGNED 1 - CELL 1 - OR 1 + ;\n"		/* (S: addr -- a-addr ) */

": CHAR+ 1 + ;\n"				/* (S: c-addr1 -- c-addr2 ) */

": CELL+ CELL + ;\n"				/* (S: a-addr1 -- a-addr2 ) */

": CELLS CELL * ;\n"				/* (S: n1 -- n2 ) */

": OVER 1 PICK ;\n"				/* (S: x y -- x y x ) */

": 2DUP OVER OVER ;\n"				/* (S: x1 x2 -- x1 x2 x1 x2 ) */

": 2DROP DROP DROP ;\n"				/* (S: x y -- ) */

": 2! SWAP OVER ! CELL+ ! ;\n"			/* (S: x y a-addr -- ) */

": 2@ DUP CELL+ @ SWAP @ ;\n"			/* (S: a-addr -- x y ) */

": 2>R SWAP >R >R ;\n"				/* (S: x1 x2 -- ) ( R:  -- x1 x2 ) */

": 2R> R> R> SWAP ;\n"				/* (S: -- x1 x2 ) ( R: x1 x2 -- ) */

": 2R@ R> R> 2DUP >R >R SWAP ;\n"		/* (S: -- x1 x2 ) ( R: x1 x2 -- x1 x2 ) */

/**
 *  ... 1+ ...
 *
 * (S: n1 | u1 -- n2 | u2 )
 *
 * @standard ANS-Forth 1994
 */
": 1+ 1 + ;\n"

/**
 *  ... 1- ...
 *
 * (S: n1 | u1 -- n2 | u2 )
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
": > < 0= ;\n"

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

": \\ \\n PARSE 2DROP ; IMMEDIATE\n"		/* (S: -- ) */

": CR \\r EMIT \\n EMIT ;\n"			/* (S: -- ) */

": SPACE \\s EMIT ;\n"				/* (S: -- ) */

": POSTPONE PARSE-WORD FIND-WORD DROP COMPILE, ; IMMEDIATE\n"	/* (C: "<spaces>name" -- ) */

": CHAR PARSE-WORD DROP C@ ;\n"			/* (S: "<spaces>name" -- char ) */

": [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE\n"	/* (C: "<spaces>name" -- ) // (S: -- char ) */

/**
 *  ... ( comment) ...
 *
 * (S: "ccc<paren>" -- )
 *
 * @standard ANS-Forth 1994
 */
": ( [CHAR] ) PARSE 2DROP ; IMMEDIATE\n"

/**
 * ... .( ccc) ...
 *
 * (S: "ccc<paren>" -- )
 *
 * @standard ANS-Forth 1994, extended
 */
": .( [CHAR] ) PARSE-ESCAPE TYPE ; IMMEDIATE\n"

": ['] ' POSTPONE LITERAL ; IMMEDIATE\n" 	/* (C: "<spaces>name" -- ) // (S: -- xt ) */

": BEGIN R> >HERE >R >R ; IMMEDIATE\n"				/* (R: ip -- dest ip ) */

": AGAIN ['] BRANCH COMPILE, R> R> >HERE - , >R ; IMMEDIATE\n"	/* (R: dest ip -- ip ) */

": UNTIL ['] BRANCHZ COMPILE, R> R> >HERE - , >R ; IMMEDIATE\n"	/* (R: dest ip -- ip ) */

": AHEAD ['] BRANCH COMPILE, R> >HERE 0 , >R >R ; IMMEDIATE\n"	/* (R: ip -- forw ip ) */

": IF ['] BRANCHZ COMPILE, R> >HERE 0 , >R >R ; IMMEDIATE\n"	/* (R: ip -- forw ip ) */

": THEN R> >HERE R> - HERE OVER - ! >R ; IMMEDIATE\n"		/* (R: forw ip -- ip ) */

": ELSE POSTPONE AHEAD 1 CS-ROLL POSTPONE THEN ; IMMEDIATE\n"	/* (R: forw1 ip -- forw2 ip ) */

": _slit R@ 3 CELLS + R@ @ R> CELL+ >R ;\n"	/* (S: -- c-addr u ) (R: ip -- ip++ ) */

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
": S\" [CHAR] \" PARSE-ESCAPE POSTPONE SLITERAL ; IMMEDIATE\n"

/**
 * ... ." ccc" ...
 *
 * (S: "ccc<quote>" -- )
 *
 * @standard ANS-Forth 1994, extended
 */
": .\" POSTPONE S\" ['] TYPE COMPILE, ; IMMEDIATE\n"

": MAX 2DUP < IF SWAP THEN DROP ;\n"		/* (S: n1 n2 -- n3 ) */

": MIN 2DUP > IF SWAP THEN DROP ;\n"		/* (S: n1 n2 -- n3 ) */

": ABS DUP 0< IF NEGATE THEN ;\n"		/* (S: n -- u ) */

": ROT 2 ROLL ;\n"				/* (S: a b c -- b c a ) */

": ?"						/* (S: a-addr -- ) */
	" 16 OBASE !"
	" DUP .\" 0x\" . SPACE SPACE @"
	" DUP .\" 0x\" ."
	" DUP 10 OBASE ! ."
	" 8 OBASE ! [CHAR] 0 EMIT ."
	" 10 OBASE ! CR ;\n"

/**
 * ... BLANK ...
 *
 * (S: c-addr u -- )
 *
 * @standard ANS-Forth 1994, extended
 */
": BLANK"
	" SWAP BL OVER"				/* (S: u c-addr ' ' c-addr ) */
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

	if ((ctx = malloc(sizeof (*ctx))) == NULL)
		goto error0;

	if (p4ArrayAssign(&ctx->rs, P4_STACK_SIZE))
		goto error1;

	if (p4ArrayAssign(&ctx->ds, P4_STACK_SIZE))
		goto error1;

	if (p4ArrayAssign(&ctx->noname, P4_STACK_SIZE))
		goto error1;

	ctx->ip = NULL;
	ctx->ibase = 10;
	ctx->obase = 10;
	ctx->xt = NULL;
	ctx->word = NULL;
	ctx->jmp_set = 0;
	ctx->input.fp = stdin;
	ctx->input.offset = 0;
	ctx->input.length = 0;
	ctx->input.buffer = ctx->console;

	if (p4_core_words == NULL) {
		/* Use first context to compile the globally defined core words.  */
		ctx->words = &p4_word_ABORT;
		P4_PUSH(ctx->ds).s = (P4_Byte *) p4_defined_words;
		P4_PUSH(ctx->ds).u = sizeof (p4_defined_words)-1;
		P4_WORD_DO(EVALUATE);
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
		return -1;
	}

	if ((ctx->input.fp = fopen(file, "r")) == NULL)
		return -1;

	if ((rc = setjmp(ctx->on_abort)) == 0) {
		ctx->jmp_set |= P4_JMP_ABORT;
		if ((rc = setjmp(ctx->on_quit)) == 0) {
			ctx->jmp_set |= P4_JMP_QUIT;

			P4_RESET(ctx->ds);
			P4_WORD_DO(MAIN);
		}
	}
	ctx->jmp_set &= ~(P4_JMP_ABORT|P4_JMP_QUIT);

	fclose(ctx->input.fp);
	ctx->input.fp = stdin;

	return rc;
}

int
p4EvalString(P4_Context *ctx, const char *string)
{
	int rc;

	if ((rc = setjmp(ctx->on_abort)) == 0) {
		ctx->jmp_set |= P4_JMP_ABORT;
		if ((rc = setjmp(ctx->on_quit)) == 0) {
			ctx->jmp_set |= P4_JMP_QUIT;

			P4_RESET(ctx->ds);
			P4_PUSH(ctx->ds).p = (P4_Pointer) string;
			P4_PUSH(ctx->ds).u = strlen(string);
			P4_WORD_DO(EVALUATE);
		}
	}

	ctx->jmp_set &= ~(P4_JMP_ABORT|P4_JMP_QUIT);

	return rc;
}

#ifdef TEST
/***********************************************************************
 *** Main
 ***********************************************************************/

static char usage[] =
"usage: " P4_NAME " [-e expr][file ...] >output\n"
"\n"
"-e expr\t\tevaluate string\n"
"file ...\tevaluate input file\n"
"\n"
P4_NAME "/" P4_VERSION " " P4_COPYRIGHT "\n"
"Built " P4_BUILT "\n"
;

int
main(int argc, char **argv)
{
	char *arg;
	int argi, rc;
	P4_Context *ctx;

	p4Init();
	rc = EXIT_FAILURE;

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
