#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define here	m[0]
#define rp	m[1]
/*		m[2]		/* Always zero. Fake word for pushint. */
#define _x	m[3]
#define _y	m[4]
#define _z	m[5]

#define c 	m[here++] =

char tokens[5000] = ":\0immediate\0_read\0@\0!\0-\0*\0/\0<0\0exit\0echo\0key\0_pick";
int tokens_size = 64;

int m[20000] = {32};
int last_word = 1;
int ip;
int data_stack[500];
int *sp = data_stack;
int w;
int top_of_stack;

void
add_word(int x)
{
	/* Index of previous word definition in dictionary. */
	c last_word;

	/* Advance last_word to this word. */
	last_word = here-1;

	/* Save index of word's textual name. */
	c tokens_size;

	/* Code pointer of primative that implements the word. */
	c x;

	(void) scanf("%s", tokens + tokens_size);
	tokens_size += strlen(tokens + tokens_size) + 1;
}

void
r(int x)
{
	switch (m[x++]) {
	case 5:
		/* internal _read part 2 */
		for (w = scanf("%s", tokens) < 1 ? exit(0),0 : last_word; strcmp(tokens, &tokens[m[w+1]]) != 0; w = m[w])
			;
		w-1 ? r(w+2) : (c 2, c atoi(tokens));
		break;
	case 12:
		/* exit ( R: x -- ; ip = x ) return from call  */
		ip = m[rp--];
		break;
	case 15:
		/* _pick ( x -- y ) nth element from top of stack */
		top_of_stack = sp[-top_of_stack];
		break;
	case 1:
		/* _read; read a space-delimited word, find it in the
		 * dictionary, and compile a pointer to that word's
		 * code pointer onto the current end of the dictionary.
		 */
		c x;
		break;
	case 9:
		/* * ( n1 n2 -- n3 ) multiply */
		top_of_stack *= *sp--;
		break;
	case 7:
		/* ! ( x a-addr -- ) store */
		m[top_of_stack] = *sp--;
		top_of_stack = *sp--;
		break;
	case 0:
		/* pushint ( -- n1 ) push literal value */
		*++sp = top_of_stack;
		top_of_stack = m[ip++];
		break;
	case 8:
		/* - ( n1 n2 -- n3 ) subtract */
		top_of_stack = *sp-- - top_of_stack;
		break;
	case 2:
		/* run me ( R: -- ip ; ip = x ) call */
		m[++rp] = ip;
		ip = x;
		break;
	case 11:
		/* <0 ( n1 -- bool ) less than zero */
		top_of_stack = 0 > top_of_stack;
		break;
	case 4:
		/* immediate; when used immediately after a name following a ':',
    		 * makes the word being defined run whenever it is typed.
		 */
		here -= 2;
		c 2;
		break;
	case 6:
		/* @ ( a-addr -- x ) fetch */
		top_of_stack = m[top_of_stack];
		break;
	case 10:
		/* / ( n1 n2 -- n3 ) divide */
		top_of_stack = *sp-- / top_of_stack;
		break;
	case 3:
		/* : define word; read in the next space-delimited word,
		 * add it to the end of our string storage, and generate
		 * a header for the new word so that when it is typed it
		 * compiles a pointer to itself so that it can be executed.
		 */
		add_word(1);
		c 2;
		break;
	case 13:
		/* echo ( c1 -- ) */
		putchar(top_of_stack);
		top_of_stack = *sp--;
		break;
	case 14:
		/* key ( -- x ) */
		*++sp = top_of_stack;
		top_of_stack = getchar();
	}
}

int
main(int argc, char **argv)
{
	add_word(3);
	add_word(4);
	add_word(1);
	w = here;
	c 5;
	c 2;
	ip = here;
	c w;
	c ip-1;

	for (w = 6; w < 16; ) {
		add_word(1);
		c w++;
	}

	/* Start of return_stack. */
	rp = here;
	here += 512;

	for (;;)
		r(m[ip++]);

	return 0;
}
