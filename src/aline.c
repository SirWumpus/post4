/*
 * aline.c
 *
 * Copyright 2024, 2024 by Anthony Howe.  All rightes reserved.
 */

#include "aline.h"

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>

int is_tty;

static int tty_fd = -1;
static int tty_mode = 0;
static struct termios tty_modes[3];
static char lastline[256];
static const char *ps2;

#define tty_saved	tty_modes[ALINE_CANONICAL]
#define tty_raw		tty_modes[ALINE_RAW]
#define tty_raw_nb	tty_modes[ALINE_RAW_NB]

struct winsize window = {
	.ws_row = 24,
	.ws_col = 80,
};

void
alineSetMode(int mode)
{
	if (is_tty && tty_mode != mode) {
		(void) tcsetattr(tty_fd, TCSANOW, &tty_modes[mode]);
		tty_mode = mode;
	}
}

static void
sig_winch(int signum)
{
#if defined(HAVE_TCGETWINSIZE)
	if (is_tty) {
		(void) tcgetwinsize(tty_fd, &window);
	}
#elif defined(TIOCGWINSZ)
	if (is_tty) {
		(void) ioctl(tty_fd, TIOCGWINSZ, &window);
	}
#endif
}

void
alineFini(void)
{
	alineSetMode(ALINE_CANONICAL);
}

void
alineInit(void)
{
	if (tty_fd != -1  || !(is_tty = isatty(fileno(stdin)))) {
		return;
	}
	tty_fd = fileno(stdin);

	sig_winch(SIGWINCH);
	signal(SIGWINCH, sig_winch);

	setvbuf(stdin, NULL, _IONBF, 0);
	(void) tcgetattr(tty_fd, &tty_modes[ALINE_CANONICAL]);
	(void) atexit(alineFini);

	/* Non-canonical blocking input. */
	tty_modes[ALINE_RAW] = tty_modes[ALINE_CANONICAL];
	tty_modes[ALINE_RAW].c_cc[VMIN] = 1;
	tty_modes[ALINE_RAW].c_cc[VTIME] = 0;
	tty_modes[ALINE_RAW].c_lflag |= ISIG;
	tty_modes[ALINE_RAW].c_lflag &= ~(ICANON|ECHO|ECHONL|ECHOCTL);

	/* Non-canonical non-blocking input. */
	tty_modes[ALINE_RAW_NB] = tty_modes[ALINE_RAW];
	tty_modes[ALINE_RAW_NB].c_cc[VMIN] = 0;

	if ((ps2 = getenv("PS2")) == NULL || *ps2 == '\0') {
		ps2 = "* ";
	}
}

/* Simple tty line editor with last line history.
 *
 * \a		Get last input line.
 * ERASE \b \?	Erase character before the cursor.
 * WERASE	Erase previous whitespace delimited word.
 * KILL		Erase entire line.
 * \r \n	End input line.
 * EOF		End of file.
 *
 * @return
 *	Length of input read or EOF.
 */
int
alineInput(FILE *fp, const char *prompt, char *buf, size_t size)
{
	unsigned char ch;
	int i, is_esc = 0;

	if (buf == NULL || size < 1) {
		return EOF;
	}
	if (!isatty(fileno(fp))) {
		*buf = '\0';
		clearerr(fp); errno = 0;
		if (fgets(buf, size, fp) != NULL) {
			return strlen(buf);
		}
		return EOF;
	}
	if (prompt == NULL) {
		prompt = ps2;
	}
	if (sizeof (lastline) < size) {
		size = sizeof (lastline);
	}
	alineSetMode(ALINE_RAW);
	(void) printf(ANSI_SAVE_CURSOR);
	for (buf[i = 0] = '\0';	; ) {
		(void) printf(ANSI_RESTORE_CURSOR"%s%s%s", prompt, buf, ANSI_ERASE_TAIL);
		(void) fflush(stdout);
		clearerr(stdin);
		ch = fgetc(stdin);
		if (ch == EOF || ch == tty_saved.c_cc[VEOL] || ch == '\r' || ch == '\n') {
			(void) fputs("\r\n", stdout);
			break;
		}
#ifdef ANSI_FUNC_KEYS
		if (ch == '\a' || ch == '\033') {
			/* Restore intput to last input line. */
			(void) strncpy(buf, lastline, size-1);
			i = strlen(lastline);
			is_esc = ch * (ch == '\033');
		}
#else
		if (ch == '\e') {
			continue;
		}
		if (ch == '\a') {
			/* Restore intput to last input line. */
			(void) strncpy(buf, lastline, size-1);
			i = strlen(lastline);
		}
#endif
		else if (ch == tty_saved.c_cc[VERASE] || ch == '\b' || ch == 127) {
			if (0 < i) {
				buf[--i] = '\0';
			}
		} else if (ch == tty_saved.c_cc[VWERASE]) {
			while (0 < i) {
				if (isspace(buf[i-1])) {
					buf[--i] = '\0';
				} else {
					break;
				}
			}
			while (0 < i) {
				if (isspace(buf[i-1])) {
					break;
				}
				buf[--i] = '\0';
			}
		} else if (ch == tty_saved.c_cc[VKILL]) {
			buf[i = 0] = '\0';
		} else if (i == 0 && ch == tty_saved.c_cc[VEOF]) {
			return EOF;
		} else if (i < size) {
#ifdef ANSI_FUNC_KEYS
/* Not sure I want to support this just yet and there is better way
 * if I stop to think about it a little while, but for now I'll just
 * leave note-to-self.
 */
			/* Some ANSI function key? */
			if (is_esc) {
				/* https://en.wikipedia.org/wiki/ANSI_escape_code#Terminal_input_sequences */
				/* ESC ESC or ESC [ digits ; digits ~ */
				if (ch == '\033' || ch == '~') {
					is_esc = 0;
					continue;
				}
				/* ESC [ ABCD or ESC O PQRS  */
				if (ch == '[' || ch == 'O') {
					is_esc = ch;
					continue;
				}
				/* Arrow key ESC [ A .. ESC [ D */
				if ((is_esc == '[' && 'A' <= ch && ch <= 'D')
				/* Function F1..F4 ESC O P .. ESC O S */
				||  (is_esc == 'O' && 'P' <= ch && ch <= 'S')) {
					is_esc = 0;
					continue;
				}
				/* Function key ESC [ digits ; digits ~ */
				if (is_esc == '[' && (isdigit(ch) || ch == ';')) {
					continue;
				}
				/* ESC timeout */
			}
#endif
			/* Append ASCII character. */
			buf[i++] = ch;
			buf[i] = '\0';
			is_esc = 0;
		}
	}
	if (0 < i) {
		(void) strncpy(lastline, buf, size-1);
	}
	return (int) i;
}

#ifdef ALINE_TEST
int
main(int argc, char **argv)
{
	char buf[40];

	alineInit();
	while (alineInput(stdin, NULL, buf, sizeof (buf)) != EOF) {
		printf("Eh? %s\n", buf);
	}

	return 0;
}
#endif
