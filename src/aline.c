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
#include <fcntl.h>
#include <unistd.h>

#undef HAVE_CTERMID

int is_tty;

static int tty_fd = -1;
static int tty_mode = 0;
static struct termios tty_modes[3];
static const char *ps2;

#ifndef ALINE_BASIC_INPUT
static int lastline;
static unsigned short histsize;
static char (*history)[MAX_INPUT];
#endif

#define tty_saved	tty_modes[ALINE_CANONICAL]
#define tty_raw		tty_modes[ALINE_RAW]
#define tty_raw_nb	tty_modes[ALINE_RAW_NB]

struct winsize window = {
	.ws_row = 24,
	.ws_col = 80,
};

int
alineSetMode(int mode)
{
	int prev = tty_mode;
	if (is_tty && tty_mode != mode) {
		errno = 0;
		if (tcsetattr(tty_fd, TCSANOW, &tty_modes[mode])) {
			(void) fprintf(stderr, "%s mode=%d errno=%d\n", __func__, mode, errno);
		}
		tty_mode = mode;
	}
	return prev;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
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
#pragma GCC diagnostic pop

static void
alineFini(void)
{
	alineSetMode(ALINE_CANONICAL);
#ifdef HAVE_CTERMID
	(void) close(tty_fd);
#endif
	free(history);
}

int
alineInit(int hist_size)
{
	if (tty_fd != -1  || !(is_tty = isatty(fileno(stdin)))) {
		return -1;
	}
#ifdef HAVE_CTERMID
	tty_fd = open(ctermid(NULL), O_RDWR, S_IRWXU|S_IRWXG|S_IRWXO);
#else
	tty_fd = fileno(stdin);
#endif
	sig_winch(SIGWINCH);
	signal(SIGWINCH, sig_winch);

	(void) setvbuf(stdin, NULL, _IOLBF, 0);
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

	/* Size as next highest power of 2. */
	for (histsize = 2, hist_size--; hist_size >>= 1; ) {
		histsize = histsize << 1;
	}

	history = (char (*)[]) calloc(histsize, sizeof (*history));
	return -(history == NULL);
}

#ifndef ALINE_BASIC_INPUT
static void
alineGetRowCol(int pos[2])
{
	char *stop, report[12];
	int n, fd = fileno(stdout);
	(void) tcsetattr(fd, TCSANOW, &tty_modes[ALINE_RAW]);
	(void) write(fd, ANSI_REPORT, sizeof (ANSI_REPORT)-1);
	n = read(fd, report, sizeof (report));
	report[n] = '\0';
	pos[0] = (unsigned) strtoul(report+2, &stop, 10);
	pos[1] = (unsigned) strtoul(stop+1, NULL, 10);
}
#endif

int
alineReadByte(void)
{
	unsigned char ch;
	if (read(tty_fd, &ch, sizeof (ch)) != sizeof (ch)) {
		return EOF;
	}
	return ch;
}

/* Simple tty line editor with last line history.
 *
 *      up      ^K      Cycle to the previous input line to edit.
 *      down    ^L      Cycle to the next input line to edit.
 *      left    right   Cursor left or right within line.
 *      ERASE   ^H  ^?  Erase character before the cursor.
 *      WERASE          Erase the previous white space delimited word.
 *      KILL            Erase current line input.
 *      EOL     ^M  ^J  Newline submits input line.
 *      EOF             End of file.
 *
 * @return
 *	Length of input read or EOF.
 */
int
alineInput(FILE *fp, const char *prompt, char *buf, size_t size)
{
	if (buf == NULL || size < 1) {
		return EOF;
	}
#ifdef ALINE_BASIC_INPUT
	clearerr(fp); errno = 0;
	(void) alineSetMode(ALINE_CANONICAL);
	if (fgets(buf, size, fp) != NULL) {
		return strlen(buf);
	}
	return EOF;
#else
	unsigned i;
	int ch, pcol, pos[2], prevline = lastline;

	(void) fflush(stdout);
	if (!isatty(fileno(fp))) {
		*buf = '\0';
		clearerr(fp); errno = 0;
		(void) alineSetMode(ALINE_CANONICAL);
		if (fgets(buf, size, fp) != NULL) {
			return strlen(buf);
		}
		return EOF;
	}
	if (prompt == NULL) {
		prompt = ps2;
	}
	pcol = strlen(prompt);
	if (sizeof (*history) < size) {
		size = sizeof (*history);
	}
	alineGetRowCol(pos);
	(void) printf(ANSI_SAVE_CURSOR);
	(void) alineSetMode(ALINE_RAW);
	for (size--, buf[i = 0] = '\0';	; ) {
		(void) printf(ANSI_RESTORE_CURSOR"%s%s"ANSI_ERASE_TAIL""ANSI_GOTO, prompt, buf, pos[0], pos[1]+pcol+i);
		(void) fflush(stdout);
#ifdef HAVE_CTERMID
		ch = alineReadByte();
#else
		clearerr(fp);
		ch = fgetc(fp);
#endif
		if (ch == EOF || ch == tty_saved.c_cc[VEOL] || ch == '\r' || ch == '\n') {
			(void) fputs("\r\n", stdout);
			break;
		}
		if (ch == '\e') {
#ifdef HAVE_CTERMID
			if ((ch = alineReadByte()) == '[') {
				ch = alineReadByte();
#else
			if ((ch = fgetc(fp)) == '[') {
				ch = fgetc(fp);
#endif
				if (ch == 'A') {
					ch = '\v';
				} else if (ch == 'B') {
					ch = '\f';
				} else if (ch == 'C') {
					i += i < size && buf[i] != '\0';
					continue;
				} else if (ch == 'D') {
					i -= 0 < i;
					continue;
				}
			}
		}
		if (ch == '\v' || ch == '\f') {
			/* Restore intput to last input line. */
			prevline = (prevline+(ch == '\v' ? -1 : 1)) & (histsize-1);
			(void) strncpy(buf, history[prevline], size-1);
			i = strlen(history[prevline]);
		} else if (ch == tty_saved.c_cc[VERASE] || ch == '\b' || ch == 127) {
			if (0 < i) {
				i--;
				(void) memmove(buf+i, buf+i+1, strlen(buf+i)+1);
			}
		} else if (ch == tty_saved.c_cc[VWERASE]) {
			int j = i;
			while (0 < i && isspace(buf[i-1])) {
				i--;
			}
			while (0 < i && !isspace(buf[i-1])) {
				i--;
			}
			(void) memmove(buf+i, buf+j, strlen(buf+j)+1);
		} else if (ch == tty_saved.c_cc[VKILL]) {
			buf[i = 0] = '\0';
		} else if (i == 0 && ch == tty_saved.c_cc[VEOF]) {
			(void) fputs("\r\n", stdout);
			return EOF;
		} else if (i < size) {
			(void) memmove(buf+i+1, buf+i, strlen(buf+i)+1);
			buf[i++] = (unsigned char) ch;
		}
	}
	if (0 < i) {
		(void) strncpy(history[lastline], buf, size-1);
		lastline = (lastline+1) & (histsize-1);
	}
	return (int) strlen(buf);
#endif
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
