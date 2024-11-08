/*
 * aline.h
 *
 * Simple tty style line editing with last line history.
 *
 * Copyright 2024, 2024 by Anthony Howe.  All rightes reserved.
 */

#ifndef __aline_h__
#define __aline_h__	1

#ifdef __cplusplus
extern "C" {
#endif

#define _DEFAULT_SOURCE			1
#define _XOPEN_SOURCE			700

#ifdef __APPLE__
# define _DARWIN_C_SOURCE 		1
#endif

#ifdef __NetBSD__
# define _NETBSD_SOURCE			1
#endif
#ifndef ECHOCTL
# define ECHOCTL			0
#endif

#include "config.h"
#include "ansiterm.h"

#include <stdio.h>
#include <limits.h>

#ifdef HAVE_TERMIOS_H
# include <termios.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif

/***********************************************************************
 *** A Line
 ***********************************************************************/

extern int is_tty;
extern struct winsize window;

#define ALINE_CANONICAL		0
#define ALINE_RAW		1
#define ALINE_RAW_NB		2

#define ALINE_HISTORY		16

extern void alineFini(void);
extern int alineInit(int hist_size);
extern int alineReadByte(void);
extern int alineSetMode(int mode);

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
 *	Zero (0) on success, EOF, or some errno value.
 */
extern int alineInput(FILE *in, const char *prompt, char *buf, size_t size);

/***********************************************************************
 *** END
 ***********************************************************************/

#ifdef  __cplusplus
}
#endif

#endif /* __aline_h__ */
