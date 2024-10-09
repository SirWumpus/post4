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

#include "config.h"

#include <stdio.h>
#include "ansiterm.h"

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

extern void alineInit(void);
extern void alineFini(void);
extern void alineSetMode(int mode);

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
