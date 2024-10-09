/*
 * ansiterm.h
 *
 * Copyright 2024, 2024 by Anthony Howe.  All rightes reserved.
 */

#ifndef __ansiterm_h__
#define __ansiterm_h__	1

#ifdef __cplusplus
extern "C" {
#endif

/***********************************************************************
 *** ANSI Terminal
 ***********************************************************************/

#define ANSI_NORMAL		"\e[0m"
#define ANSI_BOLD		"\e[1m"
#define ANSI_DIM		"\e[2m"
#define ANSI_STANDOUT		"\e[3m"
#define ANSI_UNDERLINE		"\e[4m"
#define ANSI_BLINK		"\e[5m"
#define ANSI_REVERSE		"\e[6m"
#define ANSI_HIDE		"\e[7m"

#define ANSI_BLACK		"\e[30m"
#define ANSI_RED		"\e[31m"
#define ANSI_GREEN		"\e[32m"
#define ANSI_YELLOW		"\e[33m"
#define ANSI_BLUE		"\e[34m"
#define ANSI_MAGENTA		"\e[35m"
#define ANSI_CYAN		"\e[36m"
#define ANSI_WHITE		"\e[37m"

#define ANSI_BG_BLACK		"\e[40m"
#define ANSI_BG_RED		"\e[41m"
#define ANSI_BG_GREEN		"\e[42m"
#define ANSI_BG_YELLOW		"\e[43m"
#define ANSI_BG_BLUE		"\e[44m"
#define ANSI_BG_MAGENTA		"\e[45m"
#define ANSI_BG_CYAN		"\e[46m"
#define ANSI_BG_WHITE		"\e[47m"

#define ANSI_END_LINE		"\e[256G"
#define ANSI_HOME_LINE		"\e[1G"
#define ANSI_GOTO		"\e[%u;%uH"	/* CSI row ; col H 1-based */
#define ANSI_RIGHT		"\e[%uC"	/* CSI col C 1-based */
#define ANSI_COLUMN		"\e[%uG"	/* CSI col G 1-based */
#define ANSI_ERASE_TAIL		"\e[0K"
#define ANSI_ERASE_HEAD		"\e[1K"
#define ANSI_ERASE_LINE		"\e[2K"

#define ANSI_REPORT		"\e[6n"		/* CSI row ; col R */
#define ANSI_SAVE_CURSOR	"\e[s"
#define ANSI_RESTORE_CURSOR	"\e[u"

/***********************************************************************
 *** END
 ***********************************************************************/

#ifdef  __cplusplus
}
#endif

#endif /* __ansiterm_h__ */
