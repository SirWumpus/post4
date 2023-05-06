/*
 * Trival err.h
 */

#ifndef __err_h__
#define __err_h__

#ifdef __cplusplus
extern "C" {
#endif

#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

#define warn(...)	warnc(errno, __VA_ARGS__)
#define warnx(...)	{ \
	(void) fprintf(stderr, __VA_ARGS__); \
	(void) fputc('\n', stderr); \
}
#define warnc(code, ...)	{ \
	(void) fprintf(stderr, __VA_ARGS__); \
	(void) fprintf(stderr, ": %s\n", strerror(code)); \
}

#define vwarn(fmt, args)  vwarnc(errno, fmt, args)
#define vwarnx(fmt, args) { \
	(void) vfprintf(stderr, fmt, args); \
	(void) fputc('\n', stderr); \
}
#define vwarnc(code, fmt, args)  { \
	(void) vfprintf(stderr, fmt, args); \
	(void) fprintf(stderr, "%s%s\n", (fmt) ? ": " : "", strerror(code)); \
}

#define err(ex, ...)  errc(ex, errno, __VA_ARGS__)
#define errx(ex, ...) { \
	(void) fprintf(stderr, __VA_ARGS__); \
	(void) fputc('\n', stderr); \
	exit(ex); \
}
#define errc(ex, code, ...)  { \
	(void) fprintf(stderr, __VA_ARGS__); \
	(void) fprintf(stderr, ": %s\n", strerror(code)); \
	exit(ex); \
}

#define verr(ex, fmt, args)  verrc(ex, errno, fmt args)
#define verrx(ex, fmt, args) { \
	(void) vfprintf(stderr, fmt, args); \
	(void) fputc('\n', stderr); \
	exit(ex); \
}
#define verrc(ex, code, fmt, args) { \
	(void) vfprintf(stderr, fmt, args); \
	(void) fprintf(stderr, "%s%s\n", (fmt) ? ": " : "", strerror(code)); \
	exit(ex); \
}

#ifdef  __cplusplus
}
#endif

#endif /* __err_h__ */
