#######################################################################
#	Thus it begins...
#######################################################################

AC_INIT(post4, 0.8.0, [Anthony Howe <achowe@snert.com>])

dnl The autoconf version I learned to deal with.
AC_PREREQ(2.59)

SNERT_INIT([P4], [Copyright 2007, 2023 by Anthony Howe. All rights reserved.])
AC_DEFINE_UNQUOTED(P4_PLATFORM, [["${platform}"]])

#######################################################################
#	Setup paths
#######################################################################

AC_SUBST(rel_tardir, '../../../..')

AC_CONFIG_SRCDIR(src/post4.c)

#######################################################################
#	Auxliary files to be modified later
#######################################################################

AC_CONFIG_FILES(makefile)
AC_CONFIG_FILES(src/makefile)
AC_CONFIG_FILES(src/config.h.in)
AC_CONFIG_HEADERS(src/config.h)

#######################################################################
#	Configuration options: settings
#######################################################################

SNERT_OPTION_ENABLE_64BIT
SNERT_OPTION_ENABLE_DEBUG
SNERT_OPTION_ENABLE_MATH

dnl Needs updating since much changed on Cygwin WRT mingw.
dnl SNERT_OPTION_ENABLE_MINGW

#######################################################################
#	Configuration options: packages
#######################################################################


#######################################################################
#	Check for programs
#######################################################################

AS_ECHO()
AS_ECHO("Checking for assorted tools...")
AS_ECHO()

AC_PROG_CC
AC_PROG_LN_S
AC_PROG_RANLIB
AC_PROG_MAKE_SET

dnl AS_IF([test $GCC_MAJOR -ge 4],[CFLAGS="-Wno-pointer-sign $CFLAGS"])
SNERT_TAR_SETTINGS

#######################################################################
#	Check for library, header, and function families.
#######################################################################

SNERT_ANSI_TIME
SNERT_ANSI_STRING
SNERT_EXTRA_STRING
SNERT_OPTIONS
SNERT_REGEX
SNERT_SETJMP
SNERT_POSIX_IO
SNERT_FILE_LOCKS
SNERT_TERMIOS

AC_CHECK_HEADERS([ctype.h errno.h inttypes.h limits.h stddef.h stdint.h stdio.h unistd.h sys/ioctl.h])

#######################################################################
#	Check for types.
#######################################################################

AS_ECHO()
AS_ECHO("Checking for common types...")
AS_ECHO()

AC_C_CONST
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UID_T
AC_TYPE_INTPTR_T
AC_TYPE_UINTPTR_T
dnl AC_CHECK_TYPES([gid_t, time_t])

#######################################################################
#	Check for library functions
#######################################################################

AS_ECHO()
AS_ECHO("Checking assorted...")
AS_ECHO()

AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_LIBOBJ(malloc)

#######################################################################
#	Generate output.
#######################################################################

SNERT_FINI

AS_ECHO()
AS_ECHO("Generating files...")
AS_ECHO()
AC_OUTPUT()
SNERT_SUMMARY
AS_ECHO()

#######################################################################
#	-END-
#######################################################################
