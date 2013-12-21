#######################################################################
#	Thus it begins...
#######################################################################

AC_INIT(post4, 0.5, [Anthony Howe <achowe@snert.com>])

dnl The autoconf version I learned to deal with.
AC_PREREQ(2.59)

SNERT_INIT([P4], [Copyright 2007, 2013 by Anthony Howe. All rights reserved.], [src/BUILD_ID.TXT])
AC_DEFINE_UNQUOTED(P4_PLATFORM, [["${platform}"]])

#######################################################################
#	Setup paths
#######################################################################

AC_SUBST(rel_tardir, '../../../..')

AC_CONFIG_SRCDIR(src/p4.c)

#######################################################################
#	Auxliary files to be modified later
#######################################################################

AC_CONFIG_FILES(MANIFEST.TXT)
AC_CONFIG_FILES(makefile)
AC_CONFIG_FILES(src/makefile)
AC_CONFIG_FILES(src/config.h.in)
AC_CONFIG_HEADERS(src/config.h)

#######################################################################
#	Configuration options: settings
#######################################################################

SNERT_OPTION_ENABLE_64BIT
SNERT_OPTION_ENABLE_DEBUG
SNERT_OPTION_ENABLE_MINGW

#######################################################################
#	Configuration options: packages
#######################################################################


#######################################################################
#	Check for programs
#
#	Must do this AFTER setting up Borland specifc changes that
#	AC_PROG_CC won't detect otherwise.
#######################################################################

SNERT_DEFAULT_CC_SETTINGS
dnl AS_IF([test $GCC_MAJOR -ge 4],[CFLAGS="-Wno-pointer-sign $CFLAGS"])
SNERT_TAR_SETTINGS

AC_PROG_MAKE_SET

AC_C_CONST
AC_CHECK_TYPES([mode_t, off_t, pid_t, uid_t, gid_t, size_t, ssize_t, time_t])

#######################################################################
#	Check for library, header, and function families.
#######################################################################

AC_CHECK_HEADERS([unistd.h sys/ioctl.h])

SNERT_ANSI_TIME
SNERT_ANSI_STRING
SNERT_EXTRA_STRING
SNERT_OPTIONS
SNERT_REGEX
SNERT_SETJMP
SNERT_POSIX_IO
SNERT_TERMIOS

#######################################################################
#	Check for library functions
#######################################################################

AC_FUNC_MALLOC
AC_LIBOBJ(malloc)

#######################################################################
#	Generate output.
#######################################################################

SNERT_FINI

echo
echo "Generating files..."
echo
AC_OUTPUT()
SNERT_SUMMARY
echo

#######################################################################
#	-END-
#######################################################################
