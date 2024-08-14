#######################################################################
#	Thus it begins...
#######################################################################

AC_INIT(post4, 0.10.0, [Anthony Howe <achowe@snert.com>])

dnl The autoconf version I learned to deal with.
AC_PREREQ(2.59)

SNERT_INIT([P4], [Copyright 2007, 2024 by Anthony Howe. All rights reserved.])
AC_DEFINE_UNQUOTED(P4_PLATFORM, [["${platform}"]])

#######################################################################
#	Setup paths
#######################################################################

AC_SUBST(rel_tardir, '../../../..')

AC_CONFIG_SRCDIR(src/post4.c)

#######################################################################
#	Auxliary files to be modified later
#######################################################################

AC_CONFIG_HEADERS(src/config.h)

AC_CONFIG_FILES([
	makefile
	jni/makefile
	src/makefile
	test/makefile
	src/config.h.in
])

#######################################################################
#	Configuration options: settings
#######################################################################

SNERT_OPTION_ENABLE_32BIT
SNERT_OPTION_ENABLE_64BIT
SNERT_OPTION_ENABLE_DEBUG
SNERT_OPTION_ENABLE_MATH

dnl Needs updating since much changed on Cygwin WRT mingw.
dnl SNERT_OPTION_ENABLE_MINGW

AC_ARG_ENABLE(see,[AS_HELP_STRING([--disable-see],[disable support for SEE])],[
	:
],[
	enable_see='yes'
])
AS_IF([test ${enable_see:-no} = 'yes'],[AC_DEFINE(HAVE_SEE)])

AC_ARG_ENABLE(hooks,[AS_HELP_STRING([--disable-hooks],[disable support for hooks])],[
	:
],[
	enable_hooks='yes'
])
AS_IF([test ${enable_hooks:-no} = 'yes'],[AC_DEFINE(HAVE_HOOKS)])

AC_ARG_ENABLE(exception-strings,[AS_HELP_STRING([--disable-exception-strings],[disable exception strings in error messages])],[
	:
],[
	enable_exception_strings='yes'
])
AS_IF([test ${enable_exception_strings:-no} = 'yes'],[AC_DEFINE(USE_EXCEPTION_STRINGS)])

#######################################################################
#	Configuration options: packages
#######################################################################

AC_ARG_WITH(java,[AS_HELP_STRING([--with-java],[build Java Native Interface support])],[
	AS_IF(
		[test $with_java = 'no'],[unset JAVA_HOME],
		[test $with_java != 'yes'],[export JAVA_HOME="$with_java"]
	)
],[
	with_java='yes'
])
AS_IF([test $with_java != 'no'],[AC_DEFINE_UNQUOTED(WITH_JAVA, ["$with_java"])])

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

SNERT_TAR_SETTINGS

dnl Tradional cc options.
dnl NOTE SunOS as(1) _wants_ a space between -o and its argument.
CC_E='-o'
CC_E_NAME='-o $@'
CC_O='-o'
CC_O_NAME='-o $''*$O'
LD=$CC

AC_SUBST(CC_E)
AC_SUBST(CC_E_NAME)
AC_SUBST(CC_O)
AC_SUBST(CC_O_NAME)

AC_CHECK_HEADERS([ctype.h errno.h inttypes.h limits.h stdarg.h stddef.h stdint.h stdio.h unistd.h sys/ioctl.h])

AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(size_t)
AC_CHECK_SIZEOF(off_t)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(long double)
AC_CHECK_SIZEOF(long long int)
AC_CHECK_SIZEOF(intptr_t)

#######################################################################
#	Check for library, header, and function families.
#######################################################################

SNERT_ANSI_TIME
SNERT_ANSI_STRING
SNERT_EXTRA_STRING
SNERT_EXTRA_STDIO
SNERT_OPTIONS
SNERT_REGEX
SNERT_SETJMP
SNERT_POSIX_IO
SNERT_FILE_LOCKS
SNERT_TERMIOS

AS_IF([test -d "${JAVA_HOME}/include"],[
	JNI_DIR=$(dirname $(find $JAVA_HOME/include -name jni.h))
	JNI_DIR=$(echo "$JNI_DIR" | sed -e "s,${JAVA_HOME},\\\${JAVA_HOME},")

	JNI_MD_DIR=$(dirname $(find $JAVA_HOME/include -name jni_md.h))
	JNI_MD_DIR=$(echo "$JNI_MD_DIR" | sed -e "s,${JAVA_HOME},\\\${JAVA_HOME},")

	JNI_CFLAGS="-I${JNI_MD_DIR} -I${JNI_DIR}${JNI_CFLAGS:+ $JNI_CFLAGS}"
	EXTRA_BUILDS='${TOPDIR}/jni'${EXTRA_BUILDS:+ EXTRA_BUILDS}
])
AC_SUBST(EXTRA_BUILDS)
AC_SUBST(JNI_CFLAGS)
AC_SUBST(JFLAGS)

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
AC_CHECK_TYPES([time_t])

#######################################################################
#	Check for library functions
#######################################################################

AS_ECHO()
AS_ECHO("Checking assorted...")
AS_ECHO()

AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_LIBOBJ(malloc)

AC_CHECK_FUNCS(atexit getenv putenv toupper tolower)

#######################################################################
#	Generate output.
#######################################################################

SNERT_FINI

AS_ECHO()
AS_ECHO("Generating files...")
AS_ECHO()
AC_OUTPUT()
SNERT_SUMMARY
AC_MSG_RESULT([  JAVA_HOME......: $JAVA_HOME])
AC_MSG_RESULT([  JNI_CFLAGS.....: $JNI_CFLAGS])
AC_MSG_RESULT([  JFLAGS.........: $JFLAGS])
AS_ECHO()

#######################################################################
#	-END-
#######################################################################
