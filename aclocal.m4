dnl
dnl aclocal.m4
dnl
dnl Copyright 2002, 2011 by Anthony Howe.  All rights reserved.
dnl

dnl AC_LANG(C)
dnl ----------
dnl CFLAGS is not in ac_cpp because -g, -O, etc. are not valid cpp options.
dnl
dnl Handle Borland C++ 5.5 where it has none standard output flags that
dnl require the filename argument be abutted to the option letter.
dnl
m4_define([AC_LANG(C)],
[ac_ext=c
dnl Mac OS X gcc 3.x wants to option and argument to be separated.
dnl Don't people read standards like POSIX and SUS?
CC_E='-o '
ac_cpp='$CPP $CPPFLAGS'
ac_compile='$CC -c $CFLAGS $CPPFLAGS conftest.$ac_ext >&AS_MESSAGE_LOG_FD'
ac_link='$CC ${CC_E}conftest$ac_exeext $CFLAGS $CPPFLAGS $LDFLAGS conftest.$ac_ext $LIBS >&AS_MESSAGE_LOG_FD'
ac_compiler_gnu=$ac_cv_c_compiler_gnu
])

dnl
dnl SNERT_GCC_SETTINGS
dnl
m4_define([SNERT_GCC_SETTINGS],[
	AS_IF([test $GCC = 'yes'],[
		GCC_MAJOR=`$CC -dM -E -xc /dev/null | sed -n -e 's/.*__GNUC__ \(.*\)/\1/p'`
		GCC_MINOR=`$CC -dM -E -xc /dev/null | sed -n -e 's/.*__GNUC_MINOR__ \(.*\)/\1/p'`
dnl		AS_IF([test $GCC_MAJOR -ge 4],[CFLAGS="-Wno-pointer-sign $CFLAGS"])
		AS_IF([test $GCC_MAJOR -ge 3],[CFLAGS="-Wno-char-subscripts $CFLAGS"])
		CFLAGS="-Wall $CFLAGS"
	])
	AS_IF([test ${enable_debug:-no} = 'no'],[
		CFLAGS="-O2 ${CFLAGS}"
		LDFLAGS="${LDFLAGS}"
	],[
		CFLAGS="-O0 -g ${CFLAGS}"
	])

	if test ${platform:-UNKNOWN} = 'CYGWIN'; then
		AS_IF([test ${enable_debug:-no} = 'no'],[CFLAGS="-s ${CFLAGS}"])
		CFLAGS="-I/usr/include/w32api ${CFLAGS}"
		LDFLAGS="-L/usr/lib/w32api ${LDFLAGS}"

dnl 		if test ${enable_win32:-no} = 'yes'; then
dnl 			dnl -s		strip, no symbols
dnl 			dnl -mno-cygwin	native windows console app
dnl 			dnl -mwindows	native windows gui app
dnl 			dnl -lws2_32	WinSock2 library
dnl 			CFLAGS="-mno-cygwin ${CFLAGS}"
dnl 			LIBS="-lws2_32 ${LIBS}"
dnl 		fi
	fi
])

dnl
dnl SNERT_BCC_SETTINGS
dnl
m4_define([SNERT_BCC_SETTINGS],[
	# This assumes a Cygwin environment with Boland C++ 5.5 CLI.
	#
	# Borland C++ 5.5 CLI tools don't have all the standard options
	# and require that arguments be abutted with the option letter.

	platform='BorlandC'

	ac_libext='lib'
	ac_objext='obj'
	ac_exeext='.exe'
	LIBEXT=$ac_libext
	OBJEXT=$ac_objext
	EXEEXT=$ac_exeext

	CPP='cpp32'
	CPPFLAGS=

	CFLAGS="-d -w-8064 ${CFLAGS}"
	if test ${enable_debug:-no} = 'no'; then
		CFLAGS="-O1 -v- ${CFLAGS}"
	else
		CFLAGS="-v ${CFLAGS}"
	fi

	CC_E='-e'
	CC_E_NAME='-e$''*$E'
	CC_O='-o'
	CC_O_NAME='-o$''*$O'
	COMPILE='${CC} ${CFLAGS} ${CC_O}$''*$O -c $<'

	LD='bcc32'
	LDFLAGS='-q -lc -L"c:/Borland/Bcc55/lib" -L"c:/Borland/Bcc55/lib/PSDK"'
	LIBSNERT=`echo "$abs_tardir/com/snert/lib/libsnert.$LIBEXT" | sed -e 's,/,\\\\\\\\,g'`

	ARCHIVE='tlib /C /a ${LIBSNERT} $$obj'
	RANLIB='true'

	XARGSI='xargs -i{}'
	AC_SUBST(FAMILY, [WIN])
])

dnl
dnl SNERT_DEFAULT_CC_SETTINGS
dnl
m4_define([SNERT_DEFAULT_CC_SETTINGS],[
	dnl Tradional cc options.
	dnl NOTE SunOS as(1) _wants_ a space between -o and its argument.
	CC_E='-o'
	CC_E_NAME='-o $@'
	CC_O='-o'
	CC_O_NAME='-o $''*$O'
	LD=$CC

	# Where to find libsnert includes and library.
dnl	CFLAGS='-I${SNERT_INCDIR} '"${CFLAGS}"
dnl	LDFLAGS='-L${SNERT_LIBDIR} '"${LDFLAGS}"
	LIBSNERT='-lsnert'

	# Makefile macro to compile C into an object file.
	COMPILE='${CC} ${CFLAGS} ${CC_O} $''*$O -c $<'
dnl	COMPILE='${CC} ${CFLAGS} -c $<'

	# Makefile macro to archive an object file.
	ARCHIVE='${AR} rc ${LIB} $$obj'

	# Assume the following traditional extensions.
	ac_libext='a'
	LIBEXT=$ac_libext

	case "$CC" in
	gcc)
		SNERT_GCC_SETTINGS
		;;
	bcc32)
		SNERT_BCC_SETTINGS
		;;
	esac

	AC_PROG_CC

	case "$CC" in
	gcc)
		SNERT_GCC_SETTINGS
		;;
	esac

	AC_AIX

	AC_SUBST(CC_E)
	AC_SUBST(CC_E_NAME)
	AC_SUBST(CC_O)
	AC_SUBST(CC_O_NAME)
	AC_SUBST(COMPILE)
	AC_SUBST(ARCHIVE)
	AC_SUBST(LIBEXT)
	AC_SUBST(LIBSNERT)

	AC_CHECK_TOOL(RANLIB, ranlib, true)

	dnl Check for recent ANSI C additions that HAVE_HEADER_STDC check
	dnl doesn't distinguish between C89 and C99.
	SNERT_CHECK_DEFINE([va_copy], [stdarg.h])
])

AC_DEFUN([SNERT_TAR_SETTINGS],[
	AC_MSG_CHECKING(for tar file list option to use)
	if tar --version 2>&1 | grep '(GNU tar)' >/dev/null ; then
		TAR_I='-T'
	else
		TAR_I='-I'
	fi
	AC_SUBST(TAR_I)
	AC_MSG_RESULT($TAR_I)
])

dnl
dnl SNERT_CHECK_LIB(lib, symbol)
dnl
dnl Only check AC_CHECK_LIB if $lib not already in $LIBS
dnl
AC_DEFUN(SNERT_CHECK_LIB,[
	if echo "$LIBS" | grep -- "-l$1" >/dev/null ; then
		echo "checking for $2 in $1... (cached) yes"
	else
		AC_CHECK_LIB([$1], [$2])
	fi
])

dnl
dnl SNERT_CHECK_DEFINE(symbol, header_file)
dnl
AC_DEFUN(SNERT_CHECK_DEFINE,[
	AC_LANG_PUSH(C)
	AC_CACHE_CHECK([for $1],ac_cv_define_$1,[
		AC_RUN_IFELSE([
			AC_LANG_SOURCE([[
#include <$2>
int main()
{
#ifdef $1
	return 0;
#else
	return 1;
#endif
}
			]])
		],ac_cv_define_$1=yes, ac_cv_define_$1=no)
	])
	AC_LANG_POP(C)
	AS_IF([test $ac_cv_define_$1 = 'yes'],[
		AC_DEFINE_UNQUOTED([HAVE_MACRO_]translit($1, [a-z], [A-Z]))
	])
])


dnl
dnl SNERT_CHECK_PREDEFINE(symbol)
dnl
AC_DEFUN(SNERT_CHECK_PREDEFINE,[
	AC_LANG_PUSH(C)
	AC_CACHE_CHECK([for $1],ac_cv_define_$1,[
		AC_RUN_IFELSE([
			AC_LANG_SOURCE([[
int main()
{
#ifdef $1
	return 0;
#else
	return 1;
#endif
}
			]])
		],ac_cv_define_$1=yes, ac_cv_define_$1=no)
	])
	AC_LANG_POP(C)
	AS_IF([test $ac_cv_define_$1 = 'yes'],[
		AC_DEFINE_UNQUOTED([HAVE_MACRO_]translit($1, [a-z], [A-Z]))
	])
])


dnl
dnl SNERT_GET_NUMERIC_DEFINE(header_file, symbol)
dnl
AC_DEFUN(SNERT_GET_NUMERIC_DEFINE,[
	AS_VAR_PUSHDEF([ac_Header], [snert_define_$1_$2])dnl
	AC_CACHE_CHECK([for $2],[ac_Header],[
		AC_RUN_IFELSE([
#include <stdio.h>
#include <$1>
int main()
{
#ifdef $2
	FILE *fp;
	if ((fp = fopen("snert_output.txt", "w")) == NULL)
		return 1;
	fprintf(fp, "%d", $2);
	fclose(fp);
	return 0;
#else
	return 1;
#endif
}
		],[
			AS_VAR_SET([ac_Header], [[`cat snert_output.txt`]])
			rm -f snert_output.txt
		])
	])
	AS_VAR_POPDEF([ac_Header])dnl
])

AC_DEFUN(SNERT_OPTION_DB_185,[
	AC_ARG_ENABLE(db-185
		[AS_HELP_STRING([[--enable-db-185 ]],[link with DB 1.85])]
	)
])

dnl
dnl SNERT_BERKELEY_DB
dnl
dnl	Sets $with_db to yes or no
dnl	Sets $db_lib to the library -l option or an empty string.
dnl
AC_DEFUN(SNERT_OPTION_WITH_DB,[
	AC_ARG_WITH(db, [[  --with-db[=DIR]         include Berkeley DB support]])
])
AC_DEFUN(SNERT_BERKELEY_DB,[
AS_IF([test ${with_db:-default} != 'no'],[
	echo
	echo "Check for Berkeley DB support..."
	echo

	bdb_save_LIBS=$LIBS
	bdb_save_CFLAGS=$CFLAGS
	bdb_save_LDFLAGS=$LDFLAGS

	# Find short list of system directories to try.
	BDB_BASE_DIRS="$with_db /opt/csw/bdb4 /opt /usr/pkg /usr/local /usr"

	bdb_found='no'
	for d in $BDB_BASE_DIRS ; do
		if test -d "$d/include" ; then
			bdb_dir_list="$bdb_dir_list $d"
			bdb_i_dirs=`ls -d $d/include/db[[0-9]]* $d/include/db $d/include/. 2>/dev/null | sort -r`

			for BDB_I_DIR in $bdb_i_dirs ; do
				AC_MSG_CHECKING([for db.h in $BDB_I_DIR])

				if test -r $BDB_I_DIR/db.h ; then
					AC_MSG_RESULT(yes)
					v=`basename $BDB_I_DIR`
					if test $v = 'include' ; then
						 v='.'
					fi

					if test -d $d/lib64/$v -a "$v" != '.' ; then
						BDB_L_DIR="$d/lib64/$v"
					elif test -d $d/lib/$v -a "$v" != '.'; then
						BDB_L_DIR="$d/lib/$v"
					elif test -d $d/lib64 ; then
						BDB_L_DIR="$d/lib64"
					else
						BDB_L_DIR="$d/lib"
					fi

					if test ${BDB_I_DIR} != '/usr/include/.' ; then
						CFLAGS="-I$BDB_I_DIR $CFLAGS"
					fi
					if test ${BDB_L_DIR} != '/usr/lib64' -a ${BDB_L_DIR} != '/usr/lib' ; then
						LDFLAGS="-L$BDB_L_DIR $LDFLAGS"
					fi

					bdb_major=`grep DB_VERSION_MAJOR $BDB_I_DIR/db.h | cut -f 3`
					if test -n "$bdb_major" ; then
						bdb_minor=`grep DB_VERSION_MINOR $BDB_I_DIR/db.h | cut -f 3`
						bdb_create='db_create'
					else
						bdb_major=1
						bdb_minor=85
						bdb_create='dbopen'
					fi

					for l in $v db ''; do
						if test -n "$l" ; then
							LIBS="-l$l $LIBS"
							bdb_name=$l
						else
							bdb_name='libc'
						fi

						AC_MSG_CHECKING([for $bdb_create in library $bdb_name])

						AC_LINK_IFELSE([
							AC_LANG_SOURCE([[
#include <stdlib.h>
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#include <db.h>

int
main(int argc, char **argv)
{
#ifdef DB_VERSION_MAJOR
	DB *db = NULL;
	return db_create(&db, NULL, 0) != 0 || db == NULL;
#else
	DB *db = dbopen("access.db", 0, 0, 0, NULL);
	return 0;
#endif
}
							]])
						],[
							bdb_found='yes'
							AC_DEFINE_UNQUOTED(HAVE_DB_H)
							if test $bdb_major = 1 ; then
								AC_DEFINE_UNQUOTED(HAVE_DBOPEN)
							else
								AC_DEFINE_UNQUOTED(HAVE_DB_CREATE)
							fi
							if test -n "$l" ; then
								AC_SUBST(HAVE_LIB_DB, "-l$l")
								AC_SUBST(CFLAGS_DB, "-I$BDB_I_DIR")
								AC_SUBST(LDFLAGS_DB, "-L$BDB_L_DIR")

								AC_DEFINE_UNQUOTED(HAVE_LIB_DB, "-l$l")
								AC_DEFINE_UNQUOTED(LDFLAGS_DB, "-I$BDB_I_DIR")
								AC_DEFINE_UNQUOTED(CFLAGS_DB, "-L$BDB_L_DIR")

							fi
						])
						AC_MSG_RESULT($bdb_found)
						LIBS="$bdb_save_LIBS"

						test ${bdb_found:-no} = 'yes' && break
					done
					test ${bdb_found:-no} = 'yes' && break

					LDFLAGS="$bdb_save_LDFLAGS"
					CFLAGS="$bdb_save_CFLAGS"
				else
					AC_MSG_RESULT(no)
				fi
			done
			test ${bdb_found:-no} = 'yes' && break
		fi
	done
	bdb_version="$bdb_major.$bdb_minor"

	if test ${bdb_found:-no} = 'yes' -a ${bdb_version} != '1.85' ; then
		AC_MSG_CHECKING([for dbopen in library libc])

		AC_LINK_IFELSE([
			AC_LANG_SOURCE([[
#include <stdlib.h>
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#include <db.h>

int
main(int argc, char **argv)
{
	DB *db = dbopen("access.db", 0, 0, 0, NULL);
	return 0;
}
			]])
		],[
			dbopen_found='yes'
			AC_DEFINE_UNQUOTED(HAVE_DB_H)
			AC_DEFINE_UNQUOTED(HAVE_DBOPEN)
		],[
			dbopen_found='no'
		])
		AC_MSG_RESULT(${dbopen_found})
	fi

	LDFLAGS="$bdb_save_LDFLAGS"
	CFLAGS="$bdb_save_CFLAGS"
	AC_MSG_RESULT([checking best Berkeley DB version... $bdb_version])
])
])

dnl
dnl SNERT_FIND_LIB([name],[found],[notfound])
dnl
m4_define([SNERT_FIND_LIB],[
	AS_VAR_PUSHDEF([snert_lib], [snert_find_lib_$1])dnl
	echo
	echo "Finding dynamic library $1 ..."
	echo
	AS_VAR_SET([snert_lib], 'no')
	AC_CHECK_HEADER([dlfcn.h], [
		AC_DEFINE_UNQUOTED(HAVE_DLFCN_H)
		AC_CHECK_TOOL(ldd_tool, ldd)
		if test ${ldd_tool:-no} != 'no'	; then
			AC_CHECK_LIB([dl],[dlopen])
			AC_MSG_CHECKING([for $1])
			AC_RUN_IFELSE([
				AC_LANG_SOURCE([[
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_DLFCN_H
# include <dlfcn.h>
#endif

int
main(int argc, char **argv)
{
	void *handle;
	handle = dlopen(argv[1], RTLD_NOW);
	return dlerror() != NULL;
}
				]])
			],[
				libpath=[`$ldd_tool ./conftest$ac_exeext | sed -n -e "/lib$1/s/.* \([^ ]*lib$1[^ ]*\).*/\1/p"`]
				if ./conftest$ac_exeext ${libpath:-unknown} ; then
					AS_VAR_SET([snert_lib], [${libpath}])
				else
					AS_VAR_SET([snert_lib], 'no')
				fi
			])
			AC_MSG_RESULT(AS_VAR_GET([snert_lib]))
		fi
	])
	AS_IF([test AS_VAR_GET([snert_lib]) != 'no'], [$2], [$3])[]
	AS_VAR_POPDEF([snert_lib])
])

AC_DEFUN(SNERT_FIND_LIBC,[
	SNERT_FIND_LIB([c],[AC_DEFINE_UNQUOTED(LIBC_PATH, ["$snert_find_lib_c"])], [])
])


dnl
dnl SNERT_PLATFORM
dnl
AC_DEFUN(SNERT_PLATFORM,[
	platform=`uname -s|sed -e 's/^\([[a-zA-Z0-9]]*\)[[^a-zA-Z0-9]].*/\1/'`
	AC_SUBST(platform, $platform)
	echo "platform is... $platform"
	if test -e /etc/debian_version ; then
		echo "this Linux is a Debian" `cat /etc/debian_version`
		apt-get install -y gcc libc6-dev
		isDebian='yes'
	fi

	AC_CHECK_TOOL(MD5SUM, md5sum)
	if test ${MD5SUM:-no} = 'no' ; then
		AC_CHECK_TOOL(MD5SUM, md5)
	fi

	case "$platform" in
	NetBSD)
		LDFLAGS="-R/usr/pkg/lib $LDFLAGS"
		;;
	esac


	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_PLATFORM, [["${platform}"]])
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_BUILD_HOST, [["`hostname`"]])

dnl	case $platform in
dnl	Linux*)
dnl		kernel=`uname -r`
dnl		case "$kernel" in
dnl		1.*|2.0.*)
dnl			echo "Linux kernel... $kernel"
dnl			dnl Older Linux kernels have a broken poll() where it
dnl			dnl might block indefinitely in nanosleep().
dnl			AC_DEFINE_UNQUOTED(HAS_BROKEN_POLL)
dnl			;;
dnl		esac
dnl	esac
])

dnl
dnl SNERT_CHECK_CONFIGURE
dnl
AC_DEFUN(SNERT_CHECK_CONFIGURE,[
	# When we have no makefile, do it ourselves...
dnl	snert_configure_command="$[]0 $[]@"

	AC_CHECK_TOOL([AUTOCONF], [autoconf-2.61])
	if test ${AUTOCONF:-no} = 'no' ; then
		AC_CHECK_TOOL([AUTOCONF], [autoconf-2.59])
		if test ${AUTOCONF:-no} = 'no' ; then
			AC_CHECK_TOOL([AUTOCONF], [autoconf], [true])
		fi
	fi

	if test ${AUTOCONF:-no} != 'no' -a \( aclocal.m4 -nt configure -o configure.in -nt configure -o configure.ac -nt configure \); then
		echo 'Rebuilding the configure script first...'
		${AUTOCONF} -f
		echo 'Restarting configure script...'
		echo $snert_configure_command
 		exec $snert_configure_command
	fi
])

dnl
dnl SNERT_LIBMILTER
dnl
dnl Depends on SNERT_PTHREAD
dnl
AC_DEFUN(SNERT_OPTION_WITH_MILTER,[
	AC_ARG_WITH(lua, [[  --with-milter[=DIR]     include milter support]])
])
AC_DEFUN(SNERT_LIBMILTER,[
	echo
	echo "Check for sendmail's libmilter library & header support..."
	echo

	saved_libs="$LIBS"
	saved_cflags="$CFLAGS"
	saved_ldflags="$LDFLAGS"

if test ${with_milter:-default} != 'no' ; then
	for d in "$with_milter" /usr /usr/local /usr/pkg ; do
		unset ac_cv_search_smfi_main
		unset ac_cv_header_libmilter_mfapi_h

		if test X$d != X ; then
			CFLAGS_MILTER="-I$d/include"
			if test -d $d/lib/libmilter ; then
				LDFLAGS_MILTER="-L$d/lib/libmilter"
			else
				LDFLAGS_MILTER="-L$d/lib"
			fi
		fi
		echo "trying with $LDFLAGS_MILTER ..."

		CFLAGS="$CFLAGS_MILTER $saved_cflags"
		LDFLAGS="$LDFLAGS_MILTER $saved_ldflags"

		AC_SEARCH_LIBS([smfi_main], [milter], [AC_DEFINE_UNQUOTED(HAVE_LIBMILTER, "-lmilter -lpthread")], [], [-lpthread])
		AC_CHECK_HEADERS([libmilter/mfapi.h],[],[],[/* */])

		if test "$ac_cv_search_smfi_main" != 'no' -a "$ac_cv_header_libmilter_mfapi_h" != 'no' ; then
			LIBS="-lmilter -lpthread $saved_libs"
			AC_CHECK_FUNCS([smfi_addheader smfi_addrcpt smfi_addrcpt_par smfi_chgfrom smfi_chgheader smfi_delrcpt smfi_getpriv smfi_getsymval smfi_insheader smfi_main smfi_opensocket smfi_progress smfi_quarantine smfi_register smfi_replacebody smfi_setbacklog smfi_setconn smfi_setdbg smfi_setmaxdatasize smfi_setmlreply smfi_setpriv smfi_setreply smfi_setsymlist smfi_settimeout smfi_stop smfi_version])

			AC_SUBST(HAVE_LIB_MILTER, "-lmilter")
			AC_SUBST(LDFLAGS_MILTER)
			AC_SUBST(CFLAGS_MILTER)

			AC_DEFINE_UNQUOTED(LDFLAGS_MILTER, "${LDFLAGS_MILTER}")
			AC_DEFINE_UNQUOTED(CFLAGS_MILTER, "${CFLAGS_MILTER}")

			with_milter="$d"
			break
		fi
	done

	LIBS="$saved_libs"
	CFLAGS="$saved_cflags"
	LDFLAGS="$saved_ldflags"
fi
])

dnl
dnl SNERT_LIBSNERT
dnl
AC_DEFUN(SNERT_LIBSNERT,[
	saved_cflags=$CFLAGS
	saved_ldflags=$LDFLAGS

	CFLAGS="-I../../include $CFLAGS"
	LDFLAGS="-L../../lib $LDFLAGS"

	AC_CHECK_LIB(snert, parsePath)
	if test "$ac_cv_lib_snert_parsePath" = 'no'; then
		echo
		AC_MSG_WARN([The companion library, LibSnert, is required.])
		echo
		ac_cv_lib_snert_parsePath='required'
		CFLAGS=$saved_cflags
		LDFLAGS=$saved_ldflags
	fi
	snert_libsnert=$ac_cv_lib_snert_parsePath

])

dnl
dnl SNERT_OPTION_ENABLE_DEBUG
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_DEBUG,[
	dnl Assert that CFLAGS is defined. When AC_PROC_CC is called to
	dnl check the compiler and CC == gcc is found and CFLAGS is
	dnl undefined, then it gets assigned "-g -O2", which is just
	dnl annoying when you want the default to no debugging.
	CFLAGS="${CFLAGS}"

	AC_ARG_ENABLE(debug,[AS_HELP_STRING([--enable-debug],[enable compiler debug option])])
	AS_IF([test ${enable_debug:-no} = 'yes'],[
		CFLAGS="-g -O0${CFLAGS:+ $CFLAGS}"
	],[
		AC_DEFINE(NDEBUG,[1],[Disable debug code])
		CFLAGS="-Os${CFLAGS:+ $CFLAGS}"
		enable_debug='no'
	])
])

AC_DEFUN(SNERT_OPTION_ENABLE_64BIT,[
	AC_ARG_ENABLE(64bit,
		[AS_HELP_STRING([--enable-64bit ],[enable compile & link options for 64-bit])],
		[
			CFLAGS="-m64 ${CFLAGS}"
			LDFLAGS="-m64 ${LDFLAGS}"
		],[
			dnl Option not specified, then choose based on CPU.
			case `uname -m` in
			x86_64|amd64)
				CFLAGS="-m64 ${CFLAGS}"
				LDFLAGS="-m64 ${LDFLAGS}"
				;;
			i386)
				CFLAGS="-m32 ${CFLAGS}"
				LDFLAGS="-m32 ${LDFLAGS}"
				;;
			esac
		]
	)
])

dnl
dnl SNERT_OPTION_ENABLE_MATH
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_MATH,[
	AC_ARG_ENABLE(math,
		[AS_HELP_STRING([--disable-math ],[disable libm support])],
		[
			AS_IF([test ${enable_math:-no} = 'yes'],[
				AC_CHECK_HEADERS([math.h],[LIBS="${LIBS:+$LIBS }-lm"])
			])
		],
		[
			AC_CHECK_HEADERS([math.h],[LIBS="${LIBS:+$LIBS }-lm";enable_math='yes'])
		]
	)
])

dnl
dnl SNERT_OPTION_ENABLE_WIN32
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_MINGW,[
	AC_ARG_ENABLE(mingw,
		[AS_HELP_STRING([--enable-mingw ],[generate native Windows application using mingw])],
		[
			enable_win32='yes'
			AC_SUBST(ENABLE_MINGW, 'yes')
 			CFLAGS="-mno-cygwin -I/usr/include/w32api ${CFLAGS}"
			LDFLAGS="-mno-cygwin -L/usr/lib/w32api ${LDFLAGS}"
		]
	)
])

dnl
dnl SNERT_OPTION_WITH_WINDOWS_SDK
dnl
AC_DEFUN(SNERT_OPTION_WITH_WINDOWS_SDK,[
	AC_ARG_WITH(windows-sdk,
		[AS_HELP_STRING([--with-windows-sdk=dir ],[Windows Platform SDK base directory])],
		[
			enable_win32='yes'
			AC_SUBST(WITH_WINDOWS_SDK, $with_windows_sdk)
			AC_DEFINE(WITH_WINDOWS_SDK, $with_windows_sdk)
 			CFLAGS="-mno-cygwin -I${with_windows_sdk}/include ${CFLAGS}"
			LDFLAGS="-mno-cygwin -L${with_windows_sdk}/lib ${LDFLAGS}"
		]
	)
])

dnl
dnl SNERT_OPTION_ENABLE_BCC32
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_BCC32,[
	AC_ARG_ENABLE(bcc32,
		[AS_HELP_STRING([--enable-bcc32 ],[generate native Windows application using Borland C++ 5.5])],
		[
			enable_bcc32='yes'
			CC=bcc32
		]
	)
])

dnl
dnl SNERT_OPTION_ENABLE_RUN_USER(default_user_name)
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_RUN_USER,[
	AC_ARG_ENABLE(run-user,
		[AS_HELP_STRING([--enable-run-user=user ],[specifiy the process user name, default is "$1"])],
		[enable_run_user="$enableval"], [enable_run_user=$1]
	)
	AC_DEFINE_UNQUOTED(RUN_AS_USER, ["$enable_run_user"])
	AC_SUBST(enable_run_user)
])

dnl
dnl SNERT_OPTION_ENABLE_RUN_GROUP(default_group_name)
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_RUN_GROUP,[
	AC_ARG_ENABLE(run-group,
		[AS_HELP_STRING([--enable-run-group=group ],[specifiy the process group name, default is "$1"])],
		[enable_run_group="$enableval"], [enable_run_group=$1]
	)
	AC_DEFINE_UNQUOTED(RUN_AS_GROUP, ["$enable_run_group"])
	AC_SUBST(enable_run_group)
])

dnl
dnl SNERT_OPTION_ENABLE_CACHE_TYPE(default_cache_type)
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_CACHE_TYPE,[
	AC_ARG_ENABLE(cache-type,
		[AS_HELP_STRING([--enable-cache-type=type ],[specifiy the cache type: bdb, flatfile, hash])],
		[
			# Force a specific type.
			case "$enableval" in
			bdb|flatfile|hash)
				enable_cache_type="$enableval"
				AC_DEFINE_UNQUOTED(CACHE_TYPE, ["$enable_cache_type"])
				;;
			*)
				enable_cache_type="$1"
			esac
		], [
			# Depends on whether Berkeley DB is available.
			enable_cache_type="$1"
		]
	)
])

dnl
dnl SNERT_OPTION_ENABLE_CACHE_FILE
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_CACHE_FILE,[
	AC_ARG_ENABLE(cache-file,
		[AS_HELP_STRING([--enable-cache-file=filepath ],[specifiy the cache file])],
		[
			enable_cache_file="$enableval"
		], [
			if test -d /var/cache ; then
				# Linux http://www.pathname.com/fhs/pub/fhs-2.3.html
				enable_cache_file='${localstatedir}/cache/${PACKAGE_NAME}.db'
			elif test -d /var/db ; then
				# FreeBSD, OpenBSD, NetBSD
				enable_cache_file='${localstatedir}/db/${PACKAGE_NAME}.db'
			else
				echo "Cannot find a suitable location for the cache file."
				echo "Please specify --enable-cache-file."
				exit 1
			fi
		]
	)
	snert_cache_file=`eval echo $enable_cache_file`
	AC_DEFINE_UNQUOTED(CACHE_FILE, ["`eval echo ${snert_cache_file}`"])
	AC_SUBST(snert_cache_file)
	AC_SUBST(enable_cache_file)
])

dnl
dnl SNERT_OPTION_ENABLE_PID(default)
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_PID,[
	AC_ARG_ENABLE(pid,
		[AS_HELP_STRING([--enable-pid=filepath ],[specifiy an alternative pid file path])],
		[
		],[
			dnl Almost all unix machines agree on this location.
			if test -z "$1"; then
				enable_pid='${localstatedir}/run/${PACKAGE_NAME}.pid'
			else
				enable_pid=$1
			fi
		]
	)
	snert_pid_file=`eval echo $enable_pid`
	AC_DEFINE_UNQUOTED(PID_FILE, ["`eval echo ${snert_pid_file}`"])
	AC_SUBST(snert_pid_file)
	AC_SUBST(enable_pid)
])

dnl
dnl SNERT_OPTION_ENABLE_SOCKET(default)
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_SOCKET,[
	AC_ARG_ENABLE(socket,
		[AS_HELP_STRING([--enable-socket=filepath ],[specifiy an alternative Unix domain socket])],
		[
		],[
			dnl Almost all unix machines agree on this location.
			dnl Note though that if you have more than one file
			dnl here, its recommended to create a subdirectory
			dnl with all the related files.
			if test -z "$1"; then
				enable_socket='${localstatedir}/run/${PACKAGE_NAME}.socket'
			else
				enable_socket=$1
			fi
		]
	)
	snert_socket_file=`eval echo $enable_socket`
	AC_DEFINE_UNQUOTED(SOCKET_FILE, ["`eval echo ${snert_socket_file}`"])
	AC_SUBST(snert_socket_file)
	AC_SUBST(enable_socket)
])

dnl
dnl SNERT_OPTION_ENABLE_STARTUP_DIR
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_STARTUP_DIR,[
	AC_ARG_ENABLE(startup-dir,
		[AS_HELP_STRING([--enable-startup-dir=dir ],[specifiy the startup script directory location])],
		[
			STARTUP_DIR="$enableval"
			STARTUP_EXT='.sh'
		], [
			if test -d '/usr/local/etc/rc.d'; then
				# FreeBSD
				STARTUP_DIR='/usr/local/etc/rc.d'
				STARTUP_EXT='.sh'
			elif test -d '/etc/init.d'; then
				# SunOS, Debian Linux, System V variant
				STARTUP_DIR='/etc/init.d'
			elif test -d '/etc/rc.d/init.d'; then
				# Linux, System V variant
				STARTUP_DIR='/etc/rc.d/init.d'
			else
				# OpenBSD has no place to put startup
				# scripts that might be used by rc.local.
				STARTUP_DIR='/usr/local/sbin'
				STARTUP_EXT='.sh'
			fi
		]
	)
	AC_SUBST(STARTUP_DIR)
	AC_SUBST(STARTUP_EXT)
])

dnl
dnl SNERT_OPTION_ENABLE_FORK
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_FORK,[
	AC_ARG_ENABLE(
		fork, [AS_HELP_STRING([--enable-fork],[use process fork model instead of threads])],
		[
			AC_DEFINE_UNQUOTED(ENABLE_FORK)
		]
	)
	AC_SUBST(enable_fork)
])

dnl
dnl SNERT_FUNC_FLOCK
dnl
AC_DEFUN(SNERT_FUNC_FLOCK,[
	echo
	echo "Check for flock() support..."
	echo
	AC_CHECK_HEADER(sys/file.h, [
		AC_DEFINE_UNQUOTED(HAVE_SYS_FILE_H)

		SNERT_CHECK_DEFINE(LOCK_SH, sys/file.h)
		if test $ac_cv_define_LOCK_SH = 'yes'; then
			AC_CHECK_FUNC(flock)
		fi
	])
])

dnl
dnl SNERT_OPTION_ENABLE_FCNTL_LOCKS
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_FCNTL_LOCKS,[
	AC_ARG_ENABLE(fcntl-locks,
		[AS_HELP_STRING([--enable-fcntl-locks],[use fcntl() file locking instead of flock()])],
		[
		],[
			dnl Option not specified, choose based on OS.
			case "$platform" in
			Linux)
				enable_fcntl_locks='yes'
				;;
			esac
		]
	)
	AS_IF([test ${enable_fcntl_locks:-'no'} = 'yes'],[AC_DEFINE_UNQUOTED(ENABLE_ALT_FLOCK)])
	AC_SUBST(enable_alt_flock)
])

AC_DEFUN(SNERT_FILE_LOCKS,[
	AS_ECHO()
	AS_ECHO("Check for file locking...")
	AS_ECHO()
	AC_CHECK_HEADERS([fcntl.h sys/file.h])
	AC_CHECK_FUNCS(flock fcntl lockf locking)
	SNERT_CHECK_DEFINE(O_BINARY, fcntl.h)
	SNERT_CHECK_DEFINE(LOCK_SH, fcntl.h)
])

dnl
dnl SNERT_POSIX_IO
dnl
AC_DEFUN(SNERT_POSIX_IO,[
	echo
	echo "Check for POSIX File & Directory I/O support..."
	echo
	AC_HEADER_DIRENT
dnl autoconf says the following should be included:
dnl
dnl #if HAVE_DIRENT_H
dnl # include <dirent.h>
dnl # define NAMLEN(dirent) strlen((dirent)->d_name)
dnl #else
dnl # define dirent direct
dnl # define NAMLEN(dirent) (dirent)->d_namlen
dnl # if HAVE_SYS_NDIR_H
dnl #  include <sys/ndir.h>
dnl # endif
dnl # if HAVE_SYS_DIR_H
dnl #  include <sys/dir.h>
dnl # endif
dnl # if HAVE_NDIR_H
dnl #  include <ndir.h>
dnl # endif
dnl #endif

	AC_CHECK_HEADERS([unistd.h fcntl.h sys/stat.h utime.h])
	AC_CHECK_FUNCS([chdir getcwd mkdir rmdir closedir opendir readdir])
	AC_CHECK_FUNCS([chmod chown chroot fchmod stat fstat link rename symlink unlink umask utime])
	AC_CHECK_FUNCS([close creat dup dup2 ftruncate chsize truncate lseek open pipe read write])
	AC_CHECK_FUNCS([isatty getdtablesize])
	AC_FUNC_CHOWN
])

dnl
dnl SNERT_POSIX_SIGNALS
dnl
AC_DEFUN(SNERT_POSIX_SIGNALS,[
	echo
	echo "Check for POSIX signal support..."
	echo
	AC_CHECK_HEADER([signal.h],[
		AC_DEFINE_UNQUOTED(HAVE_SIGNAL_H)
		AC_CHECK_TYPES([sigset_t, struct sigaction, struct sigaltstack],[],[],[
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SIGNAL_H
# include <signal.h>
#endif
		])
		AC_CHECK_FUNCS([kill sigaltstack sigemptyset sigfillset sigaddset sigdelset sigismember])
		AC_CHECK_FUNCS([sigaction sigprocmask sigpending sigsuspend])

		dnl _POSIX_REAL_TIME_SIGNALS
		dnl AC_CHECK_FUNCS([sigwaitinfo sigtimedwait sigqueue])
	])
])

dnl
dnl SNERT_PROCESS
dnl
AC_DEFUN(SNERT_PROCESS,[
	echo
	echo "Check for process support..."
	echo
	AC_CHECK_HEADER([unistd.h],[
		AC_DEFINE_UNQUOTED(HAVE_UNISTD_H)
		AC_CHECK_FUNCS([getuid getgid setuid setgid])
		AC_CHECK_FUNCS([geteuid getegid seteuid setegid getpgid setpgid])
		AC_CHECK_FUNCS([getresuid getresgid setresuid setresgid])
		AC_CHECK_FUNCS([setreuid getgroups setgroups initgroups])
		AC_CHECK_FUNCS([_exit exit daemon fork execl execle execlp execv execve execvp setsid])
	])
	AC_CHECK_HEADER([sys/wait.h],[
		AC_DEFINE_UNQUOTED(HAVE_SYS_WAIT_H)
		AC_CHECK_FUNCS([wait wait3 wait4 waitpid])
	])

	AC_CHECK_HEADER([sys/resource.h],[
		AC_DEFINE_UNQUOTED(HAVE_SYS_RESOURCE_H)
		AC_CHECK_TYPES([struct rlimit, rlim_t],[],[],[
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
#include <sys/resource.h>
		])
		AC_CHECK_FUNCS([getrlimit setrlimit])

	])
	AC_CHECK_HEADERS([limits.h sysexits.h syslog.h])
])

dnl
dnl SNERT_SETJMP
dnl
AC_DEFUN(SNERT_SETJMP,[
	echo
	echo "Check for setjmp support..."
	echo
	AC_CHECK_HEADER([setjmp.h], [
		AC_DEFINE_UNQUOTED(HAVE_SETJMP_H)
		AC_CHECK_TYPES([jmp_buf, sigjmp_buf],[],[],[
#ifdef HAVE_SETJMP_H
# include <setjmp.h>
#endif
		])
	])
])

dnl
dnl SNERT_OPTIONS
dnl
AC_DEFUN(SNERT_OPTIONS,[
	echo
	echo "Check for option support..."
	echo
	AC_CHECK_HEADER([unistd.h], [
		AC_DEFINE_UNQUOTED(HAVE_UNISTD_H)
		AC_CHECK_FUNCS([getopt])
	])
])

dnl
dnl SNERT_RANDOM
dnl
AC_DEFUN(SNERT_RANDOM,[
	echo
	echo "Check for random support..."
	echo
	AC_CHECK_HEADER([stdlib.h], [
		AC_DEFINE_UNQUOTED(HAVE_STDLIB_H)
		AC_CHECK_FUNCS([srand rand rand_r random srandom initstate setstate])
	])
])

dnl
dnl SNERT_ANSI_STRING
dnl
AC_DEFUN(SNERT_ANSI_STRING,[
	echo
	echo "Check for ANSI string functions..."
	echo
	AC_CHECK_FUNCS(memchr memcmp memcpy memmove memset)
	AC_FUNC_MEMCMP
	AC_LIBOBJ(memcmp)
	AC_CHECK_FUNCS(strcat strncat strcpy strncpy strcmp strncmp strxfrm)
	AC_CHECK_FUNCS(strchr strcspn strerror strlen strpbrk strrchr strspn strstr strtok)
	AC_FUNC_STRCOLL
	AC_FUNC_STRERROR_R
])

dnl
dnl SNERT_ANSI_TIME
dnl
AC_DEFUN(SNERT_ANSI_TIME,[
	echo
	echo "Check for ANSI & supplemental time support..."
	echo

dnl	saved_libs=$LIBS

	case "${platform}" in
	Linux|SunOS|Solaris)
		SNERT_CHECK_LIB(rt, clock_gettime)
		if test "$ac_cv_lib_rt_clock_gettime" != 'no'; then
			AC_SUBST(HAVE_LIB_RT, '-lrt')
		fi
		;;
	esac

	AC_CHECK_HEADERS(time.h sys/time.h)
dnl	AC_HEADER_TIME
dnl autoconf says the following should be included:
dnl
dnl #ifdef TIME_WITH_SYS_TIME
dnl # include <sys/time.h>
dnl # include <time.h>
dnl #else
dnl # ifdef HAVE_SYS_TIME_H
dnl #  include <sys/time.h>
dnl # else
dnl #  include <time.h>
dnl # endif
dnl #endif
	AC_CHECK_FUNCS(clock difftime mktime time asctime ctime gmtime localtime tzset sleep usleep nanosleep)
	AC_CHECK_FUNCS(asctime_r ctime_r gmtime_r localtime_r clock_gettime gettimeofday)
	AC_CHECK_FUNCS(alarm getitimer setitimer)
	dnl These are typically macros:  timerclear timerisset timercmp timersub timeradd
	AC_FUNC_MKTIME
	AC_LIBOBJ(mktime)
	AC_FUNC_STRFTIME
	AC_CHECK_TYPES([struct timespec, struct timeval],[],[],[
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
	])

	AC_STRUCT_TM
	AC_STRUCT_TIMEZONE

dnl	LIBS=$saved_libs
])

dnl
dnl SNERT_EXTRA_STDIO
dnl
AC_DEFUN(SNERT_EXTRA_STDIO,[
	echo
	echo "Check for supplemental stdio support..."
	echo
	SNERT_CHECK_PREDEFINE(__CYGWIN__)
	AC_CHECK_HEADERS([io.h err.h])
	AC_CHECK_FUNCS(getdelim getline getprogname setprogname err errx warn warnx verr verrx vwarn vwarnx)
])

dnl
dnl SNERT_EXTRA_STRING
dnl
AC_DEFUN(SNERT_EXTRA_STRING,[
	echo
	echo "Check for supplemental string support..."
	echo
	AC_CHECK_FUNCS(strdup strndup strtol strlcpy strlcat strcasecmp strncasecmp)
	AC_CHECK_FUNCS(snprintf vsnprintf setproctitle)
	AC_FUNC_VPRINTF
])

dnl
dnl SNERT_REGEX
dnl
AC_DEFUN(SNERT_REGEX,[
	echo
	echo "Check for regex..."
	echo
	AC_CHECK_HEADERS([regex.h],[
		AC_SEARCH_LIBS([regcomp], [regex])
		AC_CHECK_FUNCS(regcomp regexec regerror regfree)
	])
])

dnl
dnl SNERT_TERMIOS
dnl
AC_DEFUN(SNERT_TERMIOS,[
	echo
	echo "Check for termios..."
	echo
	AC_CHECK_HEADERS([termios.h],[
		AC_CHECK_FUNCS(tcgetattr tcsetattr tcgetwinsize tcsetwinsize ctermid)
	])
	AC_CHECK_TYPES([struct winsize],[],[],[
#include <termios.h>
	])
])

dnl
dnl SNERT_NETWORK
dnl
AC_DEFUN(SNERT_NETWORK,[
	echo
	echo "Check for Network services..."
	echo
	SNERT_CHECK_PREDEFINE(__WIN32__)

	if test "$ac_cv_define___WIN32__" = 'no' ; then
		AC_SEARCH_LIBS([socket], [socket nsl])
		AC_SEARCH_LIBS([inet_aton], [socket nsl resolv])

		AC_CHECK_HEADERS([ \
			sys/socket.h netinet/in.h netinet/in6.h netinet6/in6.h \
			netinet/tcp.h poll.h sys/poll.h sys/select.h sys/un.h \
			arpa/inet.h \
		])

dnl When using poll() use this block.
dnl
dnl #ifdef HAVE_POLL_H
dnl # include <poll.h>
dnl # ifndef INFTIM
dnl #  define INFTIM	(-1)
dnl # endif
dnl #endif

dnl When using kqueue() use this block.
dnl
dnl #ifdef HAVE_SYS_EVENT_H
dnl # include <sys/types.h>
dnl # include <sys/event.h>
dnl # include <sys/time.h>
dnl # ifndef INFTIM
dnl #  define INFTIM	(-1)
dnl # endif
dnl #endif

		AC_CHECK_FUNCS([ \
			inet_pton inet_aton inet_addr inet_ntoa inet_ntop \
			accept bind connect listen poll select shutdown socket \
			getpeereid getpeername getsockname getsockopt setsockopt \
			recv recvfrom recvmsg send sendmsg sendto \
			htonl htons ntohl ntohs \
		])

		AC_CHECK_HEADERS([sys/event.h],[AC_CHECK_FUNCS([kqueue kevent])])
		AC_CHECK_HEADERS([sys/epoll.h],[AC_CHECK_FUNCS([epoll_create epoll_ctl epoll_wait epoll_pwait])])

		AC_CHECK_HEADERS([netdb.h],[
			AC_CHECK_FUNCS([ \
				getaddrinfo freeaddrinfo getnameinfo \
				gethostname gethostbyname gethostbyname2 gethostbyaddr \
				gethostbyname_r gethostbyname2_r gethostbyaddr_r \
				gethostent sethostent endhostent hstrerror herror \
				getservent getservbyport getservbyname setservent endservent \
				getprotoent getprotobynumber getprotobyname setprotoent endprotoent \
			])
		])

		AC_CHECK_HEADERS([ifaddrs.h],[
			AC_CHECK_FUNCS([ \
				getifaddrs freeifaddrs
			])
		])
		AC_CHECK_HEADERS([net/if.h],[
			AC_CHECK_FUNCS([ \
				if_nameindex if_freenameindex if_nametoindex if_indextoname
			])
		])
	else
		AC_CHECK_HEADERS(windows.h io.h)
		AC_CHECK_HEADER(winsock2.h,[
			AC_DEFINE_UNQUOTED(AS_TR_CPP([HAVE_]winsock2.h))
		],[],[
#if defined(__WIN32__) && defined(HAVE_WINDOWS_H)
# include  <windows.h>
#endif
		])
		AC_CHECK_HEADER(ws2tcpip.h,[
			AC_DEFINE_UNQUOTED(AS_TR_CPP([HAVE_]ws2tcpip.h))
		],[],[
#if defined(__WIN32__)
# if defined(HAVE_WINDOWS_H)
#  include  <windows.h>
# endif
# if defined(HAVE_WINSOCK2_H)
#  include  <winsock2.h>
# endif
#endif
		])

		for i in \
			accept \
			bind \
			closesocket \
			connect \
			endservent \
			getpeername \
			getprotobyname \
			getprotobynumber \
			getservbyname \
			getservbyport \
			getservent \
			getsockname \
			getsockopt \
			htonl \
			htons \
			inet_addr \
			inet_ntoa \
			listen \
			ntohl \
			ntohs \
			recv \
			recvfrom \
			select \
			send \
			sendto \
			setservent \
			setsockopt \
			shutdown \
			socket \
			getaddrinfo freeaddrinfo getnameinfo \
			gethostname gethostbyname gethostbyaddr
		do
			AC_MSG_CHECKING([for $i])
			AC_DEFINE_UNQUOTED(AS_TR_CPP([HAVE_]$i))
			AC_MSG_RESULT([assumed in winsock2.h & ws2tcpip.h])
		done
	fi

	if test ${platform:-UNKNOWN} = 'CYGWIN' -o ${ac_cv_define___WIN32__:-no} != 'no'; then
		AC_SUBST(HAVE_LIB_WS2_32, '-lws2_32')
		AC_SUBST(HAVE_LIB_IPHLPAPI, '-lIphlpapi')
		NETWORK_LIBS="-lws2_32 -lIphlpapi $NETWORK_LIBS"
		AC_SUBST(NETWORK_LIBS, ${NETWORK_LIBS})
	fi

		AC_CHECK_TYPES([struct sockaddr_in6, struct in6_addr, struct sockaddr_un, socklen_t],[],[],[
#if defined(__WIN32__)
# define WINVER	0x0501
# if defined(HAVE_WINDOWS_H)
#  include  <windows.h>
# endif
# if defined(HAVE_WINSOCK2_H)
#  include  <winsock2.h>
# endif
# if defined(HAVE_WS2TCPIP_H)
#  include <ws2tcpip.h>
# endif
#else
# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif
# ifdef HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
# endif
# ifdef HAVE_SYS_UN_H
#  include <sys/un.h>
# endif
# ifdef HAVE_NETINET_IN_H
#  include <netinet/in.h>
# endif
# ifdef HAVE_NETINET_IN6_H
#  include <netinet/in6.h>
# endif
# ifdef HAVE_NETINET6_IN6_H
#  include <netinet6/in6.h>
# endif
#endif
		])
		AC_CHECK_MEMBERS([struct sockaddr.sa_len, struct sockaddr_in.sin_len, struct sockaddr_in6.sin6_len, struct sockaddr_un.sun_len],[],[],[
#if defined(__WIN32__)
# define WINVER	0x0501
# if defined(HAVE_WINDOWS_H)
#  include  <windows.h>
# endif
# if defined(HAVE_WINSOCK2_H)
#  include  <winsock2.h>
# endif
# if defined(HAVE_WS2TCPIP_H)
#  include <ws2tcpip.h>
# endif
#else
# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif
# ifdef HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
# endif
# ifdef HAVE_SYS_UN_H
#  include <sys/un.h>
# endif
# ifdef HAVE_NETINET_IN_H
#  include <netinet/in.h>
# endif
# ifdef HAVE_NETINET_IN6_H
#  include <netinet/in6.h>
# endif
# ifdef HAVE_NETINET6_IN6_H
#  include <netinet6/in6.h>
# endif
#endif
		])

])

dnl
dnl SNERT_INIT($c_macro_prefix, $copyright, $build_id_file)
dnl
AC_DEFUN(SNERT_INIT,[
	snert_macro_prefix="$1"

	AC_COPYRIGHT([$2])

	dnl Used for summary display. Note that the build number should be
	dnl passed on the compiler command line within the makefiles using
	dnl -D in order to make sure we have the most recent build number.
	dnl
	dnl Placing the build number into config.h using substitutions
	dnl means we have to rerun ./configure in order update config.status
	dnl when the build number changes. This is slow and cumbersome during
	dnl development.

	AC_SUBST(package_copyright, ['$2'])
	AC_SUBST(package_major, [[`echo $PACKAGE_VERSION | cut -f1 -d.`]])
	AC_SUBST(package_minor, [[`echo $PACKAGE_VERSION | cut -f2 -d.`]])
	AC_SUBST(package_patch, [[`echo $PACKAGE_VERSION | cut -f3 -d.`]])

	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_NAME, ["$PACKAGE_NAME"])
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_MAJOR, $package_major)
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_MINOR, $package_minor)
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_PATCH, $package_patch)
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_VERSION, ["$PACKAGE_VERSION"])
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_STRING, ["$PACKAGE_STRING"])

	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_AUTHOR, ["$PACKAGE_BUGREPORT"])
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_COPYRIGHT, ["$package_copyright"])

	snert_configure_command="$[]0"
	for arg in $ac_configure_args; do
		dnl skip environment variables that should appear BEFORE configure
		case $arg in
		CFLAGS=*|LDFLAGS=*)
			continue
			;;
		esac
		dnl Remove previous quoting of single quote, place single quotes around option value
		arg=`echo "$arg" | sed -e "s/'\\\\\\\\'//g" -e "s/^'\(.*\)'$/\1/" -e "s/\([[^=]]*=\)\([[^']].*[[^']]\)/\1'\2'/"`
		snert_configure_command="${snert_configure_command} [$]arg"
	done
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_CONFIGURE, ["$snert_configure_command"])

	snert_build_date=`date +'%a, %d %b %Y %H:%M:%S %z'`
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_BUILT, ["$snert_build_date"])

	SNERT_PLATFORM
	SNERT_CHECK_CONFIGURE

	dnl Define some default vaules for milters subsitutions.
	if test X"$1" = X'MILTER' ; then
		test "$prefix" = NONE -a "$localstatedir" = '${prefix}/var' && localstatedir='/var'
		AC_DEFINE_UNQUOTED(snert_milter_t_equate, [C:5m;S=10s;R=10s;E:5m])
	fi

	echo
	echo "$PACKAGE_NAME/$PACKAGE_VERSION"
	echo $package_copyright
	echo
])

AC_DEFUN(SNERT_FINI,[
	dnl Append CPPFLAGS to CFLAGS until we convert to use automake.
	CFLAGS="$CFLAGS $CPPFLAGS"

	dnl Escape double-quotes in CFLAGS for -DMACRO='"string"' case.
	quote_cflags=$(echo $CFLAGS | sed -e's/"/\\&/g')
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_CFLAGS, ["$quote_cflags"])
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_CPPFLAGS, ["$CPPFLAGS"])
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_LDFLAGS, ["$LDFLAGS"])
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_LIBS, ["$LIBS"])
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_SHARE, ["$datarootdir/share"])
])

dnl
dnl SNERT_SUMMARY
dnl
AC_DEFUN(SNERT_SUMMARY,[
	AS_ECHO()
	AS_ECHO("$PACKAGE_NAME/$PACKAGE_VERSION")
	AS_ECHO("$package_copyright")
	AS_ECHO()
	AC_MSG_RESULT([  Platform.......: $platform $CC])
	AC_MSG_RESULT([  prefix.........: $prefix])
	AC_MSG_RESULT([  exec_prefix....: $exec_prefix])
	AC_MSG_RESULT([  CFLAGS.........: $CFLAGS])
	AC_MSG_RESULT([  CPPFLAGS.......: $CPPFLAGS])
	AC_MSG_RESULT([  LDFLAGS........: $LDFLAGS])
	AC_MSG_RESULT([  LIBS...........: $LIBS])
])
