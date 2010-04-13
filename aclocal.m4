dnl
dnl aclocal.m4
dnl
dnl Copyright 2002, 2010 by Anthony Howe.  All rights reserved.
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
	CFLAGS="-Wall ${CFLAGS}"
	if test ${enable_debug:-no} = 'no'; then
		CFLAGS="-O2 ${CFLAGS}"
		LDFLAGS="${LDFLAGS}"
	else
		CFLAGS="-O0 -g ${CFLAGS}"
	fi

	if test ${platform:-UNKNOWN} = 'CYGWIN'; then
		if test ${enable_debug:-no} = 'no'; then
			CFLAGS="-s ${CFLAGS}"
		fi
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
	AC_CACHE_CHECK([for $1],ac_cv_define_$1,[
		AC_RUN_IFELSE([
#include <$2>
int main()
{
#ifdef $1
	return 0;
#else
	return 1;
#endif
}
		],ac_cv_define_$1=yes, ac_cv_define_$1=no)
	])
])


dnl
dnl SNERT_CHECK_PREDEFINE(symbol)
dnl
AC_DEFUN(SNERT_CHECK_PREDEFINE,[
	AC_CACHE_CHECK([for $1],ac_cv_define_$1,[
		AC_RUN_IFELSE([
int main()
{
#ifdef $1
	return 0;
#else
	return 1;
#endif
}
		],ac_cv_define_$1=yes, ac_cv_define_$1=no)
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
		[AC_HELP_STRING([[--enable-db-185 ]],[link with DB 1.85])]
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
	echo
	echo "Check for Berkeley DB support..."
	echo
if test ${with_db:-yes} = 'no' ; then
	bdb_version='disabled'
	echo "Berkeley DB support... disabled"
else
	bdb_save_LIBS=$LIBS
	bdb_save_CFLAGS=$CFLAGS
	bdb_save_LDFLAGS=$LDFLAGS

	count=1
	while test $count -gt 0 ; do
		unset ac_cv_header_db_h
		unset ac_cv_search_dbopen

		BDB_BASE_DIRS="$with_db /opt/csw/bdb4 /opt /usr/local /usr"
		BDB_VERSIONS='4.7 4.6 4.5 4.4 4.3 4.2 4.1 4.0 3.3 3.2'
		BDB_NAMES='db-4.7 db4.7 db47 db-4.6 db4.6 db46 db-4.5 db4.5 db45 db-4.4 db4.4 db44 db-4.3 db4.3 db43 db-4.2 db4.2 db42 db-4.1 db4.1 db41 db-4 db4 db-3.3 db3.3 db33 db-3.2 db3.2 db32 db-3 db3 db'

		# Find short list of directories to try.
		for d in $BDB_BASE_DIRS ; do
			if test -r $d/include -a -d $d/lib ; then
				bdb_dir_list="$bdb_dir_list $d"
			fi

			for v in $BDB_VERSIONS ; do
				if test -r $d/BerkeleyDB.$v ; then
					bdb_dir_list="$bdb_dir_list $d/BerkeleyDB.$v"
				fi
			done
		done

		bdb_found='no'
		for d in $bdb_dir_list ; do
			for v in $BDB_NAMES . ; do
				AC_MSG_CHECKING([for db.h in $d/include/$v])

				if test -r $d/include/$v/db.h ; then
					AC_MSG_RESULT(yes)

					BDB_I_DIR="$d/include/$v"

					if test -d $d/lib64/$v -a $v != '.' ; then
						BDB_L_DIR="$d/lib64/$v"
					elif test -d $d/lib/$v -a $v != '.'; then
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

					for l in db-$bdb_major.$bdb_minor db$bdb_major.$bdb_minor db$bdb_major$bdb_minor db$bdb_major db ''; do
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
		done

		bdb_version="$bdb_major.$bdb_minor"

		if test ${isDebian:-no} = 'yes' -a ${bdb_found:-no} = 'no'; then
			# Fetch headers matching library.
			AC_MSG_NOTICE([getting package libdb${bdb_version}-dev...])
			AS_IF([apt-get install -y libdb${bdb_version}-dev],[
				AC_MSG_NOTICE([getting package libdb${bdb_version}-dev... done])
				count=`expr $count + 1`
				AC_MSG_NOTICE([retrying after development package install...])
			],[
				AC_MSG_NOTICE([getting package libdb${bdb_version}-dev... FAILED])
			])
		fi

		count=`expr $count - 1`
	done

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

	AC_MSG_RESULT([checking best Berkeley DB version... $bdb_version])
fi
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
				if ./conftest$ac_exeext $libpath ; then
					AS_VAR_SET([snert_lib], [${libpath}])
				else
					AS_VAR_SET([snert_lib], 'no')
				fi
			])
			AC_MSG_RESULT(AS_VAR_GET([snert_lib]))
		fi
	])
	AS_IF([test AS_VAR_GET([snert_lib]) != 'no'], [$2], [$3])[]dnl
	AS_VAR_POPDEF([snert_lib])dnl
])

AC_DEFUN(SNERT_FIND_LIBPTHREAD,[
	saved_libs="$LIBS"
	LIBS="$LIBS $HAVE_LIB_PTHREAD"
	SNERT_FIND_LIB([pthread],[AC_DEFINE_UNQUOTED(LIBPTHREAD_PATH, ["$snert_find_lib_pthread"])], [])
	LIBS="$saved_libs"
])

AC_DEFUN(SNERT_FIND_LIBC,[
	SNERT_FIND_LIB([c],[AC_DEFINE_UNQUOTED(LIBC_PATH, ["$snert_find_lib_c"])], [])
dnl 	echo
dnl 	echo "Finding version of libc..."
dnl 	echo
dnl
dnl 	AC_CHECK_HEADER([dlfcn.h], [
dnl 		AC_DEFINE_UNQUOTED(HAVE_DLFCN_H)
dnl 		AC_CHECK_TOOL(ldd_tool, ldd)
dnl 		if test ${ldd_tool:-no} != 'no'	; then
dnl 			AC_CHECK_LIB([dl],[dlopen])
dnl 			AC_MSG_CHECKING([for libc])
dnl 			AC_RUN_IFELSE([
dnl 				AC_LANG_SOURCE([[
dnl #include <stdio.h>
dnl #include <stdlib.h>
dnl #ifdef HAVE_DLFCN_H
dnl # include <dlfcn.h>
dnl #endif
dnl
dnl int
dnl main(int argc, char **argv)
dnl {
dnl 	void *handle;
dnl 	handle = dlopen(argv[1], RTLD_NOW);
dnl 	return dlerror() != NULL;
dnl }
dnl 				]])
dnl 			],[
dnl 				libc=[`$ldd_tool ./conftest$ac_exeext | sed -n -e '/libc/s/.* \([^ ]*libc[^ ]*\).*/\1/p'`]
dnl 				if ./conftest$ac_exeext $libc ; then
dnl 					AC_DEFINE_UNQUOTED(LIBC_PATH, ["$libc"])
dnl 				else
dnl 					libc='not found'
dnl 				fi
dnl 			],[
dnl 				libc='not found'
dnl 			])
dnl 			AC_MSG_RESULT($libc)
dnl 		fi
dnl 	])
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
AC_DEFUN(SNERT_LIBMILTER,[
	echo
	echo "Check for sendmail's libmilter library & header support..."
	echo
	dnl Always add these paths, since its common practice to override
	dnl system defaults in a /usr/local hierarchy. Why this is not the
	dnl default for gcc is beyond me.
	saved_libs=$LIBS
	AC_CHECK_LIB(milter, smfi_main, [LIBS="-lmilter ${HAVE_LIB_PTHREAD} $LIBS"], [], [${HAVE_LIB_PTHREAD}])
	if test "$ac_cv_lib_milter_smfi_main" = 'no'; then
		save_ldflags=$LDFLAGS
		LDFLAGS="-L/usr/local/lib ${LDFLAGS}"
		unset ac_cv_lib_milter_smfi_main
		echo 'retrying with -L/usr/local/lib ...'
		AC_CHECK_LIB(milter, smfi_main, [LIBS="-lmilter ${HAVE_LIB_PTHREAD} $LIBS"], [], [${HAVE_LIB_PTHREAD}])
		if test "$ac_cv_lib_milter_smfi_main" = 'no'; then
			LDFLAGS=$save_ldflags
		fi
	fi
	if test ${isDebian:-no} = 'yes' -a "$ac_cv_lib_milter_smfi_main" = 'no'; then
		apt-get install -y libmilter-dev
		unset ac_cv_lib_milter_smfi_main
		echo 'retrying after development package update...'
		AC_CHECK_LIB(milter, smfi_main, [LIBS="-lmilter ${HAVE_LIB_PTHREAD} $LIBS"], [], [${HAVE_LIB_PTHREAD}])
	fi
	if test "$ac_cv_lib_milter_smfi_main" = 'no'; then
		ac_cv_lib_milter_smfi_main='required'
	else
		AC_SUBST(HAVE_LIB_MILTER, '-lmilter')
	fi

	AC_CHECK_HEADERS([libmilter/mfapi.h],[],[],[/* */])
	if test "$ac_cv_header_libmilter_mfapi_h" = 'no'; then
		saved_cflags=$CFLAGS
		CFLAGS="-I/usr/local/include ${CFLAGS}"
		unset ac_cv_header_libmilter_mfapi_h
		echo 'retrying with -I/usr/local/include ...'
		AC_CHECK_HEADERS([libmilter/mfapi.h],[],[],[/* */])
		if test "$ac_cv_header_libmilter_mfapi_h" = 'no'; then
			CFLAGS=$saved_cflags
		fi
	fi
	if test ${isDebian:-no} = 'yes' -a "$ac_cv_header_libmilter_mfapi_h" = 'no'; then
		apt-get install -y libmilter-dev
		unset ac_cv_header_libmilter_mfapi_h
		echo 'retrying after development package update...'
		AC_CHECK_HEADERS([libmilter/mfapi.h],[],[],[/* */])
	fi

	if test $ac_cv_header_libmilter_mfapi_h = 'yes' ; then
		AC_CHECK_FUNCS([smfi_addheader smfi_addrcpt smfi_chgheader smfi_delrcpt smfi_getpriv smfi_getsymval smfi_main smfi_register smfi_replacebody smfi_setbacklog smfi_setconn smfi_setdbg smfi_setpriv smfi_setreply smfi_settimeout smfi_stop])
		AC_CHECK_FUNCS([smfi_insheader smfi_opensocket smfi_progress smfi_quarantine smfi_setmlreply smfi_setsymlist smfi_version smfi_setmaxdatasize])
	fi

	AC_CHECK_MEMBERS([struct smfiDesc.xxfi_unknown, struct smfiDesc.xxfi_data],[],[],[
#ifdef HAVE_LIBMILTER_MFAPI_H
# define SMFI_VERSION	999		/* Look for all enhancements */
# include <libmilter/mfapi.h>
#endif
	])

	LIBS=$saved_libs
	snert_libmilter=$ac_cv_lib_milter_smfi_main
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
		LDFLAGS=$save_ldflags
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

	AC_ARG_ENABLE(debug,
		[AC_HELP_STRING([--enable-debug ],[enable compiler debug option])],
		[
			if test ${enable_debug:-no} = 'yes' ; then
				AC_DEFINE_UNQUOTED(LIBSNERT_DEBUG)
				enable_debug='yes'
			fi
		],[
			AC_DEFINE_UNQUOTED(NDEBUG)
			enable_debug='no'
		]
	)
])

AC_DEFUN(SNERT_OPTION_ENABLE_64BIT,[
	AC_ARG_ENABLE(64bit,
		[AC_HELP_STRING([--enable-64bit ],[enable compile & link options for 64-bit])],
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
dnl SNERT_OPTION_ENABLE_WIN32
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_MINGW,[
	AC_ARG_ENABLE(mingw,
		[AC_HELP_STRING([--enable-mingw ],[generate native Windows application using mingw])],
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
		[AC_HELP_STRING([--with-windows-sdk=dir ],[Windows Platform SDK base directory])],
		[
			enable_win32='yes'
			AC_SUBST(WITH_WINDOWS_SDK, $with_windows_sdk)
			AC_DEFINE(WITH_WINDOWS_SDK, $with_windows_sdk)
 			CFLAGS="-mno-cygwin -I${with_windows_sdk}/include ${CFLAGS}"
			LDFLAGS="-mno-cygwin -L${with_windows_sdk}/lib ${LDFLAGS}"
echo $LDFLAGS
		]
	)
])

dnl
dnl SNERT_OPTION_ENABLE_BCC32
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_BCC32,[
	AC_ARG_ENABLE(bcc32,
		[AC_HELP_STRING([--enable-bcc32 ],[generate native Windows application using Borland C++ 5.5])],
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
		[AC_HELP_STRING([--enable-run-user=user ],[specifiy the process user name, default is "$1"])],
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
		[AC_HELP_STRING([--enable-run-group=group ],[specifiy the process group name, default is "$1"])],
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
		[AC_HELP_STRING([--enable-cache-type=type ],[specifiy the cache type: bdb, flatfile, hash])],
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
		[AC_HELP_STRING([--enable-cache-file=filepath ],[specifiy the cache file])],
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
		[AC_HELP_STRING([--enable-pid=filepath ],[specifiy an alternative pid file path])],
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
		[AC_HELP_STRING([--enable-socket=filepath ],[specifiy an alternative Unix domain socket])],
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
dnl SNERT_OPTION_ENABLE_POPAUTH
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_POPAUTH,[
	AC_ARG_ENABLE(popauth,
		[AC_HELP_STRING([--enable-popauth ],[enable POP-before-SMTP macro checking in smf API])],
		[AC_DEFINE_UNQUOTED(HAVE_POP_BEFORE_SMTP)]
	)
])

dnl
dnl SNERT_OPTION_ENABLE_STARTUP_DIR
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_STARTUP_DIR,[
	AC_ARG_ENABLE(startup-dir,
		[AC_HELP_STRING([--enable-startup-dir=dir ],[specifiy the startup script directory location])],
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
dnl SNERT_OPTION_WITH_SENDMAIL
dnl
AC_DEFUN(SNERT_OPTION_WITH_SENDMAIL,[
	AC_ARG_WITH(sendmail,
		[AC_HELP_STRING([--with-sendmail=dir ],[directory where sendmail.cf lives, default /etc/mail])],
		[with_sendmail="$withval"], [with_sendmail='/etc/mail']
	)
	AC_SUBST(with_sendmail)
])

dnl
dnl SNERT_OPTION_ENABLE_FORK
dnl
AC_DEFUN(SNERT_OPTION_ENABLE_FORK,[
	AC_ARG_ENABLE(
		fork, [AC_HELP_STRING([--enable-fork],[use process fork model instead of threads])],
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
		[AC_HELP_STRING([--enable-fcntl-locks],[use fcntl() file locking instead of flock()])],
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
	AC_CHECK_FUNCS([chmod chown chroot fchmod stat fstat link rename unlink umask utime])
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
		AC_CHECK_FUNCS([setjmp longjmp sigsetjmp siglongjmp])
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
dnl SNERT_PTHREAD
dnl
AC_DEFUN(SNERT_OPTION_WITH_PTHREAD,[
	AC_ARG_WITH(pthread, [[  --with-pthread          include POSIX threads support]])
])
AC_DEFUN(SNERT_PTHREAD,[
	echo
	echo "Check for POSIX thread & mutex support..."
	echo
if test ${with_pthread:-yes} = 'no' ; then
	ac_cv_func_pthread_create='disabled'
	echo "POSIX thread support... disabled"
elif test ${enable_win32:-no} = 'yes' ; then
	ac_cv_func_pthread_create='limited'
	echo "POSIX thread support... limited Windows native"
else
	AC_CHECK_HEADER([pthread.h],[
		AC_DEFINE_UNQUOTED(HAVE_PTHREAD_H)

		saved_libs="$LIBS"
		AC_DEFINE(_REENTRANT)
		CFLAGS="-D_REENTRANT ${CFLAGS}"

		case "$platform" in
		FreeBSD|OpenBSD|NetBSD)
			AC_DEFINE(_THREAD_SAFE)
			CFLAGS="-D_THREAD_SAFE -pthread ${CFLAGS}"
			LDFLAGS="-pthread ${LDFLAGS}"
			;;
		esac

		dnl For SunOS. Beware of using AC_SEARCH_LIBS() on SunOS
		dnl platforms, because some functions appear as stubs in
		dnl other libraries.
		SNERT_CHECK_LIB(pthread, pthread_create)
		if test "$ac_cv_lib_pthread_pthread_create" != 'no'; then
			AC_SUBST(HAVE_LIB_PTHREAD, '-lpthread')
		fi

		AC_CHECK_TYPES([pthread_t, pthread_attr_t, pthread_mutex_t, pthread_mutexattr_t, pthread_once_t sigset_t],[],[],[
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SIGNAL_H
# include <signal.h>
#endif
#ifdef HAVE_PTHREAD_H
# include <pthread.h>
#endif
		])

		AC_CHECK_FUNCS([pthread_create pthread_cancel pthread_equal pthread_exit pthread_join pthread_kill pthread_self])
		dnl OpenBSD requires -pthread in order to link sigwait.
		AC_CHECK_FUNCS([pthread_detach pthread_yield pthread_sigmask sigwait])
		AC_CHECK_FUNCS([pthread_attr_init pthread_attr_destroy pthread_attr_getdetachstate pthread_attr_setdetachstate pthread_attr_getstackaddr pthread_attr_setstackaddr pthread_attr_getstacksize pthread_attr_setstacksize pthread_attr_getscope pthread_attr_setscope])
		AC_CHECK_FUNCS([pthread_mutex_init pthread_mutex_destroy pthread_mutex_lock pthread_mutex_trylock pthread_mutex_unlock])
		AC_CHECK_FUNCS([pthread_mutexattr_init pthread_mutexattr_destroy pthread_mutexattr_setprioceiling pthread_mutexattr_getprioceiling pthread_mutexattr_setprotocol pthread_mutexattr_getprotocol pthread_mutexattr_settype pthread_mutexattr_gettype])
		AC_CHECK_FUNCS([pthread_cond_broadcast pthread_cond_destroy pthread_cond_init pthread_cond_signal pthread_cond_timedwait pthread_cond_wait])
		AC_CHECK_FUNCS([pthread_rwlock_init pthread_rwlock_destroy pthread_rwlock_unlock pthread_rwlock_rdlock pthread_rwlock_wrlock pthread_rwlock_tryrdlock pthread_rwlock_trywrlock])
		AC_CHECK_FUNCS([pthread_lock_global_np pthread_unlock_global_np])
		AC_CHECK_FUNCS([pthread_key_create pthread_key_delete pthread_getspecific pthread_setspecific])
		AC_CHECK_FUNCS([pthread_once pthread_atfork])

		AC_CHECK_FUNC([pthread_cleanup_push],[
			AC_DEFINE_UNQUOTED(HAVE_PTHREAD_CLEANUP_PUSH)
		],[
			SNERT_CHECK_DEFINE(pthread_cleanup_push, pthread.h)
			if test $ac_cv_define_pthread_cleanup_push = 'yes'; then
				AC_DEFINE_UNQUOTED(HAVE_PTHREAD_CLEANUP_PUSH)
			fi
		])
		AC_CHECK_FUNC([pthread_cleanup_pop],[
			AC_DEFINE_UNQUOTED(HAVE_PTHREAD_CLEANUP_POP)
		],[
			SNERT_CHECK_DEFINE(pthread_cleanup_pop, pthread.h)
			if test $ac_cv_define_pthread_cleanup_pop = 'yes'; then
				AC_DEFINE_UNQUOTED(HAVE_PTHREAD_CLEANUP_POP)
			fi
		])

		LIBS="$saved_libs"
	])
fi
])

dnl
dnl SNERT_POSIX_SEMAPHORES
dnl
AC_DEFUN(SNERT_POSIX_SEMAPHORES,[
	echo
	echo "Check for POSIX semaphore support..."
	echo
	AC_CHECK_HEADER([semaphore.h],[
		AC_DEFINE_UNQUOTED(HAVE_SEMAPHORE_H)

dnl		saved_libs=$LIBS
		case "${platform}" in
		SunOS|Solaris)
			SNERT_CHECK_LIB(rt, sem_init)
			;;
		esac

		if test ${with_pthread:-yes} = 'no' ; then
			pthread=''
		else
			pthread='pthread'
		fi

		AC_SEARCH_LIBS([sem_init], [$pthread rt],[

			snert_posix_semaphores='yes'
			AC_CHECK_TYPES([sem_t],[],[],[
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SEMAPHORE_H
# include <semaphore.h>
#endif
			])
			AC_CHECK_FUNCS([sem_init sem_destroy sem_wait sem_post],[],[
				snert_posix_semaphores='no'
				break
			])
		],[
			snert_posix_semaphores='no'
		])

dnl		LIBS=$saved_libs
	],[
		snert_posix_semaphores='no'
	],[/* */])
])

dnl
dnl SNERT_SYSTEMV_SEMAPHORES
dnl
AC_DEFUN(SNERT_SYSTEMV_SEMAPHORES,[
	echo
	echo "Check for System V semaphore support..."
	echo
	snert_systemv_semaphores='yes'
	AC_CHECK_HEADERS([sys/ipc.h sys/sem.h],[],[snert_systemv_semaphores='no'],[
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
	])

	AC_CHECK_TYPES([union semun],[],[],[
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_IPC_H
# include <sys/ipc.h>
#endif
#ifdef HAVE_SYS_SEM_H
# include <sys/sem.h>
#endif
	])

	if test $snert_systemv_semaphores = 'yes'; then
		AC_CHECK_FUNCS([semget semctl semop],[],[
			snert_systemv_semaphores='no'
			break;
		])
	fi
])

dnl
dnl SNERT_PAM
dnl
AC_DEFUN(SNERT_PAM,[
	echo
	echo "Check for PAM support..."
	echo

	dnl Take care in detecting libpam, since its often an .so and
	dnl not all my software requires it. However, by detecting it,
	dnl its automatically added to $LIBS and so a version specific
	dnl reference could end up appearing in the dynamic library
	dnl start sequence. If the library has a major update, the old
	dnl one deleted, and the application didn't really require the
	dnl library, then it will refuse to start due to library version
	dnl change. Reported by Andrey Chernov
	dnl

	saved_libs=$LIBS

	AC_CHECK_LIB(pam, pam_start)
	if test "$ac_cv_lib_pam_pam_start" != 'no'; then
		AC_SUBST(HAVE_LIB_PAM, '-lpam')
	fi

	AC_CHECK_HEADERS(security/pam_appl.h)

	if test ${isDebian:-no} = 'yes' -a "$ac_cv_header_security_pam_appl_h" = 'no'; then
		apt-get install -y libpam0g-dev
		unset ac_cv_header_security_pam_appl_h
		echo 'retrying after development package update...'
		AC_CHECK_HEADERS(security/pam_appl.h)
	fi

	AC_CHECK_FUNCS(pam_start pam_authenticate pam_end)

	LIBS=$saved_libs

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

	saved_libs=$LIBS

	case "${platform}" in
	Linux|SunOS|Solaris)
		SNERT_CHECK_LIB(rt, clock_gettime)
		if test "$ac_cv_lib_rt_clock_gettime" != 'no'; then
			AC_SUBST(HAVE_LIB_RT, '-lrt')
		fi
		;;
	esac

	AC_CHECK_HEADERS(time.h sys/time.h)
	AC_HEADER_TIME
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

	LIBS=$saved_libs
])

dnl
dnl SNERT_EXTRA_STRING
dnl
AC_DEFUN(SNERT_EXTRA_STRING,[
	echo
	echo "Check for supplemental string support..."
	echo
	AC_CHECK_FUNCS(strdup strtol strlcpy strlcat strcasecmp strncasecmp)
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
dnl SNERT_OPTION_WITH_SQLITE3
dnl
dnl Depends on SNERT_PTHREAD
dnl
AC_DEFUN(SNERT_OPTION_WITH_SQLITE3,[
	AC_ARG_WITH(sqlite3, [[  --with-sqlite3[=DIR]    include SQLite3 support]])
])
AC_DEFUN(SNERT_SQLITE3,[
	echo
	echo "Check for SQLite3..."
	echo

if test ${with_sqlite3:-default} = 'no' ; then
	ac_cv_func_sqlite3_open='disabled'
	echo "SQLite3 support... disabled"
else
	saved_libs=$LIBS
	saved_cflags=$CFLAGS
	saved_ldflags=$LDFLAGS

	if test ${with_sqlite3:+set} = 'set' ; then
		CFLAGS="-I${with_sqlite3}/include $CFLAGS"
		LDFLAGS="-L${with_sqlite3}/lib $LDFLAGS"

		echo "trying with -L${with_sqlite3}/lib ..."
	fi

	dnl Take care in detecting libsqlite3, since its often an .so and
	dnl not all my software requires it. However, by detecting it,
	dnl it is automatically added to $LIBS and so a version specific
	dnl reference could end up appearing in the dynamic library
	dnl start sequence. If the library has a major update, the old
	dnl one deleted, and the application didn't really require the
	dnl library, then it will refuse to start due to library version
	dnl change. Reported by Andrey Chernov
	dnl

	AC_CHECK_LIB(sqlite3, sqlite3_open, [LIBS="-lsqlite3 ${HAVE_LIB_PTHREAD} $LIBS"], [], [${HAVE_LIB_PTHREAD}])
	if test "$ac_cv_lib_sqlite3_sqlite3_open" = 'no'; then
		LDFLAGS="-L/usr/local/lib ${saved_ldflags}"
		unset ac_cv_lib_sqlite3_sqlite3_open
		echo "retrying with -L/usr/local/lib ..."
		AC_CHECK_LIB(sqlite3, sqlite3_open, [LIBS="-lsqlite3 ${HAVE_LIB_PTHREAD} $LIBS"], [], [${HAVE_LIB_PTHREAD}])
		if test "$ac_cv_lib_sqlite3_sqlite3_open" = 'no'; then
			LDFLAGS=$saved_ldflags
		fi
	fi
	if test "$ac_cv_lib_sqlite3_sqlite3_open" != 'no'; then
#		AC_SUBST(HAVE_LIB_SQLITE3, '-lsqlite3')
		dnl Be sure to link with specially built static library.
		dnl This should avoid Mac OS X linking issues by explicitly
		dnl specifying the .a filename.
		AC_SUBST(HAVE_LIB_SQLITE3, "$with_sqlite3/lib/libsqlite3.a")
	fi

	AC_CHECK_HEADERS([sqlite3.h],[],[],[/* */])

	if test "$ac_cv_header_sqlite3_h" = 'no'; then
		CFLAGS="-I/usr/local/include ${saved_cflags}"
		unset ac_cv_header_sqlite3_h
		echo "retrying with -I/usr/local/include ..."
		AC_CHECK_HEADERS([sqlite3.h],[],[],[/* */])
		if test "$ac_cv_header_sqlite3_h" = 'no'; then
			CFLAGS=$saved_cflags
		fi
	fi

	AC_CHECK_FUNCS(sqlite3_open)

	LIBS=$saved_libs
fi
])


dnl
dnl SNERT_BUILD_THREADED_SQLITE3
dnl
AC_DEFUN(SNERT_BUILD_THREADED_SQLITE3,[
	echo
	AC_MSG_CHECKING([build threaded SQLite3])
	AC_SUBST(LIBSNERT_SQLITE3_DIR, ../../../../org/sqlite)
	supplied_sqlite3_tar_gz=`ls -t1 ${LIBSNERT_SQLITE3_DIR}/sqlite*.gz | wc -l`
	if test ${with_sqlite3:-default} = 'default' -a $supplied_sqlite3_tar_gz -gt 0; then
		AC_MSG_RESULT([yes])

		libsnertdir=`pwd`
		cd ${LIBSNERT_SQLITE3_DIR}
		with_sqlite3=`pwd`

		dnl Assume the most recent .tar.gz is the most current version.
		tarfile=`ls -t1 sqlite*tar.gz | head -n 1`
		dir=`basename $tarfile .tar.gz`

		is_amalgamation=false
		AS_IF([expr ${dir} : 'sqlite-amalgamation-.*'],[is_amalgamation=true; i=`echo ${dir} | sed -e 's/amalgamation-//'`; dir=${i}])

		AC_SUBST(LIBSNERT_SQLITE3_VERSION, ${dir})

		echo "sqlite directory... $with_sqlite3"
		echo "libsnert supplied version..." ${LIBSNERT_SQLITE3_DIR}/${LIBSNERT_SQLITE3_VERSION}
		AC_MSG_CHECKING([for previously built threaded SQLite3])
		if test -f "$with_sqlite3/include/sqlite3.h"; then
			AC_MSG_RESULT([yes])
		else
			AC_MSG_RESULT([no])

			AC_MSG_CHECKING([for tar file])
			AC_MSG_RESULT($tarfile)

			AC_MSG_CHECKING([if unpacked])
			if test -d $dir ; then
				AC_MSG_RESULT([yes])
			else
				tar -zxf $tarfile
				AC_MSG_RESULT([unpacked])
				make patch
			fi

			cd $dir
			AC_MSG_CHECKING([sqlite3 build directory])
			pwd
			if test ! -f config.status ; then
				echo
				echo 'Configuring threaded SQLite3...'

				sqlite3_configure_options="--prefix=$with_sqlite3 --enable-threadsafe"
				AS_IF([$is_amalgamation],
					[sqlite3_configure_options="${sqlite3_configure_options} --disable-dynamic-extensions"],
					[sqlite3_configure_options="${sqlite3_configure_options} --disable-amalgamation --disable-tcl --without-tcl"
					 AS_IF([test ${enable_debug:-no} = 'yes'],[sqlite3_configure_options="${sqlite3_configure_options} --enable-debug"])
					]
				)
				AS_IF([test ${platform} != 'Darwin'],[sqlite3_configure_options="${sqlite3_configure_options} --enable-static --disable-shared"])

				case $platform in
				FreeBSD)
					echo CFLAGS="-D_THREAD_SAFE -pthread ${CFLAGS}" LDFLAGS="-pthread ${LDFLAGS}" ./configure ${sqlite3_configure_options}
					CFLAGS="-D_THREAD_SAFE -pthread ${CFLAGS}" LDFLAGS="-pthread ${LDFLAGS}" ./configure ${sqlite3_configure_options}
					;;
				*)
					echo CFLAGS="'${CFLAGS}'" LDFLAGS="'${LDFLAGS}'" ./configure  ${sqlite3_configure_options}
					CFLAGS="${CFLAGS}" LDFLAGS="${LDFLAGS}" ./configure  ${sqlite3_configure_options}
					;;
				esac
				echo
			fi
			echo
			echo 'Installing threaded SQLite3...'
			echo
			make clean install
			cd -
		fi

		SQLITE3_I_DIR="-I$with_sqlite3/include"
		SQLITE3_L_DIR="-L$with_sqlite3/lib"
		cd $libsnertdir
	else
		AC_MSG_RESULT([no])
	fi
	echo
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
			sys/socket.h netinet/in.h netinet/in6.h netinet6/in6.h netinet/tcp.h \
			poll.h sys/poll.h sys/select.h sys/un.h arpa/inet.h \
		])

dnl When using poll() use this block.
dnl
dnl #ifdef HAVE_POLL_H
dnl # include <poll.h>
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
		AC_SUBST(HAVE_LIB_WS2_32, '-lws2_32')
		AC_SUBST(HAVE_LIB_IPHLPAPI, '-lIphlpapi')
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
dnl SNERT_SYS
dnl
AC_DEFUN(SNERT_SYS,[
	echo
	echo "Check for system kernel support..."
	echo
	dnl Linux
	AC_CHECK_HEADERS([sys/prctl.h],[
		AC_CHECK_FUNCS(prctl)
	])
	AC_CHECK_HEADERS([sys/sysinfo.h],[
		AC_CHECK_FUNCS(get_nprocs_conf get_nprocs)
	])
	dnl *BSD
	AC_CHECK_HEADERS([sys/param.h sys/sysctl.h],[
		AC_CHECK_FUNCS(sysctl)
	])
	AC_CHECK_HEADERS([stdlib.h],[
		AC_CHECK_FUNCS(getloadavg)
	])
	dnl POSIX / generic
	AC_CHECK_HEADERS([unistd.h],[
		AC_CHECK_FUNCS(sysconf)
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
	AC_SUBST(package_major, [[`echo $PACKAGE_VERSION | sed -e 's/^\([0-9]*\)\.\([0-9]*\)/\1/'`]])
	AC_SUBST(package_minor, [[`echo $PACKAGE_VERSION | sed -e 's/^\([0-9]*\)\.\([0-9]*\)/\2/'`]])

	if test -f "$3" ; then
		AC_SUBST(package_build, "`cat $3`")
	elif test -f BUILD_ID.TXT ; then
		AC_SUBST(package_build, "`cat BUILD_ID.TXT | tr -d '[\r\n]'`")
	fi

	AC_SUBST(package_version, "${PACKAGE_VERSION}${package_build:+.}${package_build}")
	AC_SUBST(package_string, "${PACKAGE_NAME} ${package_version}")

	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_NAME, ["$PACKAGE_NAME"])
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_MAJOR, $package_major)
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_MINOR, $package_minor)
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_BUILD, $package_build)
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_VERSION, ["$package_version"])
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_STRING, ["$package_string"])

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
	echo $PACKAGE_NAME/$package_major.$package_minor${package_build:+.}${package_build}
	echo $package_copyright
	echo
])

AC_DEFUN(SNERT_FINI,[
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_CFLAGS, ["$CFLAGS"])
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_LDFLAGS, ["$LDFLAGS"])
	AC_DEFINE_UNQUOTED(${snert_macro_prefix}_LIBS, ["$LIBS"])

])

dnl
dnl SNERT_SUMMARY
dnl
AC_DEFUN(SNERT_SUMMARY,[
	echo
	echo $PACKAGE_NAME/$package_major.$package_minor.$package_build
	echo $package_copyright
	echo
	AC_MSG_RESULT([  Platform.......: $platform $CC])
	AC_MSG_RESULT([  CFLAGS.........: $CFLAGS])
	AC_MSG_RESULT([  LDFLAGS........: $LDFLAGS])
	AC_MSG_RESULT([  LIBS...........: $LIBS])
])

dnl if ! grep -q "^milter:" /etc/group; then
dnl 	groupadd milter
dnl fi
dnl
dnl if ! grep -q "^milter:" /etc/passwd; then
dnl 	useradd -c 'milter processes' -g milter milter
dnl fi
dnl
dnl if grep -q "^smmsp:" /etc/group; then
dnl 	usermod -G smmsp milter
dnl fi
