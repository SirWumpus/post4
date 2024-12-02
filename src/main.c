/*
 * post4.c
 *
 * Copyright 2007, 2024 by Anthony Howe. All rights reserved.
 */

#include "post4.h"
#include "aline.h"

/***********************************************************************
 *** Main
 ***********************************************************************/

static const char usage[] =
"usage: post4 [-TV][-b file][-c file][-h size][-i file][-m size]\r\n"
"             [script [args ...]]\r\n"
"\r\n"
"-b file\t\topen a block file\r\n"
"-c file\t\tword definition file; default " P4_CORE_FILE " from $POST4_PATH\r\n"
"-h size\t\thistory size in lines; default " QUOTE(ALINE_HISTORY) "\r\n"
"-i file\t\tinclude file; can be repeated; searches $POST4_PATH\r\n"
"-m size\t\tdata space memory in KB; default " QUOTE(P4_MEM_SIZE) "\r\n"
"-T\t\tenable tracing; see TRACE\r\n"
"-V\t\tbuild and version information\r\n\r\n"
"If script is \"-\", read it from standard input.\r\n"
;

static char *flags = "b:c:d:f:h:i:m:r:TV";

static P4_Ctx *ctx_main;

static P4_Options options = {
	.mem_size = P4_MEM_SIZE,
	.hist_size = ALINE_HISTORY,
	.core_file = P4_CORE_FILE,
	.block_file = NULL,
};

static const char p4_build_info[] =
	P4_NAME "/" P4_VERSION "  " P4_COPYRIGHT "\r\n\r\n"
	"BUILT=\"" P4_BUILT "\"\r\n"
	"COMMIT=\"" P4_COMMIT "\"\r\n"
	"CFLAGS=\"" P4_CFLAGS "\"\r\n"
	"LDFLAGS=\"" P4_LDFLAGS "\"\r\n"
	"LIBS=\"" P4_LIBS "\"\r\n"
	"POST4_PATH=\"" P4_CORE_PATH "\"\r\n"
;

static void
cleanup(void)
{
	/* Memory clean-up on exit is redundant since it all goes back
	 * to OS anyway when the process is reaped, but it helps close
	 * the loop on memory allocations for Valgrind.
	 */
	p4Free(ctx_main);
	/* This is redundant too, but I like it for symmetry. */
	sig_fini();
}

int
main(int argc, char **argv)
{
	int ch, rc;

	while ((ch = getopt(argc, argv, flags)) != -1) {
		unsigned val = 0;
		if (optarg != NULL) {
			val = (unsigned) strtol(optarg, NULL, 10);
		}
		switch (ch) {
		case 'b':
			options.block_file = optarg;
			break;
		case 'c':
			options.core_file = optarg;
			break;
		case 'i':
			// Ignore for now.
			break;
		case 'h':
			options.hist_size = val;
			break;
		case 'm':
			options.mem_size = val;
			break;
		case 'T':
			options.trace++;
			break;
		case 'V':
			(void) printf(
				"%s\r\nsizeof char=%zu short=%zu int=%zu long=%zu size_t=%zu "
				"intptr_t=%zu float=%zu double=%zu\r\nvoid *=%zu long long=%zu "
				"long double=%zu JMP_BUF=%zu\r\n",
				p4_build_info,
				sizeof (char), sizeof (short), sizeof (int), sizeof (long),
				sizeof (size_t), sizeof (intptr_t), sizeof (float), sizeof (double),
				sizeof (void *), sizeof (long long), sizeof (long double),
				sizeof (JMP_BUF)
			);
			return P4_EXIT_OK;
		default:
			(void)fprintf(stderr, usage);
			return P4_EXIT_USAGE;
		}
	}

	options.argc = argc - optind;
	options.argv = argv + optind;

	sig_init();
	p4Init(&options);
	(void) atexit(cleanup);
	if ((rc = SETJMP(sig_break_glass)) != 0) {
		THROW_MSG(rc);
		(void) fprintf(stderr, "\r\n");
		return P4_EXIT_STATUS(rc);
	}
	if ((ctx_main = p4Create(&options)) == NULL) {
		return P4_EXIT_FAIL;
	}

	for (optind = 1; (ch = getopt(argc, argv, flags)) != -1; ) {
		if (ch == 'i' && (rc = p4EvalFile(ctx_main, optarg)) != P4_THROW_OK) {
			/* If an exception, other than ABORT or QUIT, occurs
			 * they will generate an exception message.  Do not
			 * really need to repeat it here, though including
			 * the file name would help debugging.
			 */
			return P4_EXIT_STATUS(rc);
		}
	}

	if (argc <= optind || (argv[optind][0] == '-' && argv[optind][1] == '\0')) {
		rc = SETJMP(sig_break_glass);
		rc = p4Repl(ctx_main, rc);
	} else if (optind < argc) {
		rc = p4EvalFile(ctx_main, argv[optind]);
	}

	return P4_EXIT_STATUS(rc);
}
