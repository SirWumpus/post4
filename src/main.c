/*
 * post4.c
 *
 * Copyright 2007, 2024 by Anthony Howe. All rights reserved.
 */

#include "post4.h"

/***********************************************************************
 *** Main
 ***********************************************************************/

static const char usage[] =
"usage: post4 [-TV][-b file][-c file][-d size][-f size][-i file][-m size]\r\n"
"             [-r size][script [args ...]]\r\n"
"\r\n"
"-b file\t\topen a block file\r\n"
"-c file\t\tword definition file; default " P4_CORE_FILE " from $POST4_PATH\r\n"
"-d size\t\tdata stack size in cells; default " QUOTE(P4_DATA_STACK_SIZE) "\r\n"
"-f size\t\tfloat stack size; default " QUOTE(P4_FLOAT_STACK_SIZE) "\r\n"
"-i file\t\tinclude file; can be repeated; searches $POST4_PATH\r\n"
"-m size\t\tdata space memory in KB; default " QUOTE(P4_MEM_SIZE) "\r\n"
"-r size\t\treturn stack size in cells; default " QUOTE(P4_RETURN_STACK_SIZE) "\r\n"
"-T\t\tenable tracing; see TRACE\r\n"
"-V\t\tbuild and version information\r\n\r\n"
"If script is \"-\", read it from standard input.\r\n"
;

static char *flags = "b:c:d:f:i:m:r:TV";

static P4_Ctx *ctx_main;

static P4_Options options = {
	.ds_size = P4_DATA_STACK_SIZE,
	.rs_size = P4_RETURN_STACK_SIZE,
	.fs_size = P4_FLOAT_STACK_SIZE,
	.mem_size = P4_MEM_SIZE,
	.core_file = P4_CORE_FILE,
	.block_file = NULL,
};

static const char p4_build_info[] =
	P4_NAME "/" P4_VERSION "  " P4_COPYRIGHT "\r\n\r\n"
	"BUILT=\"" P4_BUILT "\"\r\n"
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
		switch (ch) {
		case 'b':
			options.block_file = optarg;
			break;
		case 'c':
			options.core_file = optarg;
			break;
		case 'd':
			options.ds_size = strtol(optarg, NULL, 10);
			break;
		case 'f':
#ifdef HAVE_MATH_H
			options.fs_size = strtol(optarg, NULL, 10);
#else
			(void) warnx("float support disabled");
#endif
			break;
		case 'i':
			// Ignore for now.
			break;
		case 'm':
			options.mem_size = strtol(optarg, NULL, 10);
			break;
		case 'r':
			options.rs_size = strtol(optarg, NULL, 10);
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
			return EXIT_SUCCESS;
		default:
			(void)fprintf(stderr, usage);
			return 2;
		}
	}

	options.argc = argc - optind;
	options.argv = argv + optind;

	p4Init();
	if ((rc = SETJMP(sig_break_glass)) != 0) {
		THROW_MSG(rc);
		(void) fprintf(STDERR, "\r\n");
		return EXIT_FAILURE;
	}
	sig_init();
	if ((ctx_main = p4Create(&options)) == NULL) {
		return EXIT_FAILURE;
	}
	(void) atexit(cleanup);

	optind = 1;
	while ((ch = getopt(argc, argv, flags)) != -1) {
		if (ch == 'i' && (rc = p4EvalFile(ctx_main, optarg)) != P4_THROW_OK) {
			/* If an exception, other than ABORT or QUIT, occurs
			 * they will generate an execption message.  Do not
			 * really need to repeat it here, though including
			 * the file name would help debugging.
			 */
//			(void) fprintf(STDERR, "post4: include %s: %d thrown\r\n", optarg, rc);
			return EXIT_FAILURE;
		}
	}

	if (argc <= optind || (argv[optind][0] == '-' && argv[optind][1] == '\0')) {
		rc = SETJMP(sig_break_glass);
		p4ResetInput(ctx_main, stdin);
		rc = p4Repl(ctx_main, rc);
	} else if (optind < argc) {
		rc = p4EvalFile(ctx_main, argv[optind]);
	}

	return (rc != 0) * 3;
}
