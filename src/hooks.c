/*
 * hooks.c
 *
 * Copyright 2023 by Anthony Howe. All rights reserved.
 */

#include "post4.h"

#ifdef HAVE_HOOKS

# ifdef USE_HOOK_SHELL
/*
 * SH ( `input` -- )
 */
static void
p4System(P4_Ctx *ctx)
{
	// Assumes input buffer is writeable.
	ctx->input.buffer[ctx->input.length < ctx->input.size ? ctx->input.length : ctx->input.size-1] = '\0';
	(void) system(ctx->input.buffer + ctx->input.offset);
	ctx->input.offset = ctx->input.length;
}

/*
 * SHELL ( caddr u -- n )
 */
static void
p4SystemString(P4_Ctx *ctx)
{
	P4_POP(ctx->ds);
	char *s = P4_TOP(ctx->ds).s;
	// Assumes caddr NUL terminated.
	P4_TOP(ctx->ds).n = system(s);
}
# endif

# ifdef USE_HOOK_PRIMATIVES
/* Examples of how some words, in particular those calling libc
 * or other library functions can be isolated as hooks.
 */

/*
 * MOVE ( src dst u -- )
 */
static void
p4Memmove(P4_Ctx *ctx)
{
	P4_Cell len = P4_POP(ctx->ds);
	P4_Cell dst = P4_POP(ctx->ds);
	/* Using strncpy would allow for propagation, like CMOVE:
	 *	char src[] = "A    ";
	 *	strncpy(src+1, src, 4);
	 *	strcmp(src, "AAAAA") == 0;
	 * Not necessarily as efficent, plus the C standard says
	 * that the behaviour of strncpy with overlapping strings
	 * is undefined (which seems wrong when tested).
	 */
	(void) memmove(dst.s, P4_POP(ctx->ds).s, len.z);
}

/*
 * TIME&DATE ( -- sec min hour day month year )
 */
static void
p4TimeDate(P4_Ctx *ctx)
{
	time_t tick;
	struct tm *now;
	(void) time(&tick);
	now = localtime(&tick);
	p4StackCanPopPush(ctx, &ctx->ds, 0, 6);
	P4_PUSH(ctx->ds, (P4_Int) now->tm_sec);
	P4_PUSH(ctx->ds, (P4_Int) now->tm_min);
	P4_PUSH(ctx->ds, (P4_Int) now->tm_hour);
	P4_PUSH(ctx->ds, (P4_Int) now->tm_mday);
	P4_PUSH(ctx->ds, (P4_Int) now->tm_mon+1);
	P4_PUSH(ctx->ds, (P4_Int) now->tm_year+1900);
}
# endif

static P4_Hook p4_hooks[] = {
# ifdef USE_HOOK_SHELL
	{ "SH", p4System },
	{ "SHELL", p4SystemString },
# endif
# ifdef USE_HOOK_PRIMATIVES
	{ "MOVE", p4Move },
	{ "TIME&DATE", p4STimeDate },
# endif
	{ NULL, NULL }
};

int
p4HookAdd(P4_Ctx *ctx, const char *name, void (*func)(P4_Ctx *))
{
	size_t len;
	char buf[P4_INPUT_SIZE];

	len = snprintf(buf, sizeof (buf), "$%lx _hook_add %s", func, name);
	if (len < sizeof (buf)) {
		return p4EvalString(ctx, buf, len);
	}
	return P4_THROW_NAME_TOO_LONG;
}

int
p4HookInit(P4_Ctx *ctx)
{
	int rc;
	P4_Hook *h;

	for (h = p4_hooks; h->name != NULL; h++) {
		if ((rc = p4HookAdd(ctx, h->name, h->func)) != P4_THROW_OK) {
			errx(EXIT_FAILURE, "hook %s fail %d", h->name, rc);
		}
	}

	return 0;
}

#endif /* HAVE_HOOKS */