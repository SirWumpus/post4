/*
 * hooks.c
 *
 * Copyright 2023, 2004 by Anthony Howe. All rights reserved.
 */

#include "post4.h"

#ifdef HAVE_HOOKS

# ifdef HOOK_SHELL
/*
 * SH ( `remaining input line` -- )
 */
static void
p4System(P4_Ctx *ctx)
{
	P4_Input *input = ctx->input;
	// Assumes input buffer is writeable.
	input->buffer[input->length] = '\0';
	(void) system(input->buffer + input->offset);
	input->offset = input->length;
}

/*
 * SHELL ( caddr u -- n )
 */
static void
p4SystemString(P4_Ctx *ctx)
{
	P4_DROP(ctx->ds, 1);	/* Ignore u */
	char *s = P4_TOP(ctx->ds).s;
	// Assumes caddr NUL terminated.
	P4_TOP(ctx->ds).n = system(s);
}
# endif

# ifdef HOOK_PRIMATIVES
/* Examples of how some words, in particular those calling libc
 * or other library functions can be isolated as hooks.
 */

/*
 * MOVE ( src dst u -- )
 */
static void
p4Move(P4_Ctx *ctx)
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
	p4AllocStack(ctx, &ctx->P4_FLOAT_STACK, 6);
	P4_PUSH(ctx->ds, (P4_Int) now->tm_sec);
	P4_PUSH(ctx->ds, (P4_Int) now->tm_min);
	P4_PUSH(ctx->ds, (P4_Int) now->tm_hour);
	P4_PUSH(ctx->ds, (P4_Int) now->tm_mday);
	P4_PUSH(ctx->ds, (P4_Int) now->tm_mon+1);
	P4_PUSH(ctx->ds, (P4_Int) now->tm_year+1900);
}
# endif

P4_Hook p4_hooks[] = {
# ifdef HOOK_SHELL
	P4_HOOK("SH", p4System),
	P4_HOOK("SHELL", p4SystemString),
# endif
# ifdef HOOK_PRIMATIVES
	P4_HOOK("MOVE", p4Move),
	P4_HOOK("TIME&DATE", p4STimeDate),
# endif
	{ 0, NULL, NULL }
};

P4_Word *
p4HookAdd(P4_Ctx *ctx, P4_Hook *hook)
{
	P4_Word *word;
	if ((word = p4WordCreate(ctx, hook->name, hook->length, p4_hook_call->code)) != NULL) {
		p4WordAppend(ctx, (P4_Cell)(void *)hook->func);
	}
	return word;
}

void
p4HookInit(P4_Ctx *ctx, P4_Hook *hooks)
{
	for (P4_Hook *h = hooks; h->name != NULL; h++) {
		(void) p4HookAdd(ctx, h);
	}
}

#endif /* HAVE_HOOKS */
