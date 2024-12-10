/*
 * hooks.c
 *
 * Copyright 2023, 2004 by Anthony Howe. All rights reserved.
 */

#include "post4.h"

#ifdef HAVE_HOOKS
/*
 * SHELL ( caddr u -- n )
 */
static void
p4Shell(P4_Ctx *ctx)
{
	size_t len = P4_POP(ctx->ds).z;
	char *cmd = strndup(P4_TOP(ctx->ds).s, len);
	P4_TOP(ctx->ds).n = system(cmd);
	free(cmd);
}

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
# endif

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

/*
 * getcwd ( -- caddr u )
 * Be sure to FREE caddr.
 */
static void
p4GetCwd(P4_Ctx *ctx)
{
	char *cwd = getcwd(NULL, 0);
	P4_PUSH(ctx->ds, cwd);
	P4_PUSH(ctx->ds, (P4_Size)(cwd == NULL ? 0 : strlen(cwd)));
}

/*
 * getenv ( key k -- value v )
 */
static void
p4GetEnv(P4_Ctx *ctx)
{
	size_t k = P4_POP(ctx->ds).z;
	char *key = strndup(P4_TOP(ctx->ds).s, k);
	char *value = getenv(key);
	free(key);
	P4_TOP(ctx->ds).s = value;
	P4_PUSH(ctx->ds, (P4_Int)(value == NULL ? 0 : strlen(value)));
}

/*
 * system-path ( -- sd.post4 )
 */
static void
p4SystemPath(P4_Ctx *ctx)
{
	P4_String str;
	char path[PATH_MAX];
	/* Supported by NetBSD, FreeBSD, and Linux.  Assumes procfs mounted. */
	str.length = readlink("/proc/self/exe", path, sizeof (path));
	str.string = strdup(path);
	P4_PUSH(ctx->ds, str.string);
	P4_PUSH(ctx->ds, str.length);
}

P4_Hook p4_hooks[] = {
# ifdef HOOK_PRIMATIVES
	P4_HOOK(0x30, "MOVE", p4Move),
# endif
	P4_HOOK(0x21, "shell", p4Shell),
	P4_HOOK(0x02, "getcwd", p4GetCwd),
	P4_HOOK(0x22, "getenv", p4GetEnv),
	P4_HOOK(0x02, "system-path", p4SystemPath),
	P4_HOOK(0x06, "TIME&DATE", p4TimeDate),
	{ 0, 0, NULL, NULL }
};

P4_Word *
p4HookAdd(P4_Ctx *ctx, P4_Hook *hook)
{
	P4_Word *word;
	if ((word = p4WordCreate(ctx, hook->name, hook->length, p4_hook_call->code)) != NULL) {
		p4WordAppend(ctx, (P4_Cell)(void *)hook->func);
		word->poppush = hook->poppush;
	}
	return word;
}

void
p4HookInit(P4_Ctx *ctx, P4_Hook *hooks)
{
	/*** TODO Consider placing hooks in a separate `HOOK-WORDLIST`. */
	for (P4_Hook *h = hooks; h->name != NULL; h++) {
		(void) p4HookAdd(ctx, h);
	}
}

#endif /* HAVE_HOOKS */
