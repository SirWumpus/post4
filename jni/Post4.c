/*
 * Post4.c
 *
 * Copyright 2023 by Anthony Howe.  All rights reserved.
 */

#include <jni.h>
#include <stdlib.h>

#include "../src/post4.h"

#define POST4_CLASS	"post4/jni/Post4"
#define ERROR_CLASS	"post4/jni/Post4Exception"

static const char *empty_argv[] = { NULL };

static P4_Ctx *
getCtx(JNIEnv *env, jobject self)
{
	jclass post4 = (*env)->GetObjectClass(env, self);
	jfieldID fid = (*env)->GetFieldID(env, post4, "ctx", "J");
	jlong ctx = (*env)->GetLongField(env, self, fid);
	(*env)->DeleteLocalRef(env, post4);
	return (P4_Ctx *) ctx;
}

static jobject
post4Exception(JNIEnv *env, int code)
{
	jclass oops = (*env)->FindClass(env, ERROR_CLASS);
	jmethodID mid = (*env)->GetMethodID(env, oops, "<init>", "(I)V");
	jobject p4err = (*env)->NewObject(env, oops, mid, code);
	(*env)->DeleteLocalRef(env, oops);
	return p4err;
}

JNIEXPORT void JNICALL
Java_post4_jni_Post4_init(JNIEnv *env, jobject self)
{
	p4Init();
}

JNIEXPORT void JNICALL
Java_post4_jni_Post4_p4Free(JNIEnv *env, jobject self, jlong xtc)
{
	P4_Ctx *ctx = (P4_Ctx *) xtc;
	if (ctx->argv != (char **) empty_argv) {
		for (int argi = 0; argi < ctx->argc; argi++) {
			free(ctx->argv[argi]);
		}
		free(ctx->argv);
	}
	p4Free(ctx);
}

JNIEXPORT jlong JNICALL
Java_post4_jni_Post4_p4Create(JNIEnv *env, jobject self, jobject opts)
{
	P4_Ctx *ctx;
	jfieldID fid;
	P4_Options p4_opts;

	/* Map from object to struct. */
	jclass clazz = (*env)->GetObjectClass(env, opts);

	fid = (*env)->GetFieldID(env, clazz, "ds_size", "I");
	p4_opts.ds_size = (unsigned)(*env)->GetIntField(env, opts, fid);
	fid = (*env)->GetFieldID(env, clazz, "fs_size", "I");
	p4_opts.fs_size = (unsigned)(*env)->GetIntField(env, opts, fid);
	fid = (*env)->GetFieldID(env, clazz, "rs_size", "I");
	p4_opts.rs_size = (unsigned)(*env)->GetIntField(env, opts, fid);
	fid = (*env)->GetFieldID(env, clazz, "mem_size", "I");
	p4_opts.mem_size = (unsigned)(*env)->GetIntField(env, opts, fid);

	fid = (*env)->GetFieldID(env, clazz, "core_file", "Ljava/lang/String;");
	jstring jcore_file = (*env)->GetObjectField(env, opts, fid);
	p4_opts.core_file = (*env)->GetStringUTFChars(env, jcore_file, NULL);
	if (p4_opts.core_file == NULL) {
		p4_opts.core_file = P4_CORE_FILE;
	}

	fid = (*env)->GetFieldID(env, clazz, "block_file", "Ljava/lang/String;");
	jstring jblock_file = (*env)->GetObjectField(env, opts, fid);
	p4_opts.block_file = (*env)->GetStringUTFChars(env, jblock_file, NULL);

	fid = (*env)->GetFieldID(env, clazz, "argv", "[Ljava/lang/String;");
	jobjectArray jargv = (*env)->GetObjectField(env, opts, fid);
	p4_opts.argc = (int) (*env)->GetArrayLength(env, jargv);
	if ((p4_opts.argv = malloc((p4_opts.argc + 1) * sizeof (*p4_opts.argv))) == NULL) {
		p4_opts.argv = (char **) empty_argv;
		p4_opts.argc = 0;
	} else {
		p4_opts.argv[p4_opts.argc] = NULL;
		for (int argi = 0; argi < p4_opts.argc; argi++) {
			jstring jstr = (*env)->GetObjectArrayElement(env, jargv, argi);
			const char *str = (*env)->GetStringUTFChars(env, jstr, NULL);
			p4_opts.argv[argi] = strdup(str);
			(*env)->ReleaseStringUTFChars(env, jstr, str);
			(*env)->DeleteLocalRef(env, jstr);
		}
	}
	(*env)->DeleteLocalRef(env, jargv);

	/* Create Post4 context. */
	ctx = p4Create(&p4_opts);

	(*env)->ReleaseStringUTFChars(env, jcore_file, p4_opts.core_file);
	(*env)->ReleaseStringUTFChars(env, jblock_file, p4_opts.block_file);
	(*env)->DeleteLocalRef(env, clazz);

	if (ctx == NULL) {
		(*env)->Throw(env, (*env)->FindClass(env, "java/lang/OutOfMemory"));
	}

	// https://stackoverflow.com/questions/1632367/passing-pointers-between-c-and-java-through-jni
	return (jlong) ctx;
}

JNIEXPORT jint JNICALL
Java_post4_jni_Post4_repl(JNIEnv *env, jobject self)
{
	return p4Repl(getCtx(env, self));
}

JNIEXPORT void JNICALL
Java_post4_jni_Post4_eval(JNIEnv *env, jobject self)
{
	int rc = p4Eval(getCtx(env, self));
	if (rc != P4_THROW_OK) {
		(*env)->Throw(env, post4Exception(env, rc));
	}
}

JNIEXPORT void JNICALL
Java_post4_jni_Post4_evalFile(JNIEnv *env, jobject self, jstring fpath)
{
	const char *path = (*env)->GetStringUTFChars(env, fpath, NULL);
	int rc = p4EvalFile(getCtx(env, self), path);
	(*env)->ReleaseStringUTFChars(env, fpath, path);
	if (rc != P4_THROW_OK) {
		(*env)->Throw(env, post4Exception(env, rc));
	}
}

JNIEXPORT void JNICALL
Java_post4_jni_Post4_evalString(JNIEnv *env, jobject self, jstring string)
{
	size_t len = (*env)->GetStringLength(env, string);
	const char *str = (*env)->GetStringUTFChars(env, string, NULL);
	int rc = p4EvalString(getCtx(env, self), str, len);
	(*env)->ReleaseStringUTFChars(env, string, str);
	if (rc != P4_THROW_OK) {
		(*env)->Throw(env, post4Exception(env, rc));
	}
}

