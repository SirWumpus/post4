/*
 * Post4.java
 *
 * Copyright 2023 by Anthony Howe.  All rights reserved.
 */

package post4.jni;

public final class Post4
{
	private final long ctx;

	static
	{
		System.loadLibrary("post4jni");
		init();
	}

	public Post4()
	{
		this(new Post4Options());
	}

	public Post4(Post4Options opts)
	{
		ctx = p4Create(opts);
	}

	/*
	 * https://stackoverflow.com/questions/44095247/should-java-finalizer-really-be-avoided-also-for-native-peer-objects-lifecycle-m
	 * https://www.hboehm.info/misc_slides/java_finalizers.pdf
	 *
	 * @note
	 *	finalize() has been deprecated; needs to be replaced.
	 */
	protected void finalize()
	{
		synchronized (this) {
			p4Free(ctx);
		}
	}

	public static void main(String[] args)
	{
			Post4Options opts = new Post4Options();
			opts.argv = args;
			Post4 p4 = new Post4(opts);
			try {
				p4.evalString("$cafebabe .s hex . CR blocks");
			} catch (Post4Exception e) {
				System.err.println(e);
			}
//			try {
				p4.eval();
//			} catch (Post4Exception e) {
//				System.err.println(e);
//			}
	}

	private native static void init();
	private native static void p4Free(long ctx);
	private native static long p4Create(Post4Options opts);

	private native int repl();
	private native void eval(); // throws Post4Exception;
	private native void evalFile(String fpath) throws Post4Exception;
	private native void evalString(String string) throws Post4Exception;
}
