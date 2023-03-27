/*
 * Post4.java
 *
 * Copyright 2023 by Anthony Howe.  All rights reserved.
 */

package post4.jni;

public class Post4
{
	private final long ctx;

	static
	{
		System.loadLibrary("post4jni");
		p4Init();
	}

	public Post4()
	{
		this(new Post4Options());
	}

	public Post4(String[] args)
	{
		Post4Options opts = new Post4Options();
		opts.argc = args.length;
		opts.argv = args;
		ctx = p4Create(opts);
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
		int rc;
		Post4 p4 = new Post4(args);

		if (0 < args.length) {
			try {
				p4.evalFile(args[0]);
				System.exit(0);
			} catch (Exception e) {
				// Something else happened on this day, lost in time.
				System.err.println(e);
				System.exit(1);
			}
		}

		try {
			while ((rc = p4.repl()) != Post4Exception.THROW_OK) {
				; // Remain in the REPL until EOF or BYE.
			}
		} catch (Exception e) {
			// Something else happened on this day, lost in time.
			System.err.println(e);
		}
	}

	private native static void p4Init();
	private native static void p4Free(long ctx);
	private native static long p4Create(Post4Options opts);

	public native int repl();
	public native Post4Stacks stacks();
	public native void evalFile(String fpath) throws Post4Exception;
	public native void evalString(String string) throws Post4Exception;
}
