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
		p4Init();
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
				p4.evalString(
"""
$cafebabe hex U. decimal CR
41 377 3.14159 2.71828 6.62607015e-34
"""
				);
				Post4Stacks results = p4.stacks();

				System.out.print(String.format("ds[%d] ", results.ds.length));
				for (long l : results.ds) {
					System.out.print(String.format("%d ", l));
				}
				System.out.println();

				System.out.print(String.format("fs[%d] ", results.fs.length));
				for (double d : results.fs) {
					System.out.print(String.format("%.6e ", d));
				}
				System.out.println();
			} catch (Post4Exception e) {
				// You goofed.
				System.err.println(e);
			}

			try {
				int rc;
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

	private native int repl();
	private native Post4Stacks stacks();
	private native void evalFile(String fpath) throws Post4Exception;
	private native void evalString(String string) throws Post4Exception;
}
