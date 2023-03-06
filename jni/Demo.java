/*
 * Demo.java
 *
 * Copyright 2023 by Anthony Howe.  All rights reserved.
 */

package post4.jni;

public final class Demo
{
	private static void evalString(Post4 p4)
	{
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
	}

	private static void repl(Post4 p4)
	{
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

	public static void main(String[] args)
	{
		Post4 p4 = new Post4(args);
		evalString(p4);
		repl(p4);
	}
}
