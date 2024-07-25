/*
 * Post4Stacks.java
 *
 * Copyright 2023, 2024 by Anthony Howe.  All rights reserved.
 */

package post4.jni;

public class Post4Stacks
{
	public final long[] ds;
	public final double[] fs;

	protected Post4Stacks()
	{
		this(new long[]{}, new double[]{});
	}

	protected Post4Stacks(long[] ds, double[] fs)
	{
		this.ds = ds;
		this.fs = fs;
	}
}
