[UNDEFINED] cell-bits [IF]
	.( Assumes Post4 ) CR
[ELSE] cell-bits 64 <> [IF]
	.( Test suite assumes 64b cell size. ) CR
[ELSE]
	.( Post4 Unit Tests ) CR
	INCLUDE ../test/assert.p4

	test_suite
	INCLUDE ../test/core.p4
	INCLUDE ../test/2star.p4
	INCLUDE ../test/d0equal.p4
	INCLUDE ../test/umstar.p4
	INCLUDE ../test/memory.p4
	INCLUDE ../test/exceptions.p4
	test_suite_end

	rm_assert
[THEN] [THEN]
