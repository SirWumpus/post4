Post4 (Post-Forth)
==================

Copyright 2007, 2024 Anthony Howe.  All rights reserved.


### Test Case Words

To use this extension word set:

        include-path post4/assert.p4

A test case typically takes the form of

        T{ <perform test> -> <expected stack results> }T

For example:

        T{ 1 2.71828 3 -> 1 2.71828 3 }T
        T{ 1 2 + -> 3 }T

- - -
#### T{
( -- )  
Start a test case.

- - -
#### ->
( -- )  
Separate the test case results from the expected data and float stack.

- - -
#### }T
( -- )  
Compare the test results on the stacks against the expected data and float stacks, counting a pass, fail, or skipped result if the stacks fail to match.

- - -

### Post4 Specific Words

An example of a test group:

        .( INVERT ) test_group
        t{ FALSE INVERT -> TRUE }t
        t{ TRUE INVERT -> FALSE }t
        t{ 0 INVERT -> -1 }t
        test_group_end

See the Post4 unit test suite (`test/units.p4`) based on the Forth Standard section F for examples.

- - -
#### assert
( `bool` -- )  
If `bool` is `TRUE` then count a pass result, otherwise count a failed result.

- - -
#### assert_not
( `bool` -- )  
If `bool` is `FALSE` then count a pass result, otherwise count a failed result.

- - -
#### assert_skip
( `bool` -- )  
If `bool` is `TRUE` then count a pass result, otherwise count a skipped result.

- - -
#### assert_not_skip
( `bool` -- )  
If `bool` is `FALSE` then count a pass result, otherwise count a skipped result.

- - -
#### test_group
( -- )  
Start a test group and set a marker `rm_test_group`.

- - -
#### test_group_end
( -- )  
End a test group and execute `rm_test_group` to clean-up the environment of test data and words.

- - -
#### test_suite
( -- )  
Start a test suite, which is a collection of test groups and test cases.  The stacks and counts are also cleared.

- - -
#### test_suite_end
( -- )  
End a test suite and report the results, stack size, passed, failed, and skipped.  If there were any failures, then Post4 exits with status code `1`; skipped results do not terminate Post4.

- - -
#### ts{
( -- )  
Start a test case and if the result is a failure, count it as skipped instead.  Test-skip is useful while developing a test case or if a test case is expected fail, but the test suite should continue.

- - -
