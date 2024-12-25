INCLUDE-PATH post4/assert.p4

.( strlen env ) test_group
\ Environment variables are NUL terminated.
T{ S" HOME" getenv SWAP strlen = -> TRUE }T
T{ S" USER" getenv SWAP strlen = -> TRUE }T
\ S" extension NUL terminates its string.
T{ S" Hello world!" SWAP strlen = -> TRUE }T
T{ S" " SWAP strlen = -> TRUE }T
test_group_end

.( strcmp ) test_group
: tw_0 S" " ;
: tw_a S" A" ;
: tw_b S" B" ;
: tw_z S" Z" ;
: tw_ab S" AB" ;
: tw_az S" AZ" ;
: tw_abc S" ABC" ;
T{ tw_0 tw_0 strcmp -> 0 }T
T{ tw_0 tw_a strcmp -> -1 }T
T{ tw_a tw_0 strcmp ->  1 }T
T{ tw_0 tw_z strcmp -> -1 }T
T{ tw_z tw_0 strcmp ->  1 }T
T{ tw_a tw_a strcmp -> 0 }T
T{ tw_b tw_b strcmp -> 0 }T
T{ tw_ab tw_ab strcmp -> 0 }T
T{ tw_abc tw_abc strcmp -> 0 }T
T{ tw_a tw_b strcmp -> -1 }T
T{ tw_b tw_a strcmp ->  1 }T
T{ tw_a tw_z strcmp -> -1 }T
T{ tw_z tw_a strcmp ->  1 }T
T{ tw_a tw_ab strcmp -> -1 }T
T{ tw_ab tw_a strcmp ->  1 }T
T{ tw_a tw_abc strcmp -> -1 }T
T{ tw_abc tw_a strcmp ->  1 }T
T{ tw_ab tw_abc strcmp -> -1 }T
T{ tw_abc tw_ab strcmp ->  1 }T
T{ tw_z tw_ab strcmp ->   1 }T
T{ tw_ab tw_z strcmp ->  -1 }T
T{ tw_az tw_ab strcmp ->  1 }T
T{ tw_ab tw_az strcmp -> -1 }T
T{ tw_z tw_abc strcmp ->  1 }T
T{ tw_abc tw_z strcmp -> -1 }T
T{ tw_az tw_abc strcmp ->  1 }T
T{ tw_abc tw_az strcmp -> -1 }T
test_group_end

.( strrev ) test_group
: tw_0 S" " ;
: tw_a S" A" ;
: tw_ab S" AB" ;
: tw_abc S" ABC" ;
: tw_ba S" BA" ;
: tw_cba S" CBA" ;
T{ tw_0 strrev tw_0 tw_0 strcmp -> 0 }T
T{ tw_a strrev tw_a tw_a strcmp -> 0 }T
T{ tw_ab strrev tw_ab tw_ba strcmp -> 0 }T
T{ tw_abc strrev tw_abc tw_cba strcmp -> 0 }T
T{ tw_ab strrev tw_ab tw_ba strcmp -> -1 }T	\ revert tw_ab
T{ tw_abc strrev tw_abc tw_cba strcmp -> -1 }T	\ revert tw_abc
test_group_end

.( COMPARE ) test_group
: tw_str_0 S" abcdefghijklmnopqrstuvwxyz" ;
: tw_str_1 S" 12345" ;
: tw_str_2 S" 0abc" ;
: tw_str_3 S" 0aBc" ;
T{ tw_str_0 tw_str_0 COMPARE -> 0 }T
T{ tw_str_0 PAD SWAP CMOVE -> }T \ Copy tw_str_0 to PAD
T{ tw_str_0 PAD OVER COMPARE -> 0 }T
T{ tw_str_0 PAD 6 COMPARE -> 1 }T
T{ PAD 10 tw_str_0 COMPARE -> -1 }T
T{ tw_str_0 PAD 0 COMPARE -> 1 }T
T{ PAD 0 tw_str_0 COMPARE -> -1 }T
T{ tw_str_0 tw_str_1 COMPARE -> 1 }T
T{ tw_str_1 tw_str_0 COMPARE -> -1 }T
: "abdde" S" abdde" ;
: "abbde" S" abbde" ;
: "abcdf" S" abcdf" ;
: "abcdee" S" abcdee" ;
T{ tw_str_0 "abdde"  COMPARE -> -1 }T
T{ tw_str_0 "abbde"  COMPARE ->  1 }T
T{ tw_str_0 "abcdf"  COMPARE -> -1 }T
T{ tw_str_0 "abcdee" COMPARE ->  1 }T
T{ tw_str_2 tw_str_3 COMPARE ->  1 }T
T{ tw_str_3 tw_str_2 COMPARE -> -1 }T
test_group_end

.( strchr strrchr ) test_group
: tw_empty S" " ;
: tw_no_delim S" foobar" ;
: tw_leading_delim S" /foo/bar" ;
: tw_middle_delim S" foo/bar" ;
: tw_trailing_delim S" foo/bar/" ;

t{ tw_empty '/' strchr tw_empty COMPARE -> 0 }t
t{ tw_no_delim '/' strchr tw_empty COMPARE -> 0 }t
t{ tw_no_delim 2dup + -rot '#' strchr -rot = -> 0 true }t
t{ tw_leading_delim '/' strchr tw_leading_delim COMPARE -> 0 }t
t{ tw_middle_delim '/' strchr S" /bar" COMPARE -> 0 }t
t{ tw_trailing_delim '/' strchr S" /bar/" COMPARE -> 0 }t

t{ tw_empty '/' strrchr tw_empty COMPARE -> 0 }t
t{ tw_no_delim '/' strrchr tw_empty COMPARE -> 0 }t
t{ tw_no_delim over -rot '#' strrchr -rot = -> 0 true }t
t{ tw_leading_delim '/' strrchr S" /foo/" COMPARE -> 0 }t
t{ tw_middle_delim '/' strrchr S" foo/" COMPARE -> 0 }t
t{ tw_trailing_delim '/' strrchr tw_trailing_delim COMPARE -> 0 }t
test_group_end

.( starts-with ) test_group
T{ s" " s" " starts-with -> TRUE }T
T{ s" " s" foo" starts-with -> FALSE }T
T{ s" hello" s" bar" starts-with -> FALSE }T
T{ s" hello" s" hel" starts-with -> TRUE }T
test_group_end

.( BLANK ) test_group
T{ PAD 25 CHAR a FILL -> }T		\ Fill PAD with 25 'a's
T{ PAD 5 CHARS + 6 BLANK -> }T		\ Put 6 spaced from character 5
T{ PAD 12 S" aaaaa      a" COMPARE -> 0 }T
test_group_end

.( /STRING ) test_group
: tw_str_0 S" abcdefghijklmnopqrstuvwxyz" ;
T{ tw_str_0 5 /STRING -> tw_str_0 SWAP 5 + SWAP 5 - }T
T{ tw_str_0 10 /STRING -4 /STRING -> tw_str_0 6 /STRING }T
T{ tw_str_0 0 /STRING -> tw_str_0 }T
test_group_end

.( -TRAILING ) test_group
: tw_str_0 S" " ;
: tw_str_1 S" abcdefghijklmnopqrstuvwxyz" ;
: tw_str_2 S" abc  " ;
: tw_str_3 S"      " ;
: tw_str_4 S"    a " ;
T{ tw_str_0 -TRAILING -> tw_str_0 }T
T{ tw_str_2 -TRAILING -> tw_str_2 2 - }T
T{ tw_str_0 -TRAILING -> tw_str_0 }T
T{ tw_str_3 -TRAILING -> tw_str_3 DROP 0 }T
T{ tw_str_4 -TRAILING -> tw_str_4 1- }T
test_group_end

.( SEARCH ) test_group
: tw_str_0 S" " ;
: tw_str_1 S" abcdefghijklmnopqrstuvwxyz" ;
: tw_str_2 S" abc" ;
: tw_str_3 S" jklmn" ;
: tw_str_4 S" z" ;
: tw_str_5 S" mnoq" ;
: tw_str_6 S" 12345" ;
T{ tw_str_1 tw_str_0 SEARCH -> tw_str_1 TRUE }T
T{ tw_str_1 tw_str_2 SEARCH -> tw_str_1 TRUE }T
T{ tw_str_1 tw_str_3 SEARCH -> tw_str_1 9 /STRING TRUE }T
T{ tw_str_1 tw_str_4 SEARCH -> tw_str_1 25 /STRING TRUE }T
T{ tw_str_1 tw_str_5 SEARCH -> tw_str_1 FALSE }T
T{ tw_str_1 tw_str_6 SEARCH -> tw_str_1 FALSE }T
test_group_end

.( SLITERAL ) test_group
: tw_str_0 S" abcdefghijklmnopqrstuvwxyz" ;
: tw_str_1 [ tw_str_0 ] SLITERAL ;
T{ tw_str_0 tw_str_1 COMPARE -> 0 }T
T{ tw_str_0 tw_str_1 ROT = ROT ROT = -> TRUE FALSE }T
test_group_end
