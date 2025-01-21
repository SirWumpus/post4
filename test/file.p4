INCLUDE-PATH post4/assert.p4

[UNDEFINED] CREATE-FILE [IF]

.( File access support disabled. ) CR

[ELSE]

MARKER rm_file_access

VARIABLE tv_fid1
VARIABLE tv_fid2
: tw_fn1 S" /tmp/fa_file1.txt" ;
: tw_fn2 S" /tmp/fa_file2.txt" ;
: tw_line1 S" Line 1" ;

200 CONSTANT tv_bsize
CREATE tv_buf tv_bsize ALLOT
VARIABLE tv_#chars

.( CREATE-FILE R/W CLOSE-FILE ) test_group
T{ tw_fn1 R/W CREATE-FILE SWAP tv_fid1 ! -> 0 }T
T{ tv_fid1 @ CLOSE-FILE -> 0 }T
test_group_end

.( WRITE-LINE R/W OPEN-FILE ) test_group
T{ tw_fn1 R/W OPEN-FILE SWAP tv_fid1 ! -> 0 }T
T{ tw_line1 tv_fid1 @ WRITE-LINE -> 0 }T
T{ tv_fid1 @ CLOSE-FILE -> 0 }T
test_group_end

.( READ-LINE R/O FILE-POSITION ) test_group
T{ tw_fn1 R/O OPEN-FILE SWAP tv_fid1 ! -> 0 }T
T{ tv_fid1 @ FILE-POSITION -> 0 u>d 0 }T
T{ tv_buf 100 tv_fid1 @ READ-LINE ROT DUP tv_#chars ! -> TRUE 0 tw_line1 SWAP DROP }T
T{ tv_buf tv_#chars @ tw_line1 COMPARE -> 0 }T
T{ tv_fid1 @ CLOSE-FILE -> 0 }T
test_group_end

.( FILE-POSITION FILE-SIZE FLUSH-FILE READ-FILE REPOSITION-FILE BIN ) test_group
: tw_cbuf tv_buf tv_bsize 0 FILL ;
: tw_setpad PAD 50 0 DO I OVER C! CHAR+ LOOP DROP ;
tw_setpad
\ If anything else is defined setpad must be called again as the pad may move
T{ tw_fn2 R/W BIN CREATE-FILE SWAP tv_fid2 ! -> 0 }T
T{ PAD 50 tv_fid2 @ WRITE-FILE tv_fid2 @ FLUSH-FILE -> 0 0 }T
T{ tv_fid2 @ FILE-SIZE -> 50 u>d 0 }T
T{ 0 u>d tv_fid2 @ REPOSITION-FILE -> 0 }T
T{ tw_cbuf tv_buf 29 tv_fid2 @ READ-FILE -> 29 0 }T
T{ PAD 29 tv_buf 29 COMPARE -> 0 }T
T{ PAD 30 tv_buf 30 COMPARE -> 1 }T
T{ tw_cbuf tv_buf 29 tv_fid2 @ READ-FILE -> 21 0 }T
T{ PAD 29 + 21 tv_buf 21 COMPARE -> 0 }T
T{ tv_fid2 @ FILE-SIZE DROP tv_fid2 @ FILE-POSITION DROP D= -> TRUE }T
T{ tv_buf 10 tv_fid2 @ READ-FILE -> 0 0 }T
T{ tv_fid2 @ CLOSE-FILE -> 0 }T
test_group_end

.( DELETE-FILE ) test_group
T{ S" /tmp/fa_bogus.txt" DELETE-FILE 0<> -> TRUE }T
T{ tw_fn1 R/W BIN OPEN-FILE SWAP tv_fid1 ! -> 0 }T
T{ tv_fid1 @ CLOSE-FILE -> 0 }T
T{ tw_fn1 DELETE-FILE -> 0 }T
test_group_end

.( SOURCE-ID ) test_group
T{ SOURCE-ID DUP -1 = SWAP 0= OR -> FALSE }T
test_group_end

[DEFINED] REQUIRED [IF]
.( REQUIRE REQUIRED ) test_group
T{ 0
   S" ../test/data/required1.p4" REQUIRED	\ Increment TOS
   REQUIRE ../test/data/required1.p4		\ Ignore - already loaded
   INCLUDE ../test/data/required1.p4		\ Increment TOS
-> 2 }T
T{ 0
   INCLUDE ../test/data/required2.p4		\ Increment TOS
   S" ../test/data/required2.p4" REQUIRED 	\ Ignored - already loaded
   REQUIRE ../test/data/required2.p4		\ Ignored - already loaded
   S" ../test/data/required2.p4" INCLUDED	\ Increment TOS
-> 2 }T
test_group_end
[THEN]

.( GH-86 source-base-path ) test_group
t{ source-base-path s" source-base-path" evaluate compare 0= -> true }t
ts{ source-path s" ../test/data/source_path.p4" included source-path compare 0= -> true }t
test_group_end

.( GH-95 source-path ) test_group
ts{ s" source-path nip 0<>" evaluate -> true }t
test_group_end

rm_file_access

[THEN]
