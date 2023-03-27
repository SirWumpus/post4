INCLUDE ../test/assert.p4

[UNDEFINED] jCall [IF]

.( JNI support disabled. ) CR

[ELSE]

.( jFindClass jDeleteLocalRef ) test_group
VARIABLE jobj
t{ S" java/lang/BOGUS" jFindClass -> 0 }t
t{ S" java/lang/Object" jFindClass DUP jobj ! 0<> -> TRUE }t
t{ jobj @ jDeleteLocalRef -> }t
t{ S" java/lang/System" jFindClass DUP jobj ! 0<> -> TRUE }t
t{ jobj @ jDeleteLocalRef -> }t
test_group_end

.( jPushLocalFrame jField jPopLocalFrame ) test_group
VARIABLE jsys
t{ 2 jPushLocalFrame -> }t
t{ S" java/lang/System" jFindClass DUP jsys ! 0<> -> TRUE }t
t{ jsys @ S" out" S" Ljava/io/PrintStream;" jField 0<> -> TRUE }t
t{ jPopLocalFrame -> }t
test_group_end

.( jBoxArray jUnboxArray jArrayLength ) test_group
VARIABLE jarr
t{ 0 jBoxArray DUP jarr ! 0<> -> TRUE }t
t{ jarr @ jArrayLength -> 0 }t
t{ jarr @ jUnboxArray -> 0 }t
t{ jarr @ jDeleteLocalRef -> }t
t{ 12 1 jBoxArray DUP jarr ! 0<> -> TRUE }t
t{ jarr @ jArrayLength -> 1 }t
t{ jarr @ jUnboxArray -> 12 1 }t
t{ jarr @ jDeleteLocalRef -> }t
t{ 12 34 2 jBoxArray DUP jarr ! 0<> -> TRUE }t
t{ jarr @ jArrayLength -> 2 }t
t{ jarr @ jUnboxArray -> 12 34 2 }t
t{ jarr @ jDeleteLocalRef -> }t
t{ 12 34 56 3 jBoxArray DUP jarr ! 0<> -> TRUE }t
t{ jarr @ jArrayLength -> 3 }t
t{ jarr @ jUnboxArray -> 12 34 56 3 }t
t{ jarr @ jDeleteLocalRef -> }t
test_group_end

[THEN]
