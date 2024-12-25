INCLUDE-PATH post4/assert.p4

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

.( jBoxString jUnboxString jStringByteLength ) test_group
VARIABLE jstr
VARIABLE caddr
2 jPushLocalFrame

: tw_empty S" " ;
: tw_ello S" 'ello" ;

t{ tw_empty jBoxString DUP jstr ! 0<> -> TRUE }t
t{ jstr @ jStringByteLength -> 0 }t
t{ jstr @ jUnboxString SWAP caddr ! -> 0 }t
t{ caddr @ 0 tw_empty COMPARE -> 0 }t
t{ caddr @ FREE -> 0 }t

t{ tw_ello jBoxString DUP jstr ! 0<> -> TRUE }t
t{ jstr @ jStringByteLength -> 5 }t
t{ jstr @ jUnboxString SWAP caddr ! -> 5 }t
t{ caddr @ 5 tw_ello COMPARE -> 0 }t
t{ caddr @ FREE -> 0 }t

jPopLocalFrame
test_group_end

.( jCall ) test_group
VARIABLE jobj

VARIABLE jlong
t{ 2 jPushLocalFrame -> }t
t{ S" java/lang/Long" jFindClass DUP jobj ! 0<> -> TRUE }t
t{ 377 jobj @ S" valueOf" S" (J)Ljava/lang/Long;" jCall DUP jlong ! 0<> -> TRUE }t
t{ jlong @ S" longValue" S" ()J" jCall -> 377 }t
t{ jPopLocalFrame -> }t

VARIABLE jstr
VARIABLE caddr
t{ 2 jPushLocalFrame -> }t
t{ S" java/lang/System" jFindClass DUP jobj ! 0<> -> TRUE }t
t{ jobj @ S" lineSeparator" S" ()Ljava/lang/String;" jCall DUP jstr ! 0<> -> TRUE }t
t{ jstr @ jUnboxString OVER caddr ! TYPE -> }t
t{ caddr @ FREE -> 0 }t
t{ jPopLocalFrame -> }t
test_group_end

[THEN]
