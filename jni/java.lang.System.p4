
\ https://docs.oracle.com/en/java/javase/17/
\ https://docs.oracle.com/en/java/javase/17/docs/specs/jni/functions.html

MARKER rm_java_lang_system

: java.lang.System.out ( -- obj )
    S" java/lang/System" jFindClass DUP
    S" out" S" Ljava/io/PrintStream;" jField
    SWAP jDeleteLocalRef
;

: jprints ( caddr u -- )
    8 jPushLocalFrame
    jBoxString
    java.lang.System.out
    S" print" S" (Ljava/lang/String;)V" jCall
    jPopLocalFrame
;

: jprintn ( n -- )
    java.lang.System.out TUCK
    S" print" S" (J)V" jCall
    jDeleteLocalRef
;

: jprintd ( F: f -- )
    fs>ds java.lang.System.out TUCK
    S" print" S" (D)V" jCall
    jDeleteLocalRef
;
