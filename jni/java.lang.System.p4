
\ https://docs.oracle.com/en/java/javase/17/
\ https://docs.oracle.com/en/java/javase/17/docs/specs/jni/functions.html

MARKER rm_java_lang_system

: java.lang.System ( -- obj )
    S" java/lang/System" jFindClass
;

: java.lang.System.out ( -- obj )
    java.lang.System DUP
    S" out" S" Ljava/io/PrintStream;" jField
    SWAP jDeleteLocalRef
;

: jprints ( jstr -- )
    java.lang.System.out TUCK
    S" print" S" (Ljava/lang/String;)V" jCall
    jDeleteLocalRef
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

: jNewline ( -- jstr )
    java.lang.System DUP
    S" lineSeparator" S" ()Ljava/lang/String;" jCall
    SWAP jDeleteLocalRef
;

: print ( caddr u -- )
    4 jPushLocalFrame
    jBoxString jprints
    jPopLocalFrame
;
