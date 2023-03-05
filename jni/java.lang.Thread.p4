MARKER rm_java_lang_thread

: java.lang.Thread ( -- cls )
    S" java/lang/Thread" jFindClass
;

: jmillisleep ( ms -- )
    java.lang.Thread TUCK
    S" sleep" S" (J)V" jCall
    jdeleteLocalRef
;

: jnanosleep ( ms ns -- )
    java.lang.Thread ROT ROT 2 PICK
    S" sleep" S" (JI)V" jCall
    jDeleteLocalRef
;

: jCurrentThread ( -- thread )
    java.lang.Thread DUP
    S" currentThread" S" ()Ljava/lang/Thread;" jCall
    SWAP jDeleteLocalRef
;

: jDumpStack ( -- )
    java.lang.Thread DUP
    S" dumpStack" S" ()V" jCall
    jDeleteLocalRef
;

: jThreadId ( thread -- n )
    S" getId" S" ()J" jCall
;

: jThreadName ( thread -- caddr u )
    8 jPushLocalFrame
    S" getName" S" ()Ljava/lang/String;" jCall
    jUnboxString OVER >R
    _string0_store R> FREE DROP
    jPopLocalFrame
;

\ Example
\
\ jcurrentthread dup jthreadname type space jdeletelocalref
\
