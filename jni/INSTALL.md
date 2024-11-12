Post4 JNI Install
-----------------

There are currently two methods of installing Post4 JNI.  

* The first method involves downloading the GitHub repository https://github.com/SirWumpus/post4, building, and installing using `make(1)`.  Those instructions are found in `jni/README.md` of the respository.  This is the recommended method as it ensures that you have the latest version and build the correct `libpost4jni.so` for your OS.

* The second method as discussed here are for those cases where you might want to copy the built software to other systems or you are supplied the `Post4.jar` separately.

  The default install location for the `Post4.jar`, `libpost4jni.so`, `post4.p4`, and any additional support files is `/usr/local/lib/post4`.  The `.jar` file bundles all the necessary files within itself, but Java requires native libraries to be external to the `.jar` file and Post4 also needs to find its core file `post4.p4` and any utility `.p4` files outside of the `.jar` file.

        # View contents.
        unzip -l Post4.jar

        # Unpack
        sudo unzip -d /usr/local/lib Post4.jar

        # Do not want cruft in /usr/local/lib
        sudo mv /usr/local/lib/META-INF/MANIFEST.MF /usr/local/lib/post4
        sudo rmdir /usr/local/lib/META-INF

        # Update your $ENV file assuming it hold your enviroment variables.    
        cat >$ENV <<EOF
        export POST4_PATH="/usr/local/lib/post4:/usr/local/lib/post4/jni"
        export CLASSPATH="$CLASSPATH:$POST4_PATH"
        <<EOF
        source $ENV
        
        # Start your engines...
        java -Djava.library.path=$POST4_PATH post4.jni.Post4
        CAFEBABE Monaco 377 e 2.718280 PI 3.141590 Planck's 6.626070E-34
        ok

  Note in the end, we don't actually bother with the .jar file for execution.  In this instance its just a .zip archive for packaging.

-END-
