package saki.cli;

import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;

import java.io.Closeable;
import java.util.List;

@SuppressWarnings("unused")
@CContext(ReadEvalPrintLoop.Directives.class)
public class ReadEvalPrintLoop implements Closeable {

    static class Directives implements CContext.Directives {
        @Override
        public List<String> getHeaderFiles() {
            return List.of("<repl.h>");
        }

        @Override
        public List<String> getLibraries() {
            return List.of("sakirepl");
        }
    }

    @CFunction(value = "init_reporter")
    public static native void initReporter();

    @CFunction(value = "report_error")
    private static native void printError(
        CCharPointer src,
        CCharPointer path,
        CCharPointer title,
        CCharPointer message,
        int offset, int length
    );

    public static void printError(
        String src, String path, String title,
        String message, int offset, int length
    ) {
        ReadEvalPrintLoop.printError(
            stringToCCharPointer(src),
            stringToCCharPointer(path),
            stringToCCharPointer(title),
            stringToCCharPointer(message),
            offset, length
        );
    }

    public static CCharPointer stringToCCharPointer(String javaString) {
        try (CTypeConversion.CCharPointerHolder holder = CTypeConversion.toCString(javaString)) {
            return holder.get();
        }
    }

    @CFunction("repl_iterate")
    private static native void iterate(CCharPointer buffer);

    @CFunction("repl_create_buffer")
    private static native CCharPointer createBuffer();

    @CFunction("repl_drop_buffer")
    private static native void dropBuffer(CCharPointer buffer);

    private final CCharPointer buffer;

    public ReadEvalPrintLoop() {
        this.buffer = createBuffer();
    }

    public String iterate() {
        iterate(buffer);
        return CTypeConversion.toJavaString(buffer);
    }

    @Override
    public void close() {
        dropBuffer(buffer);
    }
}
