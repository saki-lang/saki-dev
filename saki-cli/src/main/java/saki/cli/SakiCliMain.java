package saki.cli;

import picocli.CommandLine;

import java.io.File;

@SuppressWarnings("unused")
@CommandLine.Command(
    name = "saki",
    description = "Saki programming language CLI",
    subcommands = {
        SakiCliMain.Run.class,
    }
)
public class SakiCliMain implements Runnable {

    private final SakiToolTrait core;
    public final CommandLine commandLine = new CommandLine(this, new InnerClassFactory(this));

    public SakiCliMain(SakiToolTrait core) {
        this.core = core;
    }

    @Override
    @SuppressWarnings("InfiniteLoopStatement")
    public void run() {
        try (var repl = new ReadEvalPrintLoop()) {
            while (true) {
                String source = repl.iterate();
                core.iterate(source);
            }
        }
    }

    @CommandLine.Command(
        name = "run",
        description = "Run a Saki program"
    )
    class Run implements Runnable {

        @CommandLine.Parameters(
            index = "0",
            description = "The Saki program file, with the '.saki' extension"
        )
        private File file;

        @Override
        public void run() { core.run(file); }
    }

}
