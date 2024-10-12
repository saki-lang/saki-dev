package saki.cli;

import picocli.CommandLine;

import java.io.File;

@CommandLine.Command(
    name = "saki",
    description = "Saki programming language CLI",
    subcommands = {
        SakiCliMain.Run.class,
    }
)
public class SakiCliMain {

    private final SakiToolTrait core;
    public final CommandLine commandLine = new CommandLine(this, new InnerClassFactory(this));

    public SakiCliMain(SakiToolTrait core) {
        this.core = core;
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
        private File file = null;

        @Override
        public void run() { core.run(file); }
    }

}
