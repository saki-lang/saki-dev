
import saki.cli.SakiCliMain
import saki.tool.SakiTool

@main
def main(args: String*): Unit = {
  val cli = new SakiCliMain(SakiTool)
  val exitCode = cli.commandLine.execute(args: _*)
  System.exit(exitCode)
}
