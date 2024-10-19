package saki.tool

import saki.cli.SakiToolTrait
import saki.error.Error

import java.io.File
import scala.io.Source

object SakiTool extends SakiToolTrait {

  lazy val repl = new ReplCore()

  override def run(file: File): Unit = {
    val source = Source.fromFile(file)
    try compileModule(source.mkString, Some(file.getAbsolutePath)) catch {
      case _: Error => ()
      case exception => throw exception
    }
    source.close()
  }

  override def iterate(source: String): Unit = repl.iterate(source)
}
