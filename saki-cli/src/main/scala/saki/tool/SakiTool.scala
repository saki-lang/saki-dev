package saki.tool

import saki.cli.SakiToolTrait
import saki.error.Error

import java.io.File
import scala.io.Source

object SakiTool extends SakiToolTrait {
  override def run(file: File): Unit = {
    val source = Source.fromFile(file)
    try compileModule(source.mkString) catch {
      case _: Error => ()
      case exception => throw exception
    }
    source.close()
  }
}
