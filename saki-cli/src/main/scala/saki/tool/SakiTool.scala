package saki.tool

import saki.cli.SakiToolTrait

import java.io.File
import scala.io.Source

object SakiTool extends SakiToolTrait {
  override def run(file: File): Unit = {
    val source = Source.fromFile(file)
    compileModule(source.mkString)
    source.close()
  }
}
