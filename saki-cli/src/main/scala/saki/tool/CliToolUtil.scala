package saki.tool

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import saki.concrete.Visitor
import saki.concrete.syntax.{Definition, Evaluation}
import saki.core.syntax.Module
import saki.core.{Error, InfoSpan}
import saki.grammar.{SakiLexer, SakiParser}

def catchError[R](source: String)(action: => R): R = {
  try action catch {
    case error: Error =>
      error.span match {
        case Some(infoSpan) =>
          println(s"Error: ${error.message}")
          printSourceWithHighlight(source, infoSpan)
        case None =>
          println(s"Error: ${error.message}")
      }
      // Rethrow the original Error after showing it
      throw error
  }
}

def printSourceWithHighlight(source: String, span: InfoSpan): Unit = {
  // Split source by lines for display
  val lines = source.split("\n").zipWithIndex

  // Calculate the line and character positions of the error
  val (startLine, endLine) = {
    val (start, end) = (span.start, span.end)
    val startLine = source.substring(0, start).count(_ == '\n')
    val endLine = source.substring(0, end).count(_ == '\n')
    (startLine, endLine)
  }

  // Print each line, and highlight the range containing the error
  lines.foreach {
    case (line, idx) if idx >= startLine && idx <= endLine =>
      println(f"$idx%4d: $line")
      // Highlight the error within the line
      if (idx == startLine) {
        val highlightStart = span.start - source.substring(0, span.start).lastIndexOf('\n') - 1
        val highlightEnd = if (startLine == endLine) span.end - span.start + highlightStart else line.length
        println(" " * (highlightStart + 6) + "^" * (highlightEnd - highlightStart + 1) + " " + span.info)
      }
    case (line, idx) =>
      println(f"$idx%4d: $line")
  }
}

def compileModule(source: String, doEvaluation: Boolean = true): Module = {
  val lexer = SakiLexer(CharStreams.fromString(source))
  val parser = SakiParser(CommonTokenStream(lexer))
  val entities = Visitor().visitProgram(parser.program())
  val definitions = entities.collect { case defn: Definition => defn }
  val evaluations = entities.collect { case eval: Evaluation => eval }
  catchError(source) {
    val module = Module.from(definitions.map(_.emit))
    if doEvaluation then evaluations.foreach { evaluation =>
      println(s"${module.evaluate(evaluation.expr.emit)}")
    }
    module
  }
}
