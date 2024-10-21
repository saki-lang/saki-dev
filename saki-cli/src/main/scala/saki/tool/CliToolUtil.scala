package saki.tool

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import saki.cli.ReadEvalPrintLoop
import saki.concrete.{ErrorListener, Visitor}
import saki.concrete.syntax.{Definition, Evaluation}
import saki.core.syntax.Module
import saki.error.Error
import saki.grammar.{SakiLexer, SakiParser}

def catchError[R](source: String, path: Option[String] = None)(action: ErrorListener => R): R = {
  val errorListener = ErrorListener(source)
  try action(errorListener) catch {
    case error: Error => {
      error.infoSpans.headOption match {
        case Some(span, info) => {
          val spanLength = span.end - span.start + 1
          ReadEvalPrintLoop.printError(source, path.getOrElse(""), error.message, info, span.start, spanLength)
        }
        case None => println(s"Error: ${error.message}")
      }
      // Rethrow the original Error after showing it
      throw error
    }
  }
}

def compileModule(source: String, path: Option[String] = None, doEvaluation: Boolean = true): Module = {
  catchError(source, path) { listener =>
    val parser = parseSource(source, listener)
    
    val entities = Visitor().visitProgram(parser.program())
    val definitions = entities.collect { case defn: Definition => defn }
    val evaluations = entities.collect { case eval: Evaluation => eval }
    
    val module = Module.from(definitions.map(_.emit))
    if doEvaluation then evaluations.foreach { evaluation =>
      println(s"${module.evaluate(evaluation.expr.emit)}")
    }

    module
  }
}

def parseSource(source: String, listener: ErrorListener): SakiParser = {
  val lexer = SakiLexer(CharStreams.fromString(source))
  lexer.removeErrorListeners()
  lexer.addErrorListener(listener)
  val parser = SakiParser(CommonTokenStream(lexer))
  parser.removeErrorListeners()
  parser.addErrorListener(listener)
  return parser
}
