package saki.tool

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import saki.cli.ReadEvalPrintLoop
import saki.concrete.{ErrorListener, Visitor}
import saki.concrete.syntax.{Definition, Evaluation}
import saki.core.syntax.{Literal, Module}
import saki.core.term
import saki.core.term.Term
import saki.error.{CoreError, Error, PanicError}
import saki.grammar.{SakiLexer, SakiParser}

def catchError[R](source: String, path: Option[String] = None, doPrint: Boolean = true)(
  action: ErrorListener => R
): R = {
  val errorListener = ErrorListener(source)
  try action(errorListener) catch {
    case error: Error => {
      error.infoSpans.headOption match {
        case Some(span, info) => {
          val spanLength = span.end - span.start + 1
          if doPrint then {
            ReadEvalPrintLoop.printError(source, path.getOrElse(""), error.message, info, span.start, spanLength)
          }
        }
        case None => if doPrint then println(s"Error: ${error.message}")
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
      val evalResult: Term | Unit = try module.evaluate(evaluation.expr.emit).term catch {
        case err: PanicError => println(s"Panic: ${err.message}")
        case err: CoreError => throw err.spanned(evaluation.span)
        case err: Throwable => throw err
      }
      evalResult match {
        case term.Primitive(Literal.StringValue(value)) => println(value)
        case term: Term => println(term.evalString(module.env))
        case _ => { /* Do nothing */ }
      }
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
