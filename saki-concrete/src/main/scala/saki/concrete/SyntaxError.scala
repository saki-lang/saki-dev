package saki.concrete

import org.antlr.v4.runtime.{BaseErrorListener, ParserRuleContext, RecognitionException, Recognizer}
import saki.util.{SourcePosition, SourceSpan}
import saki.error.Error

enum SyntaxError extends Exception with Error {

  override def message: String = "Syntax error"

  def info: String
  def span: SourceSpan

  override lazy val infoSpans: Map[SourceSpan, String] = Map(this.span -> info)
  override def toString: String = s"SyntaxError: $info at ${this.span}"

  case InvalidOperator(override val info: String, override val span: SourceSpan)
  case InvalidSymbol(override val info: String, override val span: SourceSpan)
  case InvalidDeclaration(override val info: String, override val span: SourceSpan)
  case ParsingError(override val info: String, override val span: SourceSpan, cause: RecognitionException)
}

extension (context: ParserRuleContext) {

  def span: SourceSpan = SourceSpan(context.start.getStartIndex, context.stop.getStopIndex)

  def raiseError(errCons: (String, SourceSpan) => SyntaxError)(info: => String): Nothing = {
    val span = SourceSpan(context.start.getStartIndex, context.stop.getStopIndex)
    throw errCons(info, span)
  }

}

class ErrorListener(code: String) extends BaseErrorListener {
  override def syntaxError(
    recognizer: Recognizer[_, _],
    offendingSymbol: Any,
    line: Int, charPositionInLine: Int,
    message: String, exception: RecognitionException
  ): Unit = {
    val charPos = SourcePosition(line, charPositionInLine).toCharPos(code)
    val span = SourceSpan(charPos, charPos)
    throw SyntaxError.ParsingError(message, span, exception)
  }
}
