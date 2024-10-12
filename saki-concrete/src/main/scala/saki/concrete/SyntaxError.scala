package saki.concrete

import org.antlr.v4.runtime.ParserRuleContext
import saki.core.{Error, InfoSpan, SourceSpan}

case class SyntaxError(
  message: String, span: Option[InfoSpan], file: Option[String]
) extends Exception with Error {
  override def toString: String = s"Syntax error: $message at $span"
}

extension (context: ParserRuleContext) {

  def span: SourceSpan = SourceSpan(context.start.getStartIndex, context.stop.getStopIndex)

  def raiseError(message: String, note: String, file: Option[String] = None): Nothing = {
    throw SyntaxError(message, Some(InfoSpan(span, note)), file)
  }
}
