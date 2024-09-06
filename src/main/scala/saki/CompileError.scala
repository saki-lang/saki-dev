package saki

import org.antlr.v4.runtime.ParserRuleContext

case class CompileError(message: String, span: SourceSpan, file: Option[String]) {
  override def toString: String = s"CompileError: $message at $span"
}

object CompileError {
  def apply(context: ParserRuleContext, message: String): CompileError = {
    new CompileError(message, context.span, None)
  }
}

class CompileErrorException(val errors: Seq[CompileError]) extends Exception {
  override def getMessage: String = errors.mkString("\n")
}

object CompileErrorException {
  def apply(errors: CompileError*): CompileErrorException = new CompileErrorException(errors)
  def apply(context: ParserRuleContext, message: String): CompileErrorException = {
    new CompileErrorException(Seq(CompileError(context, message)))
  }
}
