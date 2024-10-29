package saki.error

import saki.util.SourceSpan

trait Error extends Exception {
  def message: String
  def infoSpans: Map[SourceSpan, String]
}

case class UnknownError(cause: Throwable, span: SourceSpan) extends Error {
  override def message: String = cause.getMessage
  override def infoSpans: Map[SourceSpan, String] = Map(span -> cause.toString)
}

case class SingleSpanError(
  kind: ErrorKind,
  info: String,
  span: SourceSpan,
) extends Error {
  override def message: String = kind.message
  override def infoSpans: Map[SourceSpan, String] = Map(span -> info)
}

case class CoreError(kind: ErrorKind, info: String) extends Exception {
  def spanned(span: SourceSpan): Error = SingleSpanError(kind, info, span)
  override def toString: String = s"Error: ${kind.message} - $info"
}

trait ErrorKind {
  def message: String
}

enum CoreErrorKind(override val message: String) extends ErrorKind {
  case UnsupportedFeature extends CoreErrorKind("Unsupported feature")
  case SizeNotMatch extends CoreErrorKind("Size not match")
  case TypeNotMatch extends CoreErrorKind("Type not match")
  case DefinitionNotMatch extends CoreErrorKind("Definition not match")
  case OverloadingNotMatch extends CoreErrorKind("Overloading not match")
  case OverloadingAmbiguous extends CoreErrorKind("Overloading ambiguous")
  case NoSuchOverloading extends CoreErrorKind("No such overloading")
  case UnresolvedReference extends CoreErrorKind("Unresolved reference")
  case UnexpectedType extends CoreErrorKind("Unexpected type")
  case UnexpectedValue extends CoreErrorKind("Unexpected value")
  case UnboundVariable extends CoreErrorKind("Unbound variable")
  case VariableNotFound extends CoreErrorKind("Variable not found")
  case MethodNotFound extends CoreErrorKind("Method not found")
  case ConstructorNotFound extends CoreErrorKind("Constructor not found")
  case PatternMismatch extends CoreErrorKind("Pattern mismatch")
  case PatternMatchFail extends CoreErrorKind("Pattern match fail")
  case MatchNotExhaustive extends CoreErrorKind("Match not exhaustive")
  case NoLeastUpperBound extends CoreErrorKind("No least upper bound")
  case RecordMissingField extends CoreErrorKind("Missing field")

  def raise(info: => String): Nothing = throw CoreError(this, info)
  def raise(span: SourceSpan)(info: => String): Nothing = throw CoreError(this, info).spanned(span)
}

case class PanicError(message: String) extends Exception
