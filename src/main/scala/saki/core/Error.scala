package saki.core

trait Error {
  def message: String
  def span: Option[InfoSpan]
}

case class SourceSpan(start: Int, end: Int)

case class InfoSpan(span: SourceSpan, info: String) {
  def start: Int = span.start
  def end: Int = span.end
}

case class TypeError(message: String, span: Option[InfoSpan] = None) extends Exception with Error {
  override def toString: String = message
  def raise: Nothing = throw this
  def withSpan(span: SourceSpan, info: String): TypeError = TypeError(message, Some(InfoSpan(span, info)))
}

object TypeError {
  def mismatch(expected: String, actual: String, span: SourceSpan): Nothing = {
    throw TypeError("Type mismatch", Some(InfoSpan(span, s"expected $expected, found $actual")))
  }

  def error(info: String, span: SourceSpan): Nothing = {
    throw TypeError("Type error", Some(InfoSpan(span, info)))
  }

  def overloadingNoMatch(name: String, span: Option[InfoSpan]): Nothing = {
    throw TypeError(s"No matched overloading: $name", span)
  }

  def overloadingMultipleMatch(name: String, span: Option[InfoSpan]): Nothing = {
    throw TypeError(s"Multiple matched overloading: $name", span)
  }
}

object SymbolError {
  def notFound(name: String, span: SourceSpan): Nothing = {
    throw TypeError("Symbol not found", Some(InfoSpan(span, s"symbol $name not found")))
  }

  def undefined(name: String, span: SourceSpan): Nothing = {
    throw TypeError("Symbol undefined", Some(InfoSpan(span, s"symbol $name undefined")))
  }

  def notConstructor(name: String, span: SourceSpan): Nothing = {
    throw TypeError("Symbol not a constructor", Some(InfoSpan(span, s"symbol $name is not a constructor")))
  }
}

case class ValueError(message: String, span: Option[InfoSpan] = None) extends Exception with Error {
  override def toString: String = message
  def raise: Nothing = throw this
  def withSpan(span: SourceSpan, info: String): ValueError = ValueError(message, Some(InfoSpan(span, info)))
}

object ValueError {
  def error(info: String, span: SourceSpan): Nothing = {
    throw ValueError("Value error", Some(InfoSpan(span, info)))
  }

  def missingField(name: String, record: Term.RecordType, span: SourceSpan): Nothing = {
    throw ValueError("Missing field", Some(InfoSpan(span, s"missing field $name in $record")))
  }

  def mismatch(expected: String, actual: String, span: SourceSpan): Nothing = {
    throw ValueError("Value mismatch", Some(InfoSpan(span, s"expected $expected, found $actual")))
  }
}

case class SizeError(message: String, span: Option[InfoSpan] = None) extends Exception with Error {
  override def toString: String = message
  def raise: Nothing = throw this
  def withSpan(span: SourceSpan, info: String): SizeError = SizeError(message, Some(InfoSpan(span, info)))
}

object SizeError {
  def mismatch(expected: Int, actual: Int, span: SourceSpan): Nothing = {
    throw SizeError("Size mismatch", Some(InfoSpan(span, s"expected $expected, found $actual")))
  }
}

case class PatternError(message: String, span: Option[InfoSpan] = None) extends Exception with Error {
  override def toString: String = message
  def raise: Nothing = throw this
  def withSpan(span: SourceSpan, info: String): PatternError = PatternError(message, Some(InfoSpan(span, info)))
}

object PatternError {
  def mismatch(expected: String, actual: String, span: SourceSpan): Nothing = {
    throw PatternError("Pattern[Term] mismatch", Some(InfoSpan(span, s"expected $expected, found $actual")))
  }

  def noMatch(value: String, span: SourceSpan): Nothing = {
    throw PatternError("No matched pattern", Some(InfoSpan(span, s"no matched pattern with $value")))
  }
  
  def unexpected(pattern: Pattern[Term], term: Term, span: SourceSpan): Nothing = {
    throw PatternError("Unexpected pattern", Some(InfoSpan(span, s"expected $pattern, found $term")))
  }
}

case class UnsupportedError(message: String, span: Option[InfoSpan] = None) extends Exception with Error {
  override def toString: String = message
  def raise: Nothing = throw this
  def withSpan(span: SourceSpan, info: String): UnsupportedError = UnsupportedError(message, Some(InfoSpan(span, info)))
}

object UnsupportedError {
  def unsupported(feature: String, span: SourceSpan): Nothing = {
    throw UnsupportedError("Unsupported feature", Some(InfoSpan(span, s"unsupported $feature")))
  }
}