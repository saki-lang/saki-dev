package saki.core

import saki.core.syntax.{Pattern, Term}
import saki.util.SourceSpan

trait Error {
  def message: String
  def span: Option[InfoSpan]
}

case class InfoSpan(span: SourceSpan, info: String)

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
    throw PatternError("Pattern mismatch", Some(InfoSpan(span, s"expected $expected, found $actual")))
  }
  
  def unexpected(pattern: Pattern, term: Term, span: SourceSpan): Nothing = {
    throw PatternError("Unexpected pattern", Some(InfoSpan(span, s"expected $pattern, found $term")))
  }
}