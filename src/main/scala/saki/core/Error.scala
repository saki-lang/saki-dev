package saki.core

import util.SourceSpan

case class InfoSpan(span: SourceSpan, info: String)

case class TypeError(message: String, span: Option[InfoSpan] = None) extends Exception {
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