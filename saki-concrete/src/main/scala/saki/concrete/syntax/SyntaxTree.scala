package saki.concrete.syntax

import org.antlr.v4.runtime.ParserRuleContext
import saki.concrete.span
import saki.util.SourceSpan

trait SyntaxTree[+T] {
  def span: SourceSpan
  def emit: T
}

case class Spanned[+T](
  element: T,
)(implicit ctx: ParserRuleContext) extends SyntaxTree[T] {
  def span: SourceSpan = ctx.span
  def emit: T = element
  def get: T = element
  def map[U](f: T => U): Spanned[U] = Spanned(f(element))
}
