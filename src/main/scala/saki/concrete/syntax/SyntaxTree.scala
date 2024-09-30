package saki.concrete.syntax

import saki.util.SourceSpan

trait SyntaxTree[+T] {
  def span: SourceSpan
  def emit: T
}
