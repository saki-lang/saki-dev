package saki.concrete.syntax
import saki.core.SourceSpan

case class Evaluation(expr: ExprTree) extends SyntaxTree[Unit] {
  override def emit: Unit = ()
  override def span: SourceSpan = expr.span
}
