package saki.concrete.syntax
import saki.util.SourceSpan

case class Evaluation(expr: ExprTree) extends SyntaxTree[Unit] {
  override def emit: Unit = ()
  override def span: SourceSpan = expr.span
}
