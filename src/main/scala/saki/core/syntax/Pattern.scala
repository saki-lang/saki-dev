package saki.core.syntax

import saki.core.typing.{Match, Resolve}
import saki.util.SourceSpan

enum Pattern[T](val span: SourceSpan) {

  case Primitive(
    value: Literal,
  )(implicit span: SourceSpan) extends Pattern[T](span)

  case Bind(
    binding: Var.Local,
  )(implicit span: SourceSpan) extends Pattern[T](span)

  case Cons(
    cons: Var.Defined[Definition.Constructor],
    patterns: Seq[Pattern[T]],
  )(implicit span: SourceSpan) extends Pattern[T](span)

  case Typed(
    pattern: Pattern[T],
    `type`: T,
  )(implicit span: SourceSpan) extends Pattern[T](span)

  case Record(
    fields: Seq[(String, Pattern[T])],
  )(implicit span: SourceSpan) extends Pattern[T](span)

  given SourceSpan = span

  override def toString: String = {
    this match {
      case Primitive(value) => value.toString
      case Bind(binding) => binding.name
      case Cons(cons, patterns) => s"${cons.name}(${patterns.mkString(", ")})"
      case Typed(pattern, ty) => s"$pattern : $ty"
      case Record(fields) => s"{${
        fields.map((name, pattern) => s"$name = $pattern").mkString(", ")
      }}"
    }
  }

}

extension (self: Pattern[Term]) {
  def buildSubstMap(term: Term): Option[Map[Var.Local, Term]] = {
    Match.buildSubstMap(self, term)
  }

  def matchWith(term: Term): Map[Var.Local, Type] = {
    Match.matchPattern(self, term)
  }
}

case class UnresolvedPattern(
  name: String,
  patterns: Seq[UnresolvedPattern]
)(implicit span: SourceSpan) {
  def resolve(implicit ctx: Resolve.Context): (Pattern[Term], Resolve.Context) = {
    Resolve.resolveUnresolvedPattern(this, span)
  }
}
