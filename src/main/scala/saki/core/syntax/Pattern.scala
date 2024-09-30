package saki.core.syntax

import saki.core.typing.{Match, Resolve}
import saki.util.SourceSpan

enum Pattern(val span: SourceSpan) {

  case Primitive(
    value: Literal,
  )(implicit span: SourceSpan) extends Pattern(span)

  case Bind(
    binding: Var.Local,
  )(implicit span: SourceSpan) extends Pattern(span)

  case Cons(
    cons: Var.Defined[Definition.Constructor],
    patterns: Seq[Pattern],
  )(implicit span: SourceSpan) extends Pattern(span)

  case Typed(
    pattern: Pattern,
    `type`: Type,
  )(implicit span: SourceSpan) extends Pattern(span)

  given SourceSpan = span

  override def toString: String = {
    this match {
      case Primitive(value) => value.toString
      case Bind(binding) => binding.name
      case Cons(cons, patterns) => s"${cons.name} ${patterns.mkString(" ")}"
    }
  }

  def buildSubstMap(term: Term): Option[Map[Var.Local, Term]] = {
    Match.buildSubstMap(this, term)
  }

  def matchWith(term: Term): Map[Var.Local, Type] = {
    Match.matchPattern(this, term)
  }
}

case class UnresolvedPattern(
  name: String,
  patterns: Seq[UnresolvedPattern]
)(implicit span: SourceSpan) {
  def resolve(implicit ctx: Resolve.Context): (Pattern, Resolve.Context) = {
    Resolve.resolveUnresolvedPattern(this, span)
  }
}
