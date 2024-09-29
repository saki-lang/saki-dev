package saki.core.pattern

import saki.core.*
import util.SourceSpan

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

  given SourceSpan = span

  override def toString: String = {
    this match {
      case Primitive(value) => value.toString
      case Bind(binding) => binding.name
      case Cons(cons, patterns) => s"${cons.name} ${patterns.mkString(" ")}"
    }
  }

  def buildSubstMap(term: Term): Option[Map[Var.Local, Term]] = {
    PatternMatching.buildSubstMap(this, term)
  }

  def matchWith(term: Term): Map[Var.Local, Type] = {
    PatternMatching.matchPattern(this, term)
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
