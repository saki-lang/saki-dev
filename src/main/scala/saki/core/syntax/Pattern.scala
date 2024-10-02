package saki.core.syntax

import saki.core.SourceSpan
import saki.core.typing.{Match, Resolve}

import scala.collection.Seq

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

  def map[U](f: T => U): Pattern[U] = {
    this match {
      case Primitive(value) => Pattern.Primitive(value)
      case Bind(binding) => Pattern.Bind(binding)
      case Cons(cons, patterns) => Pattern.Cons(cons, patterns.map(_.map(f)))
      case Typed(pattern, ty) => Pattern.Typed(pattern.map(f), f(ty))
      case Record(fields) => Pattern.Record(fields.map((name, pattern) => (name, pattern.map(f))))
    }
  }

  def getBindings: Seq[Var.Local] = {
    this match {
      case Primitive(_) => Seq.empty
      case Bind(binding) => Seq(binding)
      case Cons(_, patterns) => patterns.flatMap(_.getBindings)
      case Typed(pattern, _) => pattern.getBindings
      case Record(fields) => fields.flatMap((_, pattern) => pattern.getBindings)
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

object Pattern {
  extension (self: Pattern[Expr]) {
    def resolve(implicit ctx: Resolve.Context): (Pattern[Expr], Resolve.Context) = {
      Resolve.resolvePattern(self)
    }
  }
}
