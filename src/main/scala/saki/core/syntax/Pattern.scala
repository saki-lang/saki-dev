package saki.core.syntax

import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.{Entity, PatternError, SizeError, SourceSpan, SymbolError, ValueError}
import saki.core.elaborate.{Match, Resolve, Synthesis}
import saki.core.syntax.buildMatch

import scala.collection.Seq

enum Pattern[T <: Entity](val span: SourceSpan) {

  case Primitive(
    value: Literal,
  )(implicit span: SourceSpan) extends Pattern[T](span)

  case Bind(
    binding: Var.Local,
  )(implicit span: SourceSpan) extends Pattern[T](span)

  case Cons(
    cons: Var.Defined[?, Constructor],
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
      case Cons(cons, patterns) => {
        val patternsStr = if patterns.isEmpty then "" else s"(${patterns.mkString(", ")})"
        s"${cons.name}$patternsStr"
      }
      case Typed(pattern, ty) => s"$pattern : $ty"
      case Record(fields) => s"{${
        fields.map((name, pattern) => s"$name = $pattern").mkString(", ")
      }}"
    }
  }

  def map[U <: Entity](f: T => U): Pattern[U] = this match {
    case Primitive(value) => Pattern.Primitive(value)
    case Bind(binding) => Pattern.Bind(binding)
    case Cons(cons, patterns) => Pattern.Cons(cons, patterns.map(_.map(f)))
    case Typed(pattern, ty) => Pattern.Typed(pattern.map(f), f(ty))
    case Record(fields) => Pattern.Record(fields.map((name, pattern) => (name, pattern.map(f))))
  }

  def forall(f: T => Boolean): Boolean = this match {
    case Primitive(_) => true
    case Bind(_) => true
    case Cons(_, patterns) => patterns.forall(_.forall(f))
    case Typed(pattern, ty) => pattern.forall(f) && f(ty)
    case Record(fields) => fields.forall((_, pattern) => pattern.forall(f))
  }

  def getBindings: Seq[Var.Local] = this match {
    case Primitive(_) => Seq.empty
    case Bind(binding) => Seq(binding)
    case Cons(_, patterns) => patterns.flatMap(_.getBindings)
    case Typed(pattern, _) => pattern.getBindings
    case Record(fields) => fields.flatMap((_, pattern) => pattern.getBindings)
  }

}

extension (self: Pattern[Term]) {
  
  def buildMatch(`type`: Term)(
    implicit env: Environment.Typed[Value]
  ): Map[Var.Local, Term] = self match {

    case Pattern.Primitive(_) => Map.empty
    case Pattern.Bind(binding) => Map(binding -> `type`)

    // When calling an inductive directly results in a type.
    // e.g. In this example, `Option(A)` returns a type:
    // ```
    //  inductive Option(A: 'Type) {
    //    None : this
    //    Some : A -> this
    //  }
    // ```
    case Pattern.Cons(cons, patterns) if `type`.isInstanceOf[Term.InductiveType] => {
      val inductiveType = `type`.asInstanceOf[Term.InductiveType]
      val consDef: Constructor[Term] = cons.definition.toOption match {
        case Some(definition) => definition.asInstanceOf[Constructor[Term]]
        case None => env.definitions.getOrElse(cons, SymbolError.undefined(cons.name, self.span)) match {
          case consDef: Constructor[Term] => consDef
          case _ => SymbolError.notConstructor(cons.name, self.span)
        }
      }
      if consDef.params.size != patterns.size then {
        SizeError.mismatch(consDef.params.size, patterns.size, self.span)
      } else if consDef.owner != inductiveType.inductive then {
        ValueError.mismatch(consDef.owner.name, inductiveType.inductive.name, self.span)
      } else {
        patterns.zip(consDef.params).foldLeft(Map.empty: Map[Var.Local, Term]) {
          case (subst, (pattern, param)) => subst ++ pattern.buildMatch(param.`type`)
        }
      }
    }

    case Pattern.Cons(_, _) => {
      PatternError.mismatch("Inductive", `type`.toString, self.span)
    }

    case Pattern.Typed(pattern, ty) => pattern.buildMatch(ty)

    case Pattern.Record(fields) if `type`.isInstanceOf[Term.RecordType] => {
      val recordType = `type`.asInstanceOf[Term.RecordType]
      fields.foldLeft(Map.empty: Map[Var.Local, Term]) {
        case (subst, (name, pattern)) => {
          val field = recordType.fields.getOrElse(name, {
            ValueError.missingField(name, recordType, pattern.span)
          })
          subst ++ pattern.buildMatch(field)
        }
      }
    }

    case Pattern.Record(_) => {
      PatternError.mismatch("Record", `type`.toString, self.span)
    }
  }

  def buildSubstMap(value: Value): Option[Map[Var.Local, Value]] = self match {
    case Pattern.Primitive(literal) if value.isInstanceOf[Value.Primitive] => {
      val primitive = value.asInstanceOf[Value.Primitive]
      if literal == primitive.value then Some(Map.empty) else None
    }
    case Pattern.Bind(binding) => Some(Map(binding -> value))
    case Pattern.Cons(cons, patterns) if value.isInstanceOf[Value.InductiveVariant] => {
      val variant = value.asInstanceOf[Value.InductiveVariant]
      if cons != variant.cons then {
        None
      } else {
        assert(patterns.size == variant.consArgs.size)
        patterns.zip(variant.consArgs).foldLeft(Some(Map.empty): Option[Map[Var.Local, Value]]) {
          case (Some(subst), (pattern, value)) => pattern.buildSubstMap(value).map(subst ++ _)
          case (None, (_, _)) => None
        }
      }
    }
    case _ => None
  }
}

object Pattern {
  extension (self: Pattern[Expr]) {
    def resolve(implicit ctx: Resolve.Context): (Pattern[Expr], Resolve.Context) = {
      Resolve.resolvePattern(self)
    }
  }
}
