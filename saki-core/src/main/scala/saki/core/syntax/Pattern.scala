package saki.core.syntax

import saki.core.context.{Environment, Typed}
import saki.core.domain.{Type, Value}
import saki.core.elaborate.Resolve
import saki.core.*
import saki.error.CoreErrorKind.*
import saki.util.SourceSpan

import scala.collection.Seq

enum Pattern[T <: Entity](val span: SourceSpan) {

  case Primitive(
    value: Literal,
  )(implicit span: SourceSpan) extends Pattern[T](span)

  case Bind(
    binding: Var.Local,
  )(implicit span: SourceSpan) extends Pattern[T](span)

  case Variant(
    inductive: T,
    constructor: String,
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
      case Variant(inductive, constructor, patterns) => {
        val patternsStr = if patterns.isEmpty then "" else s"(${patterns.mkString(", ")})"
        s"$inductive::$constructor$patternsStr"
      }
      case Typed(pattern, ty) => s"$pattern : $ty"
      case Record(fields) => s"{${
        fields.map((name, pattern) => s"$name = $pattern").mkString(", ")
      }}"
    }
  }

  def map[U <: Entity](transform: T => U): Pattern[U] = this match {
    case Primitive(value) => Pattern.Primitive(value)
    case Bind(binding) => Pattern.Bind(binding)
    case Variant(inductive, constructor, patterns) => {
      // FIXME: some bugs here
      Pattern.Variant(transform(inductive), constructor, patterns.map(_.map(transform)))
    }
    case Typed(pattern, ty) => Pattern.Typed(pattern.map(transform), transform(ty))
    case Record(fields) => Pattern.Record(fields.map((name, pattern) => (name, pattern.map(transform))))
  }

  def forall(predicate: T => Boolean): Boolean = this match {
    case Primitive(_) => true
    case Bind(_) => true
    case Variant(inductive, _, patterns) => predicate(inductive) && patterns.forall(_.forall(predicate))
    case Typed(pattern, ty) => pattern.forall(predicate) && predicate(ty)
    case Record(fields) => fields.forall((_, pattern) => pattern.forall(predicate))
  }

  def getBindings: Seq[Var.Local] = this match {
    case Primitive(_) => Seq.empty
    case Bind(binding) => Seq(binding)
    case Variant(_, _, patterns) => patterns.flatMap(_.getBindings)
    case Typed(pattern, _) => pattern.getBindings
    case Record(fields) => fields.flatMap((_, pattern) => pattern.getBindings)
  }

}

extension (self: Pattern[Term]) {
  
  def buildMatchBindings(`type`: Type)(
    implicit env: Environment.Typed[Value]
  ): Map[Var.Local, Type] = self match {

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
    case Pattern.Variant(
      patternInductiveTerm, constructorIdent, patterns
    ) if `type`.isInstanceOf[Value.InductiveType] => {
      val patternInductiveType = patternInductiveTerm.eval match {
        case inductiveType: Value.InductiveType => inductiveType
        case ty => TypeNotMatch.raise {
          s"Expected inductive type, but got: ${ty.readBack}"
        }
      }
      val inductiveType = `type`.asInstanceOf[Value.InductiveType]
      val patternInductive = patternInductiveType.inductive.definition.get
      val constructor = patternInductive.getConstructor(constructorIdent) match {
        case Some(constructor) => constructor
        case None => ConstructorNotFound.raise {
          s"Constructor $constructorIdent not found in ${patternInductive.ident}"
        }
      }
      if constructor.params.size != patterns.size then {
        SizeNotMatch.raise {
          s"Constructor ${constructor.ident} of ${patternInductive.ident}" +
          s"expected ${constructor.params.size} arguments, but got ${patterns.size}"
        }
      } else if !(patternInductiveType <:< inductiveType) then {
        TypeNotMatch.raise {
          s"Expected inductive type ${inductiveType.readBack}, but got ${patternInductiveType.readBack}"
        }
      } else {
        patterns.zip(constructor.params).foldLeft(Map.empty: Map[Var.Local, Value]) {
          case (subst, (pattern, param)) => subst ++ env.withLocals(patternInductiveType.argsMap) {
            implicit env => pattern.buildMatchBindings(param.`type`.eval)
          }
        }
      }
    }

    case Pattern.Variant(inductive, _, _) => {
      TypeNotMatch.raise {
        s"Expected inductive type ${inductive}, but got: ${`type`.readBack}"
      }
    }

    case Pattern.Typed(pattern, ty) => pattern.buildMatchBindings(ty.eval)

    case Pattern.Record(fields) if `type`.isInstanceOf[Value.RecordType] => {
      val recordType = `type`.asInstanceOf[Value.RecordType]
      fields.foldLeft(Map.empty: Map[Var.Local, Value]) {
        case (subst, (name, pattern)) => {
          val fieldType = recordType.fields.getOrElse(name, {
            RecordMissingField.raise(s"Field $name not found in record")
          })
          subst ++ pattern.buildMatchBindings(fieldType)
        }
      }
    }

    case Pattern.Record(_) => TypeNotMatch.raise {
      s"Expected record type, but got: ${`type`.readBack}"
    }
  }

  def buildSubstMap(value: Value)(
    implicit env: Environment.Typed[Value]
  ): Option[Map[Var.Local, Value]] = self match {
    case Pattern.Primitive(literal) if value.isInstanceOf[Value.Primitive] => {
      val primitive = value.asInstanceOf[Value.Primitive]
      if literal == primitive.value then Some(Map.empty) else None
    }
    case Pattern.Bind(binding) => Some(Map(binding -> value))
    case Pattern.Variant(
      inductiveTerm, constructorIdent, patterns
    ) if value.isInstanceOf[Value.InductiveVariant] => {
      val inductiveType = inductiveTerm.eval match {
        case inductive: Value.InductiveType => inductive
        case ty => TypeNotMatch.raise(s"Expected inductive type, but got: ${ty.readBack}")
      }
      val constructor = inductiveType.inductive.definition.get.getConstructor(constructorIdent) match {
        case Some(constructor) => constructor
        case None => ConstructorNotFound.raise {
          s"Constructor $constructorIdent not found in ${inductiveType.inductive.name}"
        }
      }
      val variant = value.asInstanceOf[Value.InductiveVariant]
      if constructor != variant.constructor then None else {
        assert(patterns.size == variant.args.size)
        patterns.zip(variant.args).foldLeft(Some(Map.empty): Option[Map[Var.Local, Value]]) {
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
