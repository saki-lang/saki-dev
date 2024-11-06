package saki.core.syntax

import saki.core.context.{Environment, Typed}
import saki.core.domain.{Type, Value}
import saki.core.elaborate.Resolve
import saki.core.*
import saki.error.CoreErrorKind.*
import saki.util.SourceSpan

import scala.annotation.tailrec
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

extension [T <: RuntimeEntity[Type]](self: Pattern[T]) {

  def buildTypeMapping(`type`: Type)(implicit env: Environment.Typed[Value]): Map[Var.Local, Type] = self match {

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
            implicit env => pattern.buildTypeMapping(param.`type`.eval)
          }
        }
      }
    }

    case Pattern.Variant(inductive, _, _) => {
      TypeNotMatch.raise {
        s"Expected inductive type ${inductive}, but got: ${`type`.readBack}"
      }
    }

    case Pattern.Typed(pattern, ty) => pattern.buildTypeMapping(ty.eval)

    case Pattern.Record(fields) if `type`.isInstanceOf[Value.RecordType] => {
      val recordType = `type`.asInstanceOf[Value.RecordType]
      fields.foldLeft(Map.empty: Map[Var.Local, Value]) {
        case (subst, (name, pattern)) => {
          val fieldType = recordType.fields.getOrElse(name, {
            RecordMissingField.raise(s"Field $name not found in record")
          })
          subst ++ pattern.buildTypeMapping(fieldType)
        }
      }
    }

    case Pattern.Record(_) => TypeNotMatch.raise {
      s"Expected record type, but got: ${`type`.readBack}"
    }
  }

  def buildMatchBindings(
    value: Value, `type`: Option[Type] = None
  )(implicit env: Environment.Typed[Value]): Map[Var.Local, Typed[Value]] = {

    lazy val ty: Type = `type` match {
      case Some(ty) => ty
      case None => value.infer
    }

    self match {

      case Pattern.Primitive(_) => Map.empty
      case Pattern.Bind(binding) => Map(binding -> Typed(value, ty))

      case Pattern.Typed(pattern, expectedType) => {
        val expectedTypeValue = expectedType.eval
        if value.isFinal() && !(expectedTypeValue <:< ty) then TypeNotMatch.raise {
          s"Expected type ${expectedTypeValue.readBack}, but got: ${ty.readBack}"
        }
        pattern.buildMatchBindings(value, Some(expectedTypeValue))
      }

      case _ if value.isNeutral => Map.empty

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
      ) if value.isInstanceOf[Value.InductiveVariant] => {
        val variant = value.asInstanceOf[Value.InductiveVariant]
        val patternInductiveType = patternInductiveTerm.eval match {
          case inductiveType: Value.InductiveType => inductiveType
          case ty => TypeNotMatch.raise {
            s"Expected inductive type, but got: ${ty.readBack}"
          }
        }
        val inductiveType = ty.asInstanceOf[Value.InductiveType]
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
          patterns.zip(variant.args).foldLeft(Map.empty: Map[Var.Local, Typed[Value]]) {
            case (subst, (pattern, value)) => subst ++ pattern.buildMatchBindings(value)
          }
        }
      }

      case Pattern.Record(patternFields) if value.isInstanceOf[Value.Record] => {
        val record = value.asInstanceOf[Value.Record]
        patternFields.foldLeft(Map.empty[Var.Local, Typed[Value]]) {
          case (subst, (name, pattern)) => {
            val field = record.fields.getOrElse(name, {
              RecordMissingField.raise(s"Field $name not found in record")
            })
            subst ++ pattern.buildMatchBindings(field)
          }
        }
      }

      case _ => Map.empty
    }
  }

  private[core] def iotaReduce(value: Value)(implicit env: Environment.Typed[Value]): Option[IotaReduction] = {
    
    self match {
      
      case Pattern.Primitive(literal) if value.isInstanceOf[Value.Primitive] => {
        val primitive = value.asInstanceOf[Value.Primitive]
        if literal == primitive.value then Some(IotaReduction.Matched(Map.empty)) else None
      }
      
      case Pattern.Bind(binding) => Some(IotaReduction.Matched(Map(binding -> value)))

      case Pattern.Typed(pattern, ty) => {
        val expectedType: Value = ty.eval
        val valueType: Value = value.infer
        if value.isFinal() && expectedType <:< valueType then {
          pattern.iotaReduce(value)
        } else if valueType <:< expectedType then {
          Some(IotaReduction.PartialMatched)
        } else None
      }

      case _ if value.isNeutral => Some(IotaReduction.PartialMatched)

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
          patterns.zip(variant.args).foldLeft(Some(IotaReduction.Matched(Map.empty)): Option[IotaReduction]) {
            case (Some(reduction), (pattern, value)) => reduction match {
              case IotaReduction.PartialMatched => Some(IotaReduction.PartialMatched)
              case IotaReduction.Matched(subst) => pattern.iotaReduce(value).map {
                case IotaReduction.PartialMatched => IotaReduction.PartialMatched
                case IotaReduction.Matched(bindings) => IotaReduction.Matched(subst ++ bindings)
              }
            }
            case (None, (_, _)) => None
          }
        }
      }
      
      case _ => None
    }
  }

  def toTerm(implicit env: Environment.Typed[Value]): Term = self match {
    case Pattern.Primitive(value) => Term.Primitive(value)
    case Pattern.Bind(binding) => Term.Variable(binding)
    case Pattern.Variant(inductiveTerm, constructorIdent, patterns) => {
      val inductiveType = inductiveTerm.eval match {
        case inductive: Value.InductiveType => inductive
        case ty => TypeNotMatch.raise(s"Expected inductive type, but got: ${ty.readBack}")
      }
      val inductive = inductiveType.inductive.definition.get
      val constructor = inductive.getConstructor(constructorIdent) match {
        case Some(constructor) => constructor
        case None => ConstructorNotFound.raise {
          s"Constructor $constructorIdent not found in ${inductive.ident}"
        }
      }
      val args = patterns.map(_.toTerm)
      Term.InductiveVariant(inductiveType.readBack, constructor, args)
    }
    case Pattern.Typed(pattern, _) => pattern.toTerm
    case Pattern.Record(fields) => {
      val fieldValues = fields.map((name, pattern) => (name, pattern.toTerm))
      Term.Record(fieldValues.toMap)
    }
  }
}

private[core] enum IotaReduction {
  case PartialMatched
  case Matched(bindings: Map[Var.Local, Value])
}

object Pattern {
  extension (self: Pattern[Expr]) {
    def resolve(implicit ctx: Resolve.Context): (Pattern[Expr], Resolve.Context) = {
      Resolve.resolvePattern(self)
    }
  }
}
