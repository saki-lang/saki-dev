package saki.core.elaborate

import saki.core.context.Environment

import scala.collection.Seq
import saki.core.{PatternError, SizeError, SymbolError, TypeError, ValueError}
import saki.core.syntax.*

private[core] object Match {

  def buildPatternMatch(pattern: Pattern[Term], `type`: Term)(
    implicit env: Environment.Untyped[Term]
  ): Map[Var.Local, Term] = pattern match {

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
        case None => env.definitions.getOrElse(cons, SymbolError.undefined(cons.name, pattern.span)) match {
          case consDef: Constructor[Term] => consDef
          case _ => SymbolError.notConstructor(cons.name, pattern.span)
        }
      }
      if consDef.params.size != patterns.size then {
        SizeError.mismatch(consDef.params.size, patterns.size, pattern.span)
      } else if consDef.owner != inductiveType.inductive then {
        ValueError.mismatch(consDef.owner.name, inductiveType.inductive.name, pattern.span)
      } else {
        patterns.zip(consDef.params).foldLeft(Map.empty: Map[Var.Local, Term]) {
          case (subst, (pattern, param)) => subst ++ buildPatternMatch(pattern, param.`type`)
        }
      }
    }

    case Pattern.Cons(_, _) => {
      PatternError.mismatch("Inductive", `type`.toString, pattern.span)
    }

    case Pattern.Typed(pattern, ty) => buildPatternMatch(pattern, ty)

    case Pattern.Record(fields) if `type`.isInstanceOf[Term.RecordType] => {
      val recordType = `type`.asInstanceOf[Term.RecordType]
      fields.foldLeft(Map.empty: Map[Var.Local, Term]) {
        case (subst, (name, pattern)) => {
          val field = recordType.fields.getOrElse(name, {
            ValueError.missingField(name, recordType, pattern.span)
          })
          subst ++ buildPatternMatch(pattern, field)
        }
      }
    }

    case Pattern.Record(_) => {
      PatternError.mismatch("Record", `type`.toString, pattern.span)
    }
  }
  
}
