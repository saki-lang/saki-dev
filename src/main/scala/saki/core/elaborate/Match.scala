package saki.core.elaborate

import scala.collection.Seq
import saki.core.{PatternError, SizeError, SymbolError, TypeError, ValueError}
import saki.core.syntax.*

private[core] object Match {

  def matchPattern(
    pattern: Pattern[Term], `type`: Term
  )(implicit ctx: Synthesis.Context): Map[Var.Local, Term] = pattern match {

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
        case None => ctx.definitions.getOrElse(cons, SymbolError.undefined(cons.name, pattern.span)) match {
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
          case (subst, (pattern, param)) => subst ++ matchPattern(pattern, param.`type`)
        }
      }
    }

    case Pattern.Cons(_, _) => {
      PatternError.mismatch("Inductive", `type`.toString, pattern.span)
    }

    case Pattern.Typed(pattern, ty) => matchPattern(pattern, ty)

    case Pattern.Record(fields) if `type`.isInstanceOf[Term.RecordType] => {
      val recordType = `type`.asInstanceOf[Term.RecordType]
      fields.foldLeft(Map.empty: Map[Var.Local, Term]) {
        case (subst, (name, pattern)) => {
          val field = recordType.fields.getOrElse(name, {
            ValueError.missingField(name, recordType, pattern.span)
          })
          subst ++ matchPattern(pattern, field)
        }
      }
    }

    case Pattern.Record(_) => {
      PatternError.mismatch("Record", `type`.toString, pattern.span)
    }
  }

  /**
   * Build a substitution map from a pattern and a term.
   * When a match fails, return None.
   */
  def buildSubstMap(pattern: Pattern[Term], term: Term): Option[Map[Var.Local, Term]] = pattern match {
    case Pattern.Primitive(value) if term.isInstanceOf[Term.Primitive] => {
      val primitive = term.asInstanceOf[Term.Primitive]
      if value == primitive.value then Some(Map.empty) else None
    }
    case Pattern.Bind(binding) => Some(Map(binding -> term))
    case Pattern.Cons(cons, patterns) if term.isInstanceOf[Term.InductiveVariant] => {
      val consCall = term.asInstanceOf[Term.InductiveVariant]
      if cons != consCall.cons then {
        None
      } else {
        buildSubstMap(patterns, consCall.consArgs)
      }
    }
    case _ => None
  }

  /**
   * Build a substitution map from a sequence of patterns and terms.
   * All patterns must match the corresponding terms.
   */
  def buildSubstMap(patterns: Seq[Pattern[Term]], terms: Seq[Term]): Option[Map[Var.Local, Term]] = {
    patterns.zip(terms).foldLeft(Some(Map.empty): Option[Map[Var.Local, Term]]) {
      case (Some(subst), (pattern, term)) => buildSubstMap(pattern, term).map(subst ++ _)
      case _ => None
    }
  }

}

extension (patterns: Seq[Pattern[Term]]) {
  def buildSubstMap(terms: Seq[Term]): Option[Map[Var.Local, Term]] = {
    Match.buildSubstMap(patterns, terms)
  }
}
