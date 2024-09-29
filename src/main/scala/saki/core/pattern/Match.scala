package saki.core.pattern

import saki.core.{Elaborate, Expr, Param, PatternError, SizeError, Term, Type, TypeError, Var}

object PatternMatching {

  def matchPattern(pattern: Pattern, `type`: Type): Map[Var.Local, Type] = pattern match {

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
    case Pattern.Cons(cons, patterns) if `type`.isInstanceOf[Term.InductiveCall] => {
      val inductiveCall = `type`.asInstanceOf[Term.InductiveCall]
      if cons.definition.get.params.size != inductiveCall.args.size then {
        SizeError.mismatch(
          cons.definition.get.params.size,
          inductiveCall.args.size,
          pattern.span
        )
      } else {
        patterns.zip(inductiveCall.args).foldLeft(Map.empty: Map[Var.Local, Type]) {
          case (subst, (pattern, argType)) => subst ++ matchPattern(pattern, argType)
        }
      }
    }

    case Pattern.Cons(_, _) => {
      PatternError.mismatch("Inductive", `type`.toString, pattern.span)
    }

    // TODO: record pattern
  }

  /**
   * Build a substitution map from a pattern and a term.
   * When a match fails, return None.
   */
  def buildSubstMap(pattern: Pattern, term: Term): Option[Map[Var.Local, Term]] = pattern match {
    case Pattern.Primitive(value) if term.isInstanceOf[Term.Primitive] => {
      val primitive = term.asInstanceOf[Term.Primitive]
      if value == primitive.value then Some(Map.empty) else None
    }
    case Pattern.Bind(binding) => Some(Map(binding -> term))
    case Pattern.Cons(cons, patterns) if term.isInstanceOf[Term.ConstructorCall] => {
      val consCall = term.asInstanceOf[Term.ConstructorCall]
      if cons != consCall.cons then {
        TypeError.mismatch(cons.toString, consCall.cons.toString, pattern.span)
      } else {
        buildSubstMap(patterns, consCall.consArgs)
      }
    }
    case _ => PatternError.unexpected(pattern, term, pattern.span)
  }

  /**
   * Build a substitution map from a sequence of patterns and terms.
   * All patterns must match the corresponding terms.
   */
  def buildSubstMap(patterns: Seq[Pattern], terms: Seq[Term]): Option[Map[Var.Local, Term]] = {
    patterns.zip(terms).foldLeft(Some(Map.empty): Option[Map[Var.Local, Term]]) {
      case (Some(subst), (pattern, term)) => pattern.buildSubstMap(term).map(subst ++ _)
      case _ => None
    }
  }

}

extension (patterns: Seq[Pattern]) {
  def buildSubstMap(terms: Seq[Term]): Option[Map[Var.Local, Term]] = {
    PatternMatching.buildSubstMap(patterns, terms)
  }
}
