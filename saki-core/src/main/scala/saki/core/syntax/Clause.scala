package saki.core.syntax

import saki.core.Entity
import saki.core.context.{Environment, Typed}
import saki.core.domain.{NeutralValue, Value}
import saki.error.{CoreErrorKind, PanicError}

import scala.collection.Seq
import scala.util.boundary
import scala.util.boundary.break

/**
 * A clause (case) in a pattern matching expression.
 */
case class Clause[T <: Entity](patterns: Seq[Pattern[T]], body: T) {
  def map[U <: Entity](f: T => U): Clause[U] = Clause(patterns.map(_.map(f)), f(body))
  def forall(f: T => Boolean): Boolean = f(body) && patterns.forall(_.forall(f))
  override def toString: String = s"${patterns.mkString(", ")} => $body"
}

extension (clauses: Seq[Clause[Term]]) {
  /**
   * Try to match the given arguments with the clause set.
   * Return the body of the first matching clause.
   */
  def tryMatch(scrutinees: Seq[Value], evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Option[Value] = {

    // Iota-reduction:
    //  filter all clauses in sequence
    //    when the first possible match is a complete match =>
    //      return the evaluate body
    //    when the first possible match is a partial match =>
    //      filter out clauses that are not matched, return the updated matching
    //    when no possible match => throw an error
    enum IotaReducedClause {
      case Matched(body: Value, override val clause: Clause[Term])
      case PartialMatch(override val clause: Clause[Term])
      def clause: Clause[Term]
    }

    lazy val allArgsFinal = scrutinees.forall(_.isFinal(Set.empty))
    // Here we use `iterator` to avoid evaluating all clauses.
    // This is not just an optimization, but also a crucial approach when dealing with panics.
    // e.g.
    // ```
    // match value {
    //   case 0 => 0
    //   case _ => panic("not zero")
    // }
    // ```
    val matchedIter = clauses.iterator.flatMap[IotaReducedClause] {
      clause => boundary {
        val substMap: Map[Var.Local, Value] = {
          clause.patterns.zip(scrutinees).foldLeft(Map.empty[Var.Local, Value]) {
            case (subst, (pattern, value)) => pattern.iotaReduce(value).map {
              case IotaReduction.Matched(bindings) => subst ++ bindings
              case IotaReduction.PartialMatched => break(Some(IotaReducedClause.PartialMatch(clause)))
            }.getOrElse(break(None))
          }
        }
        val typedSubstMap = substMap.map {
          (ident, untyped) => (ident, Typed[Value](untyped, untyped.infer))
        }
        val updatedEnv = env.addAll(typedSubstMap)
        try Some(IotaReducedClause.Matched(clause.body.eval(updatedEnv), clause)) catch {
          // if the error is a panic and not all arguments are final, ignore it
          case _: PanicError if !allArgsFinal => None
          case e: Throwable => throw e
        }
      }
    }

    if !matchedIter.hasNext then CoreErrorKind.PatternMatchFail.raise {
      s"Pattern match failed: ${scrutinees.mkString(", ")}"
    }

    matchedIter.next() match {
      case IotaReducedClause.Matched(body, _) => Some(body)
      case IotaReducedClause.PartialMatch(init) => {
        val clauses = init +: matchedIter.map(_.clause).toSeq
        val valueClauses = clauses.map { clause =>
          // TODO: Is this (matchedBindings) necessary?
          val matchedBindings: Map[Var.Local, Typed[Value]] = scrutinees.zip(clause.patterns).flatMap {
            (scrutinee, pattern) => pattern.buildMatchBindings(scrutinee)
          }.toMap
          // Bind the pattern variables with scrutinee types
          val bindings = scrutinees.zip(clause.patterns).flatMap {
            (scrutinee, pattern) => pattern.buildTypeMapping(scrutinee.infer)
          }.map {
            case (variable, ty) => matchedBindings.get(variable) match {
              case Some(value) => (variable, value)
              case None => (variable, Typed[Value](Value.variable(variable, ty), ty))
            }
          }
          val body = env.withLocals(bindings.toMap) {
            implicit env => clause.body.eval(evalMode)
          }
          Clause(clause.patterns.map(_.map(_.eval(evalMode))), body)
        }
        Some(Value.Neutral(NeutralValue.Match(scrutinees, valueClauses)))
      }
    }
  }

}
