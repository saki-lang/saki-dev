package saki.core.syntax

import saki.core.Entity
import saki.core.context.{Environment, Typed}
import saki.core.domain.Value
import saki.error.PanicError

import scala.collection.Seq

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
  def tryMatch(args: Seq[Value])(implicit env: Environment.Typed[Value]): Option[Value] = {
    // Here we use `iterator` to avoid evaluating all clauses.
    // This is not just an optimization, but also a crucial approach when dealing with panics.
    // e.g.
    // ```
    // match value {
    //   case 0 => 0
    //   case _ => panic("not zero")
    // }
    // ```
    lazy val allArgsFinal = args.forall(_.isFinal(Set.empty))
    clauses.iterator.map { clause =>
      val optionalSubstMap: Option[Map[Var.Local, Value]] = {
        clause.patterns.zip(args).foldLeft(Some(Map.empty): Option[Map[Var.Local, Value]]) {
          case (Some(subst), (pattern, value)) => pattern.buildSubstMap(value).map(subst ++ _)
          case _ => None
        }
      }
      optionalSubstMap.flatMap { implicit substMap =>
        val typedSubstMap = substMap.map {
          (ident, untyped) => (ident, Typed[Value](untyped, untyped.infer))
        }
        try Some(clause.body.eval(env.addAll(typedSubstMap))) catch {
          // if the error is a panic and not all arguments are final, ignore it
          case _: PanicError if !allArgsFinal => None
          case e: Throwable => throw e
        }
      }
    }.collectFirst { case Some(body) => body }
  }
}
