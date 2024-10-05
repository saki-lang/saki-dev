package saki.core.syntax

import saki.core.Entity
import saki.core.elaborate.buildSubstMap

import scala.collection.Seq

extension (clauses: Seq[Clause[Term]]) {
  /**
   * Try to match the given arguments with the clause set.
   * Return the body of the first matching clause.
   */
  def tryMatch(args: Seq[Term]): Option[Term] = {
    clauses.map { clause =>
      val substOpt = clause.patterns.buildSubstMap(args)
      substOpt.map { implicit subst => clause.body.normalize }
    }.collectFirst { case Some(body) => body }
  }
}

/**
 * A clause (case) in a pattern matching expression.
 */
case class Clause[T <: Entity](patterns: Seq[Pattern[T]], body: T) {
  def map[U <: Entity](f: T => U): Clause[U] = Clause(patterns.map(_.map(f)), f(body))
  def mapPatterns(f: Pattern[T] => Pattern[T]): Clause[T] = Clause(patterns.map(f), body)
  def forall(f: T => Boolean): Boolean = f(body) && patterns.forall(_.forall(f))
  override def toString: String = s"${patterns.mkString(", ")} => $body"
}
