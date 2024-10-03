package saki.core.syntax

import saki.core.typing.buildSubstMap

import scala.collection.Seq

extension (clauses: Seq[Clause[Term]]) {
  /**
   * Try to match the given arguments with the clause set.
   * Return the body of the first matching clause.
   */
  def tryMatch(args: Seq[Term]): Option[Term] = {
    clauses.collectFirst { clause =>
      val substOpt = clause.patterns.buildSubstMap(args)
      substOpt.map(subst => clause.body.subst(subst))
    }.flatten
  }
}

/**
 * A clause (case) in a pattern matching expression.
 */
case class Clause[T](patterns: Seq[Pattern[T]], body: T) {
  def map[U](f: T => U): Clause[U] = Clause(patterns.map(_.map(f)), f(body))

  override def toString: String = s"${patterns.mkString(", ")} => $body"
}
