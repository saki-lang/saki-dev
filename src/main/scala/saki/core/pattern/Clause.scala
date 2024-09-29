package saki.core.pattern

import saki.core.{Expr, Term, Var}

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
case class Clause[T](patterns: Seq[Pattern], body: T)

case class UnresolvedClause(patterns: Seq[Pattern.Unresolved], body: Expr) {

  def resolve(implicit env: Map[String, Var]): Clause[Expr] = {
    val (resolvedPatterns, ctx) = this.patterns.foldLeft((List.empty[Pattern], ResolvingContext(env))) {
      case ((resolvedPatterns, context), pattern) => {
        val (resolved, newCtx) = pattern.resolve(context)
        (resolvedPatterns :+ resolved, newCtx)
      }
    }
    val body = this.body.resolve(ctx)
    Clause(resolvedPatterns, body)
  }

}
