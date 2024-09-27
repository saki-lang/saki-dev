package saki.core

enum Pattern {
  case Unresolved(name: String, patterns: Seq[Unresolved])
  case Primitive(value: Literal)
  case Bind(binding: Var.Local)
  case Cons(cons: Var.Defined[Definition.Constructor], patterns: Seq[Pattern])
  case Clause[T](patterns: Seq[Pattern], body: T)
  case ClauseSet[T](clauses: Seq[Clause[T]])
  case UnresolvedClause(patterns: Seq[Unresolved], body: Expr)
}

object PatternMatching {
  def unfold(clauses: Pattern.ClauseSet[Term], args: Seq[Term]): Option[Term] = ???
}
