package saki.core

object Unify {
  def unify(value1: Term, value2: Term): Boolean = ???
}

extension (self: Term) {
  def unify(that: Term): Boolean = Unify.unify(self, that)
  def eta(lambda: Term.Lambda): Boolean = {
    Unify.unify(lambda.body, self.apply(Term.Ref(lambda.param)))
  }
}
