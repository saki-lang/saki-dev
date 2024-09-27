package saki.core

object Elaborate {

  case class Context(
    definitions: Map[Var.Defined[?], Definition],
    locals: Map[Var.Local, Type],
  )

  def elaborate(expr: Expr, expected: Type)(implicit ctx: Context): Term = ???

  case class Synth(term: Term, `type`: Type) {
    def unpack: (Term, Type) = (term, `type`)
  }

  def synth(expr: Expr)(implicit ctx: Context): Synth = ???

}

extension (self: Expr) {
  def synth(implicit ctx: Elaborate.Context): Elaborate.Synth = Elaborate.synth(self)
  def infer(implicit ctx: Elaborate.Context, expected: Type): Type = Elaborate.elaborate(self, expected)
}
