package saki.concrete

object Prelude {

  import saki.concrete.SpineParser.*

  private object Operators {
    val add: Operator.Binary = Operator.Binary("+", Associativity.Left)
    val sub: Operator.Binary = Operator.Binary("-", Associativity.Left, sameAs = Set(add))
    val mul: Operator.Binary = Operator.Binary("*", Associativity.Left, tighterThan = Set(add, sub))
    val div: Operator.Binary = Operator.Binary("/", Associativity.Left, tighterThan = Set(add, sub), sameAs = Set(mul))
  }

  lazy val operators: Seq[SpineParser.Operator] = Seq(
    Operators.add, Operators.sub, Operators.mul, Operators.div
  )
}
