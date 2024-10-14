package saki.concrete

object Prelude {

  import saki.concrete.SpineParser.*

  private object Operators {
    val add: Operator.Binary = Operator.Binary("+", Associativity.Left)
    val sub: Operator.Binary = Operator.Binary("-", Associativity.Left, sameAs = Set(add))
    val mul: Operator.Binary = Operator.Binary("*", Associativity.Left, tighterThan = Set(add, sub))
    val div: Operator.Binary = Operator.Binary("/", Associativity.Left, tighterThan = Set(add, sub), sameAs = Set(mul))
    val mod: Operator.Binary = Operator.Binary("%", Associativity.Left, tighterThan = Set(add, sub), sameAs = Set(mul, div))
    val pow: Operator.Binary = Operator.Binary("**", Associativity.Right, tighterThan = Set(add, sub, mul, div, mod))

    val arithmAnd: Operator.Binary = Operator.Binary("&", Associativity.Left, looserThan = Set(add, sub, mul, div, mod))
    val arithmOr: Operator.Binary = Operator.Binary("|", Associativity.Left, looserThan = Set(arithmAnd))
    val arithmXor: Operator.Binary = Operator.Binary("^", Associativity.Left, tighterThan = Set(arithmOr), looserThan = Set(arithmAnd))

    val arithmBinaryOperators: Set[Operator.Binary] = Set(add, sub, mul, div, mod, arithmAnd, arithmOr, arithmXor)

    val eq: Operator.Binary = Operator.Binary("==", Associativity.Left, looserThan = arithmBinaryOperators)
    val ne: Operator.Binary = Operator.Binary("!=", Associativity.Left, looserThan = arithmBinaryOperators, sameAs = Set(eq))
    val lt: Operator.Binary = Operator.Binary("<", Associativity.Left, looserThan = arithmBinaryOperators, sameAs = Set(eq, ne))
    val le: Operator.Binary = Operator.Binary("<=", Associativity.Left, looserThan = arithmBinaryOperators, sameAs = Set(eq, ne, lt))
    val gt: Operator.Binary = Operator.Binary(">", Associativity.Left, looserThan = arithmBinaryOperators, sameAs = Set(eq, ne, lt, le))
    val ge: Operator.Binary = Operator.Binary(">=", Associativity.Left, looserThan = arithmBinaryOperators, sameAs = Set(eq, ne, lt, le, gt))

    val cmpBinaryOperators: Set[Operator.Binary] = Set(eq, ne, lt, le, gt, ge)

    val logicalAnd: Operator.Binary = Operator.Binary("&&", Associativity.Left, looserThan = cmpBinaryOperators)
    val logicalOr: Operator.Binary = Operator.Binary("||", Associativity.Left, looserThan = Set(logicalAnd))

    val stringConcat: Operator.Binary = Operator.Binary("++", Associativity.Left, looserThan = Set(logicalAnd, logicalOr))

    // Unary operators

    val neg: Operator.Unary = Operator.Unary("--", UnaryType.Prefix)
    val logicalNot: Operator.Unary = Operator.Unary("!", UnaryType.Prefix)
  }

  lazy val operators: Seq[SpineParser.Operator] = Seq(
    // Binary
    Operators.add, Operators.sub, Operators.mul, Operators.div, Operators.mod, Operators.pow,
    Operators.arithmAnd, Operators.arithmOr, Operators.arithmXor,
    Operators.eq, Operators.ne, Operators.lt, Operators.le, Operators.gt, Operators.ge,
    Operators.logicalAnd, Operators.logicalOr,
    Operators.stringConcat,
    // Unary
    Operators.neg, Operators.logicalNot
  )
}
