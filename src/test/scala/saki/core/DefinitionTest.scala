package saki.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.*
import saki.cli.compileModule

class DefinitionTest extends AnyFlatSpec with should.Matchers with SakiTestExt {
  it should "" in {
    val code = {
      """
        type Nat = inductive {
            Zero
            Succ(Nat)
        }

        def plus(a b : Nat): Nat = match a {
            case Nat::Zero => b
            case Nat::Succ(a') => Nat::Succ(plus(a', b))
        }

        def fib(n : Nat): Nat = match n {
            case Nat::Zero => Nat::Zero
            case Nat::Succ(Nat::Zero) => Nat::Succ(Nat::Zero)
            case Nat::Succ(Nat::Succ(n')) => plus(fib(n'), fib(Nat::Succ(n')))
        }

        def n0: Nat = Nat::Zero
        def n1: Nat = Nat::Succ(n0)
        def n2: Nat = Nat::Succ(n1)
        def n3: Nat = Nat::Succ(n2)
        def n4: Nat = Nat::Succ(n3)
        def n5: Nat = Nat::Succ(n4)
        def n6: Nat = Nat::Succ(n5)
        def n7: Nat = Nat::Succ(n6)
        def n8: Nat = Nat::Succ(n7)
        def n9: Nat = Nat::Succ(n8)
      """
    }
    val module = compileModule(code)

    // Test add: 3 + 4 = 7
    module.eval(parseExpr("plus(n3, n4)")) should be (module.eval(parseExpr("n7")))

    // Test add: 0 + n = n
    module.eval(parseExpr("plus(n0, n5)")) should be (module.eval(parseExpr("n5")))
    module.eval(parseExpr("plus(n0, n0)")) should be (module.eval(parseExpr("n0")))

    // Test add: n + 0 = n
    module.eval(parseExpr("plus(n6, n0)")) should be (module.eval(parseExpr("n6")))

    // Commutativity of addition: 2 + 3 = 3 + 2
    module.eval(parseExpr("plus(n2, n3)")) should be (module.eval(parseExpr("plus(n3, n2)")))

    // Test specific edge case: fibonacci of 0
    module.eval(parseExpr("fib(n0)")) should be (module.eval(parseExpr("n0")))

    // Test fibonacci: fib(1) = 1, fib(2) = 1, fib(3) = 2, fib(4) = 3, fib(5) = 5, fib(6) = 8
    module.eval(parseExpr("fib(n1)")) should be(module.eval(parseExpr("n1")))
    module.eval(parseExpr("fib(n2)")) should be(module.eval(parseExpr("n1")))
    module.eval(parseExpr("fib(n3)")) should be(module.eval(parseExpr("n2")))
    module.eval(parseExpr("fib(n4)")) should be(module.eval(parseExpr("n3")))
    module.eval(parseExpr("fib(n5)")) should be(module.eval(parseExpr("n5")))
    module.eval(parseExpr("fib(n6)")) should be(module.eval(parseExpr("n8")))
  }

}
