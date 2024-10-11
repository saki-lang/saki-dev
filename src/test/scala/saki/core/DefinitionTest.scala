package saki.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.*
import saki.cli.compileModule

class DefinitionTest extends AnyFlatSpec with should.Matchers with SakiTestExt {
  it should "fibonacci" in {
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
    module.eval("plus(n3, n4)") should be (module.eval("n7"))

    // Test add: 0 + n = n
    module.eval("plus(n0, n5)") should be (module.eval("n5"))
    module.eval("plus(n0, n0)") should be (module.eval("n0"))

    // Test add: n + 0 = n
    module.eval("plus(n6, n0)") should be (module.eval("n6"))

    // Commutativity of addition: 2 + 3 = 3 + 2
    module.eval("plus(n2, n3)") should be (module.eval("plus(n3, n2)"))

    // Test specific edge case: fibonacci of 0
    module.eval("fib(n0)") should be (module.eval("n0"))

    // Test fibonacci: fib(1) = 1, fib(2) = 1, fib(3) = 2, fib(4) = 3, fib(5) = 5, fib(6) = 8
    module.eval("fib(n1)") should be (module.eval("n1"))
    module.eval("fib(n2)") should be (module.eval("n1"))
    module.eval("fib(n3)") should be (module.eval("n2"))
    module.eval("fib(n4)") should be (module.eval("n3"))
    module.eval("fib(n5)") should be (module.eval("n5"))
    module.eval("fib(n6)") should be (module.eval("n8"))
  }

  it should "mutual recursive" in {
    val code = {
      """
        type Nat = inductive {
            Zero
            Succ(Nat)
        }

        def isEven(n: Nat): Bool = match n {
            case Nat::Zero => true
            case Nat::Succ(n') => isOdd(n')
        }

        def isOdd(n: Nat): Bool = match n {
            case Nat::Zero => false
            case Nat::Succ(n') => isEven(n')
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

    module.eval("isEven(n3)") should be (module.eval("false"))
    module.eval("isEven(n4)") should be (module.eval("true"))
    module.eval("isEven(n5)") should be (module.eval("false"))
    module.eval("isEven(n6)") should be (module.eval("true"))
    module.eval("isEven(n7)") should be (module.eval("false"))
    module.eval("isEven(n8)") should be (module.eval("true"))
    module.eval("isEven(n9)") should be (module.eval("false"))

    module.eval("isOdd(n3)") should be (module.eval("true"))
    module.eval("isOdd(n4)") should be (module.eval("false"))
    module.eval("isOdd(n5)") should be (module.eval("true"))
    module.eval("isOdd(n6)") should be (module.eval("false"))
    module.eval("isOdd(n7)") should be (module.eval("true"))
    module.eval("isOdd(n8)") should be (module.eval("false"))
    module.eval("isOdd(n9)") should be (module.eval("true"))
  }

  it should "eq refl" in {
    val code = {
      """
        def eq(A: 'Type, a b: A): 'Type = ∀(P: A -> 'Type) -> P(a) -> P(b)

        def refl(A: 'Type, a: A): eq(A, a, a) = {
            (P: A -> 'Type, pa: P(a)) => pa
        }

        def symmetry(A: 'Type, a b: A, e: eq(A, a, b)): eq(A, b, a) = {
            e((b: A) => eq(A, b, a), refl(A, a))
        }
      """
    }
    compileModule(code)
  }

  it should "option" in {
    val code = {
      """
        type Option[A: 'Type] = inductive {
            None
            Some(A)
        }

        def map[A: 'Type, B: 'Type](option: Option[A], transform: A -> B): Option[B] = {
            match option {
                case Option[A]::None => Option[B]::None
                case Option[A]::Some(value) => Option[B]::Some(transform(value))
            }
        }
      """
    }
    val module = compileModule(code)
  }

}
