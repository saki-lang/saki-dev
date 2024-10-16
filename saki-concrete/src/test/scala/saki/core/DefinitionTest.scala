package saki.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.*
import saki.core.compileModule

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

  it should "peano to int" in {
    val code = {
      """
        type Nat = inductive {
            Zero
            Succ(Nat)
        }

        def toInt(n: Nat): Int = match n {
            case Nat::Zero => 0
            case Nat::Succ(n') => toInt(n') + 1
        }

        def toPeano(n: Int): Nat = {
            if n == 0 then Nat::Zero
            else Nat::Succ(toPeano(n - 1))
        }
      """
    }
    val module = compileModule(code)
    module.eval("Nat::Zero.toInt") should be (module.eval("0"))
    module.eval("Nat::Succ(Nat::Zero).toInt") should be (module.eval("1"))
    module.eval("Nat::Succ(Nat::Succ(Nat::Zero)).toInt") should be (module.eval("2"))
    module.eval("Nat::Succ(Nat::Succ(Nat::Succ(Nat::Zero))).toInt") should be (module.eval("3"))

    module.eval("0.toPeano") should be (module.eval("Nat::Zero"))
    module.eval("1.toPeano") should be (module.eval("Nat::Succ(Nat::Zero)"))
    module.eval("2.toPeano") should be (module.eval("Nat::Succ(Nat::Succ(Nat::Zero))"))
    module.eval("3.toPeano") should be (module.eval("Nat::Succ(Nat::Succ(Nat::Succ(Nat::Zero)))"))
  }

  it should "is prime" in {
    val code = {
      """
        def checkDivisors(value: Int, divisor: Int): Bool = {
            if divisor * divisor > value then true
            else if value % divisor == 0 then false
            else checkDivisors(value, divisor + 1)
        }

        def isPrime(n: Int): Bool = {
            if n <= 1 then false      // 0 and 1 are not prime numbers
            else if n == 2 then true  // 2 is a prime number
            else checkDivisors(n, 2)  // Start checking from 2
        }
      """
    }
    val module = compileModule(code)

    module.eval("isPrime(0)") should be (module.eval("false"))
    module.eval("isPrime(1)") should be (module.eval("false"))
    module.eval("isPrime(2)") should be (module.eval("true"))
    module.eval("isPrime(3)") should be (module.eval("true"))
    module.eval("isPrime(4)") should be (module.eval("false"))

    module.eval("isPrime(97)") should be (module.eval("true"))
    module.eval("isPrime(98)") should be (module.eval("false"))
    module.eval("isPrime(101)") should be (module.eval("true"))
    module.eval("isPrime(143)") should be (module.eval("false"))
    module.eval("isPrime(163)") should be (module.eval("true"))
    module.eval("isPrime(211)") should be (module.eval("true"))
    module.eval("isPrime(221)") should be (module.eval("false"))
    module.eval("isPrime(233)") should be (module.eval("true"))
    module.eval("isPrime(263)") should be (module.eval("true"))

    module.eval("isPrime(5389)") should be (module.eval("false"))
    module.eval("isPrime(6221)") should be (module.eval("true"))
    module.eval("isPrime(8633)") should be (module.eval("false"))

    // Need optimization, the following numbers will cause stack overflow in scalatest environment
    // module.eval("isPrime(131071)") should be (module.eval("true"))
    // module.eval("isPrime(180469)") should be (module.eval("false"))
  }

  it should "different style fn call" in {
    val code = {
      """
        def square(n: Int): Int = n * n

        def add(a b: Int): Int = a + b

        def powerMod(base exponent mod: Int): Int = {
            if exponent == 0 then 1
            else if exponent % 2 == 0 then {
                let half = powerMod(base, exponent / 2, mod)
                (half * half) % mod
            } else {
                (base * powerMod(base, exponent - 1, mod)) % mod
            }
        }
      """
    }
    val module = compileModule(code)

    // 12996: 12 months a year, work from 9:00 am to 9:00 pm, 6 days per week
    module.eval("square(114)") should be (module.eval("12996"))
    module.eval("114.square") should be (module.eval("12996"))
    module.eval("square 114") should be (module.eval("12996"))

    module.eval("add(98)(527)") should be (module.eval("625"))
    module.eval("98.add(527)") should be (module.eval("625"))
    module.eval("add 98 527") should be (module.eval("625"))

    module.eval("powerMod(1926, 8, 17)") should be (module.eval("16"))
    module.eval("1926.powerMod(8, 17)") should be (module.eval("16"))
    module.eval("powerMod 1926 8 17") should be (module.eval("16"))
  }

  it should "GADT explicit type param" in {
    val code = {
      """
        type Option(T: 'Type) = inductive {
            None
            Some(T)
        }

        def map(A: 'Type, B: 'Type, option: Option(A), transform: A -> B): Option(B) = {
            match option {
                case Option(A)::None => Option(B)::None
                case Option(A)::Some(value) => Option(B)::Some(transform(value))
            }
        }
      """
    }
    val module = compileModule(code)
    module.eval("map(Int, Bool, Option(Int)::Some(514 - 114), (n: Int) => n > 0)") should be (module.eval("Option(Bool)::Some(true)"))
    module.eval("map(Int, Bool, Option(Int)::None, (n: Int) => n > 0)") should be (module.eval("Option(Bool)::None"))
    module.eval("map(Int, Bool, Option(Int)::Some(114 - 514), (n: Int) => n > 0)") should be (module.eval("Option(Bool)::Some(false)"))
    module.eval("map(Int, String, Option(Int)::Some(114514), (n: Int) => n.toString)") should be (module.eval("Option(String)::Some(\"114514\")"))
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
    module.eval("""{
      let option = Option[Int]::Some(19260817)
      option.map[Int, String]((n: Int) => n.toString)
    }""") should be (module.eval("Option(String)::Some(\"19260817\")"))
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

    module.eval("isEven(n3)") should be(module.eval("false"))
    module.eval("isEven(n4)") should be(module.eval("true"))
    module.eval("isEven(n5)") should be(module.eval("false"))
    module.eval("isEven(n6)") should be(module.eval("true"))
    module.eval("isEven(n7)") should be(module.eval("false"))
    module.eval("isEven(n8)") should be(module.eval("true"))
    module.eval("isEven(n9)") should be(module.eval("false"))

    module.eval("isOdd(n3)") should be(module.eval("true"))
    module.eval("isOdd(n4)") should be(module.eval("false"))
    module.eval("isOdd(n5)") should be(module.eval("true"))
    module.eval("isOdd(n6)") should be(module.eval("false"))
    module.eval("isOdd(n7)") should be(module.eval("true"))
    module.eval("isOdd(n8)") should be(module.eval("false"))
    module.eval("isOdd(n9)") should be(module.eval("true"))
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

}
