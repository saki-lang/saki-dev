package saki.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.*
import saki.core.compileModule

class DefinitionTest extends AnyFunSuite with should.Matchers with SakiTestExt {
  test("fibonacci") {
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

  test("peano to int") {
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

  test("is prime") {
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

  test("different style fn call") {
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

  test("GADT explicit type param") {
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

  test("overloaded 1") {
    val code = {
      """
        def add(a b: Int): Int = a + b
        def add(a b: String): String = a ++ b
      """
    }
    val module = compileModule(code)
    module.eval("add(114, 514)") should be (module.eval("628"))
    module.eval("add(\"It's \", \"mygo!!!!!\")") should be (module.eval("\"It's mygo!!!!!\""))
  }

  test("overloaded 2") {
    val code = {
      """
        def ceilDec(x: Int): Int = if x == 0 then 1 else 10 * ceilDec(x / 10)
        def concat(a: Int, b: Int): Int = a * ceilDec(b) + b
        def concat(a b: String): String = a ++ b
      """
    }
    val module = compileModule(code)
    module.eval("concat(123, 456)") should be (module.eval("123456"))
    module.eval("concat(\"It's \", \"mygo!!!!!\")") should be (module.eval("\"It's mygo!!!!!\""))
  }

  test("sum type") {
    val code = {
      """
        def describeValue(value: Bool | ℤ | String): String = match value {
            case true => "It's true!"
            case false => "It's false!"
            case n: ℤ => "It's an integer: " ++ n.toString ++ "!"
            case s: String => "It's " ++ s ++ "!!!!!"
        }
      """
    }
    val module = compileModule(code)
    module.eval("describeValue(true)") should be (module.eval("\"It's true!\""))
    module.eval("describeValue(false)") should be (module.eval("\"It's false!\""))
    module.eval("describeValue(114)") should be (module.eval("\"It's an integer: 114!\""))
    module.eval("describeValue(\"mygo\")") should be (module.eval("\"It's mygo!!!!!\""))
  }

  test("superposition type") {
    val code = {
      """
        def appendToString(s: String, x: Int): String = s ++ x.toString
        def appendToString(s: String, x: String): String = s ++ x
        eval appendToString "foo"
      """
    }
    compileModule(code)
  }

  test("mutual recursive") {
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

  test("inductive mutual recursive") {
    val code = {
      """
        type Expr = inductive {
            Var(String)
            // Π(x : A). B
            Pi(String, Expr, Expr)
            // λ(x : A). t
            Lambda(String, Expr, Expr)
            // f a
            Apply(Expr, Expr)
        }

        type Value = inductive {
            Neutral(NeutralValue)
            Type(Int)
            Lambda(Value, Value -> Value)
            Pi(Value, Value -> Value)
        }

        type Type = Value

        type NeutralValue = inductive {
            Var(String)
            Apply(NeutralValue, Value)
        }
      """
    }
    compileModule(code)
  }

  test("overloaded mutual recursive") {
    val code = {
      """
        type Value = inductive {
            Neutral(NeutralValue)
            Type(Int)
        }

        type NeutralValue = inductive {
            Var(String)
            Apply(NeutralValue, Value)
        }

        // Overloading for `NeutralValue`
        def toString(neutral: NeutralValue): String = match neutral {
            case NeutralValue::Var(name) => name
            // Here `fn` is a `NeutralValue` and `arg` is a `Value`
            // Thus, the `toString` functions we are calling here are two different overloadings
            case NeutralValue::Apply(fn, arg) => "app(" ++ fn.toString ++ ", " ++ arg.toString ++ ")"
        }

        // Overloading for `Value`
        def toString(value: Value): String = match value {
            case Value::Neutral(neutral) => neutral.toString
            case Value::Type(univ) => univ.toString
        }
      """
    }
    compileModule(code)
  }

  test("overloaded mutual recursive with other modules") {
    val code = {
      """
        type Expr = inductive {
            Var(String)
            Type(Int)
            Pi(String, Expr, Expr)
            Lambda(String, Expr, Expr)
            Apply(Expr, Expr)
        }

        def toString(expr: Expr): String = match expr {
            case Expr::Var(ident) => ident
            case Expr::Type(level) => "Type_" ++ level.toString
            case Expr::Pi(ident, ty, codomain) => "(" ++ ident ++ " : " ++ ty.toString ++ ")" ++ "→" ++ codomain.toString
            case Expr::Lambda(ident, ty, body) => "λ(" ++ ident ++ " : " ++ ty.toString ++ ")" ++ "→" ++ body.toString
            case Expr::Apply(fn, arg) => fn.toAtomicString ++ arg.toAtomicString
        }

        def toAtomicString(expr: Expr): String = match expr {
            case Expr::Var(ident) => ident
            case Expr::Type(level) => "Type_" ++ level.toString
            case _ => "(" ++ expr.toString ++ ")"
        }
      """
    }
    compileModule(code)
  }

  test("eq refl") {
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

  test("iota reduction") {
    val code = {
      """
        type ℕ = inductive {
            Zero
            Succ(ℕ)
        }

        def add(a b : ℕ): ℕ = match a {
            case ℕ::Zero => b
            case ℕ::Succ(a') => ℕ::Succ(add a' b)
        }
      """
    }
    val module = compileModule(code)
    module.eval(
      "(a: ℕ) => (b: ℕ) => add(ℕ::Succ(a), b)"
    ) should be (module.eval(
      "(a: ℕ) => (b: ℕ) => ℕ::Succ(add(a, b))"
    ))
  }

  test("nested function calls and partial application") {
    val code = {
      """
        def multiply(a b: Int): Int = a * b
        def subtract(a b: Int): Int = a - b
        def compose(f: Int -> Int, g: Int -> Int): Int -> Int = (x: Int) => f(g(x))
      """
    }
    val module = compileModule(code)

    module.eval("multiply(3, subtract(10, 7))") should be(module.eval("9"))
    module.eval("subtract(15)(multiply(2, 4))") should be(module.eval("7"))
    module.eval("compose(multiply(2), subtract(5))(3)") should be(module.eval("4"))
  }

  test("recursive function call with different styles") {
    val code = {
      """
        def factorial(n: Int): Int = {
            if n == 0 then 1
            else n * factorial(n - 1)
        }
      """
    }
    val module = compileModule(code)

    module.eval("factorial(5)") should be(module.eval("120"))
    module.eval("5.factorial") should be(module.eval("120"))
    module.eval("factorial 5") should be(module.eval("120"))
  }

  test("higher order function") {
    val code = {
      """
        def twice(f: Int -> Int, x: Int): Int = f(f(x))
        def increment(n: Int): Int = n + 1
      """
    }
    val module = compileModule(code)

    module.eval("twice(increment, 5)") should be(module.eval("7"))
    module.eval("increment.twice 5") should be(module.eval("7"))
    module.eval("twice increment 5") should be(module.eval("7"))
  }

  test("using GADT with map and pattern matching") {
    val code = {
      """
        type Either(L: 'Type, R: 'Type) = inductive {
            Left(L)
            Right(R)
        }

        def mapEither(L: 'Type, R: 'Type, S: 'Type, either: Either(L, R), transform: R -> S): Either(L, S) = {
            match either {
                case Either(L, R)::Left(value) => Either(L, S)::Left(value)
                case Either(L, R)::Right(value) => Either(L, S)::Right(transform(value))
            }
        }
      """
    }
    val module = compileModule(code)

    module.eval("mapEither(Int, String, Bool, Either(Int, String)::Right(\"hello\"), (s: String) => s == \"hello\")") should be(module.eval("Either(Int, Bool)::Right(true)"))
    module.eval("mapEither(Int, String, Bool, Either(Int, String)::Left(42), (s: String) => s == \"hello\")") should be(module.eval("Either(Int, Bool)::Left(42)"))
  }

  test("higher kinded type and mapping") {
    val code = {
      """
        type List(T: 'Type) = inductive {
            Nil
            Cons(T, List(T))
        }

        def mapList(A: 'Type, B: 'Type, list: List(A), transform: A -> B): List(B) = {
            match list {
                case List(A)::Nil => List(B)::Nil
                case List(A)::Cons(head, tail) => List(B)::Cons(transform(head), mapList(A, B, tail, transform))
            }
        }
      """
    }
    val module = compileModule(code)

    module.eval("mapList(Int, String, List(Int)::Cons(1, List(Int)::Cons(2, List(Int)::Nil)), (n: Int) => n.toString)") should be(module.eval("List(String)::Cons(\"1\", List(String)::Cons(\"2\", List(String)::Nil))"))
    module.eval("mapList(Int, Bool, List(Int)::Nil, (n: Int) => n > 0)") should be(module.eval("List(Bool)::Nil"))
  }

  test("lambda apply different style") {
    val code = {
      """
        type List(T: 'Type) = inductive {
            Nil
            Cons(T, List(T))
        }

        def fold[A: 'Type, B: 'Type](list: List(A), initial: B, combine: B -> A -> B): B = {
            match list {
                case List(A)::Nil => initial
                case List(A)::Cons(head, tail) => tail.fold[A, B](combine(initial, head), combine)
            }
        }

        def sum(list: List(Int)): Int = list.fold[Int, Int](0) |lhs: Int, rhs: Int| { lhs + rhs }
      """
    }
    val module = compileModule(code)
    module.eval("sum(List(Int)::Cons(1, List(Int)::Cons(2, List(Int)::Cons(3, List(Int)::Nil))))") should be(module.eval("6"))
  }

  test("overloaded function with different types") {
    val code = {
      """
          def compare(a b: Int): String = if a > b then "greater" else if a < b then "less" else "equal"
          def compare(a b: String): String = if a.length > b.length then "longer" else if a.length < b.length then "shorter" else "same length"
        """
    }
    val module = compileModule(code)

    module.eval("compare(10, 5)") should be(module.eval("\"greater\""))
    module.eval("compare(5, 10)") should be(module.eval("\"less\""))
    module.eval("compare(10, 10)") should be(module.eval("\"equal\""))
    module.eval("compare(\"hello\", \"world\")") should be(module.eval("\"same length\""))
    module.eval("compare(\"hi\", \"world\")") should be(module.eval("\"shorter\""))
    module.eval("compare(\"hello\", \"hi\")") should be(module.eval("\"longer\""))
  }

  test("pattern matching with sum type containing more variants") {
    val code = {
      """
          type Shape = inductive {
              Circle(Int)
              Rectangle(Int, Int)
              Square(Int)
          }

          def area(shape: Shape): Int = match shape {
              case Shape::Circle(radius) => 3 * radius * radius
              case Shape::Rectangle(width, height) => width * height
              case Shape::Square(side) => side * side
          }
        """
    }
    val module = compileModule(code)

    module.eval("area(Shape::Circle(5))") should be(module.eval("75"))
    module.eval("area(Shape::Rectangle(4, 5))") should be(module.eval("20"))
    module.eval("area(Shape::Square(3))") should be(module.eval("9"))
  }

  test("proof: a + b = b + a") {
    val code = {
      """
        def Eq(A: 'Type, a b: A): 'Type = ∀(P: A -> 'Type) -> P(a) -> P(b)

        def refl(A: 'Type, a: A): A.Eq(a, a) = {
            (P: A -> 'Type, pa: P(a)) => pa
        }

        def symmetry(A: 'Type, a b: A, eqab: A.Eq(a, b)): A.Eq(b, a) = {
            eqab((b': A) => A.Eq(b', a), A.refl(a))
        }

        type ℕ = inductive {
            Zero
            Succ(ℕ)
        }

        def o: ℕ = ℕ::Zero
        def succ(n: ℕ): ℕ = ℕ::Succ(n)

        operator binary (===) left-assoc {
            looser-than (+)
        }

        def (===)(a b: ℕ): 'Type = ℕ.Eq(a, b)

        def (+)(a b : ℕ): ℕ = match a {
            case ℕ::Zero => b
            case ℕ::Succ(a') => ℕ::Succ(a' + b)
        }

        def induction(
            P: ℕ -> 'Type,
            base: P(ℕ::Zero),
            induce: ∀(n: ℕ) -> P(n) -> P(n.succ),
            nat: ℕ,
        ): P(nat) = match nat {
            case ℕ::Zero => base
            case ℕ::Succ(n') => induce(n', P.induction(base, induce, n'))
        }

        def inductionReduce(
            a b: ℕ,
            eqba: (b === a),
            P: ℕ -> 'Type,
            pa: P(a),
        ): P(b) = {
            let eqab = ℕ.symmetry(b, a, eqba)
            eqab(P, pa)
        }

        def theoremPlusZero: ∀(n: ℕ) -> (n + o === n) = {
            ((n: ℕ) => (n + o === n)).induction(
                ℕ.refl(ℕ::Zero),
                (n: ℕ, assumption: (n + o === n)) => {
                    inductionReduce(
                        n, n + o, assumption,
                        (n': ℕ) => (n'.succ === n.succ),
                        ℕ.refl(n.succ)
                    )
                }
            )
        }

        def leibnizEq(f: ℕ -> ℕ): ∀(a: ℕ) -> ∀(b: ℕ) -> (a === b) -> (f(a) === f(b)) = {
            (a b : ℕ, eqab: (a === b)) => {
                (P: ℕ -> 'Type, pfa: P(f a)) => eqab((b': ℕ) => P(f b'), pfa)
            }
        }

        def theoremPlusZeroInv: ∀(n: ℕ) -> (n === n + o) = {
            (n: ℕ) => ℕ.symmetry(n + o, n, theoremPlusZero(n))
        }

        def theoremPlusSucc: ∀(a: ℕ) -> ∀(b: ℕ) -> (succ(a + b) === a + b.succ) = {
            (a b : ℕ) => induction(
                (a': ℕ) => (succ(a' + b) === a' + b.succ),
                ℕ.refl(succ(o + b)),
                (a': ℕ, assumption: (succ(a' + b) === a' + b.succ)) => {
                    leibnizEq(succ, succ(a' + b), a' + b.succ, assumption)
                }, a
            )
        }

        def transitivity(A: 'Type, a b c: A, eqab: A.Eq(a, b), eqbc: A.Eq(b, c)): A.Eq(a, c) = {
            (P: A -> 'Type, pa: P(a)) => eqbc(P, eqab(P, pa))
        }

        def theoremPlusComm: ∀(a: ℕ) -> ∀(b: ℕ) -> (a + b === b + a) = {
            (a: ℕ, b: ℕ) => induction(
                (a': ℕ) => (a' + b === b + a'),
                theoremPlusZeroInv b,
                (a': ℕ, IH: (a' + b === b + a')) => {
                    let eq1 = ℕ.refl(succ(a' + b))                  // succ(a') + b === succ(a' + b)
                    let eq2 = leibnizEq(succ, a' + b, b + a', IH)   // succ(a' + b) === succ(b + a')
                    let eq3 = theoremPlusSucc(b, a')                // succ(b + a') === b + succ(a')
                    let eq4 = transitivity(ℕ, succ(a' + b), succ(b + a'), b + succ(a'), eq2, eq3)
                    transitivity(ℕ, succ(a') + b, succ(a' + b), b + succ(a'), eq1, eq4)
                }, a
            )
        }
      """
    }
    compileModule(code)
  }

  test("operator declaration") {
    val code = {
      """
        operator binary (#) left-assoc
        def (#)(a b: Int): Int = a + b
        def add(a b: Int): Int = a # b
      """
    }
    val module = compileModule(code)
    module.eval("add 114 514") should be (module.eval("628"))
  }

  test("rbtree") {
    val code = {
      """
        type Option[A: 'Type] = inductive {
            None
            Some(A)
        }

        // Definition of possible colors in a Red-Black Tree
        // Red or Black color to maintain tree properties
        type Color = inductive {
            Red
            Black
        }

        // Definition of the Red-Black Tree data structure
        // Tree can be a Leaf or a Node with color, value, left and right children
        type Tree = inductive {
            Leaf
            Node(Color, Int, Tree, Tree)
        }

        // Function to balance the Red-Black Tree after insertion
        // Ensures that Red-Black Tree properties are maintained, such as balancing after consecutive red nodes
        def balance(tree: Tree): Tree = match tree {
            // Left-Left case (left subtree of left child is red)
            // Perform a right rotation to balance the tree
            // This situation occurs when the left child and its left child are both red, causing a violation
            case Tree::Node(
                Color::Black, valueRight,
                Tree::Node(
                    Color::Red, valueTop,
                    Tree::Node(Color::Red, valueLeft, leftLeft, leftRight),
                    rightLeft
                ), rightRight
            ) | Tree::Node(
                Color::Black, valueRight,
                Tree::Node(
                    Color::Red, valueLeft,
                    leftLeft,
                    Tree::Node(Color::Red, valueTop, leftRight, rightLeft)
                ), rightRight
            ) => Tree::Node(
                Color::Red, valueTop,
                Tree::Node(Color::Black, valueLeft, leftLeft, leftRight),
                Tree::Node(Color::Black, valueRight, rightLeft, rightRight)
            )

            // Right-Right case (right subtree of right child is red)
            // Perform a left rotation to balance the tree
            // This situation occurs when the right child and its right child are both red, causing a violation
            case Tree::Node(
                Color::Black, valueLeft,
                leftLeft,
                Tree::Node(
                    Color::Red, valueRight,
                    Tree::Node(Color::Red, valueTop, leftRight, rightLeft),
                    rightRight
                )
            ) | Tree::Node(
                Color::Black, valueLeft,
                leftLeft,
                Tree::Node(
                    Color::Red, valueTop,
                    leftRight,
                    Tree::Node(Color::Red, valueRight, rightLeft, rightRight)
                )
            ) => Tree::Node(
                Color::Red, valueTop,
                Tree::Node(Color::Black, valueLeft, leftLeft, leftRight),
                Tree::Node(Color::Black, valueRight, rightLeft, rightRight)
            )

            // Recoloring case: both children are red
            // Recolor the children to black and maintain the parent as red
            // This occurs to fix the situation where both children of a red node are also red
            case Tree::Node(
                Color::Red, value,
                Tree::Node(Color::Red, leftValue, leftLeft, leftRight),
                Tree::Node(Color::Red, rightValue, rightLeft, rightRight)
            ) => Tree::Node(
                Color::Red, value,
                Tree::Node(Color::Black, leftValue, leftLeft, leftRight),
                Tree::Node(Color::Black, rightValue, rightLeft, rightRight)
            )

            // Other cases: no need to balance
            case node => node
        }

        // Insert a new value into the Red-Black Tree as a red node
        // Recursively inserts the new value and then balances the tree if necessary
        def insertRed(tree: Tree, newValue: Int): Tree = match tree {
            case Tree::Leaf => Tree::Node(Color::Red, newValue, Tree::Leaf, Tree::Leaf)
            case Tree::Node(color, value, left, right) => if newValue < value then {
                Tree::Node(color, value, left.insertRed(newValue), right).balance
            } else if newValue > value then {
                Tree::Node(color, value, left, right.insertRed(newValue)).balance
            } else {
                Tree::Node(color, newValue, left, right)
            }
        }

        // insert a value into the Red-Black Tree
        // Ensures that the root of the tree is always black after insertion
        def insert(tree: Tree, value: Int): Tree = match tree.insertRed(value) {
            case Tree::Node(Color::Red, value, left, right) => Tree::Node(Color::Black, value, left, right)
            case Tree::Node(Color::Black, value, left, right) => Tree::Node(Color::Black, value, left, right)
            case Tree::Leaf => Tree::Leaf   // Should not happen
        }

        def find(tree: Tree, target: Int): Option[Int] = match tree {
            case Tree::Leaf => Option[Int]::None
            case Tree::Node(color, value, left, right) => {
                if target < value then {
                    left.find(target)
                } else if target > value then {
                    right.find(target)
                } else {
                    Option[Int]::Some(value)
                }
            }
        }

        // Find the predecessor of a given value in the Red-Black Tree
        // The predecessor is the largest value smaller than the given value
        def predecessor(tree: Tree, value: Int): Option[Int] = match tree {
            case Tree::Leaf => Option[Int]::None
            case Tree::Node(color, nodeValue, left, right) => if value <= nodeValue then {
                // Search in the left subtree if the value is less than or equal to the current node's value
                left.predecessor(value)
            } else {
                // Search in the right subtree, but also consider the current node as a potential predecessor
                match right.predecessor(value) {
                    case Option[Int]::None => Option[Int]::Some(nodeValue)
                    case Option[Int]::Some(pred) => Option[Int]::Some(pred)
                }
            }
        }

        // Find the successor of a given value in the Red-Black Tree
        // The successor is the smallest value greater than the given value
        def successor(tree: Tree, value: Int): Option[Int] = match tree {
            case Tree::Leaf => Option[Int]::None
            case Tree::Node(color, nodeValue, left, right) => if value >= nodeValue then {
                // Search in the right subtree if the value is greater than or equal to the current node's value
                right.successor(value)
            } else {
                // Search in the left subtree, but also consider the current node as a potential successor
                match left.successor(value) {
                    case Option[Int]::None => Option[Int]::Some(nodeValue)
                    case Option[Int]::Some(succ) => Option[Int]::Some(succ)
                }
            }
        }

        def depth(tree: Tree): ℤ = match tree {
            case Tree::Leaf => 0
            case Tree::Node(c, x, left, right) => max(left.depth, right.depth) + 1
        }

        def formatLevel(tree: Tree, level maxDepth: ℤ): String = {
            let spaces = " ".repeat(2 ** maxDepth - 1)
            if level == 0 then {
                match tree {
                    case Tree::Leaf => " "
                    case Tree::Node(c, value, l, r) => value.toString
                } ++ spaces
            } else match tree {
                case Tree::Leaf => " "
                case Tree::Node(c, x, left, right) => {
                    let leftStr = left.formatLevel(level - 1, maxDepth - 1)
                    let rightStr = right.formatLevel(level - 1, maxDepth - 1)
                    leftStr ++ rightStr
                }
            }
        }

        def formatLevelBelow(tree: Tree, level maxDepth: ℤ): String = {
            if level >= maxDepth then "" else {
                let prefixStr = " ".repeat(2 ** (maxDepth - 1 - level) + 4)
                let currentLevelStr = tree.formatLevel(level, maxDepth)
                let levelBelowStr = tree.formatLevelBelow(level + 1, maxDepth)
                prefixStr ++ currentLevelStr ++ "\n" ++ levelBelowStr
            }
        }

        // It's myTree!!!!!
        def myTree: Tree = {
            let tree = Tree::Leaf
            let tree = tree.insert(5)
            let tree = tree.insert(2)
            let tree = tree.insert(7)
            let tree = tree.insert(9)
            let tree = tree.insert(8)
            let tree = tree.insert(1)
            let tree = tree.insert(3)
            let tree = tree.insert(1)
            let tree = tree.insert(4)
            tree
        }
      """
    }
    val module = compileModule(code)

    module.eval("myTree.find(9)") should be (module.eval("Option(Int)::Some(9)"))
    module.eval("myTree.find(0)") should be (module.eval("Option(Int)::None"))
    module.eval("myTree.find(5)") should be (module.eval("Option(Int)::Some(5)"))
    module.eval("myTree.find(1)") should be (module.eval("Option(Int)::Some(1)"))
    module.eval("myTree.find(7)") should be (module.eval("Option(Int)::Some(7)"))
    module.eval("myTree.find(3)") should be (module.eval("Option(Int)::Some(3)"))
    module.eval("myTree.find(6)") should be (module.eval("Option(Int)::None"))
    module.eval("myTree.find(8)") should be (module.eval("Option(Int)::Some(8)"))
    module.eval("myTree.find(4)") should be (module.eval("Option(Int)::Some(4)"))
    module.eval("myTree.find(2)") should be (module.eval("Option(Int)::Some(2)"))

    module.eval("myTree.predecessor(5)") should be (module.eval("Option(Int)::Some(4)"))
    module.eval("myTree.predecessor(2)") should be (module.eval("Option(Int)::Some(1)"))
    module.eval("myTree.predecessor(7)") should be (module.eval("Option(Int)::Some(5)"))
    module.eval("myTree.predecessor(9)") should be (module.eval("Option(Int)::Some(8)"))
    module.eval("myTree.predecessor(8)") should be (module.eval("Option(Int)::Some(7)"))
    module.eval("myTree.predecessor(1)") should be (module.eval("Option(Int)::None"))
    module.eval("myTree.predecessor(3)") should be (module.eval("Option(Int)::Some(2)"))
    module.eval("myTree.predecessor(4)") should be (module.eval("Option(Int)::Some(3)"))

    module.eval("myTree.successor(5)") should be (module.eval("Option(Int)::Some(7)"))
    module.eval("myTree.successor(2)") should be (module.eval("Option(Int)::Some(3)"))
    module.eval("myTree.successor(7)") should be (module.eval("Option(Int)::Some(8)"))
    module.eval("myTree.successor(9)") should be (module.eval("Option(Int)::None"))
    module.eval("myTree.successor(8)") should be (module.eval("Option(Int)::Some(9)"))
    module.eval("myTree.successor(1)") should be (module.eval("Option(Int)::Some(2)"))
    module.eval("myTree.successor(3)") should be (module.eval("Option(Int)::Some(4)"))
    module.eval("myTree.successor(4)") should be (module.eval("Option(Int)::Some(5)"))
  }

  test("MLTT type checker") {
    val code = {
      """
        /**
         * This code implements a simple type checker and evaluator for Martin-Löf Type Theory (MLTT).
         *
         * MLTT is a constructive type theory foundational to many proof assistants and dependently
         * typed programming languages, such as Agda (MLTT) and Coq (CIC).
         *
         * In MLTT, types depend on values, leading to a system where functions can accept types
         * as parameters and return types as results. Key concepts include:
         * - Dependent Function Types (Pi Types): Generalizations of function types where the
         *   return type depends on the input value.
         * - Lambda Abstractions: Anonymous functions defined by specifying parameters and body.
         * - Universes: A hierarchy of types (e.g., `Type(0)`, `Type(1)`, etc.).
         *
         * This implementation models core constructs of MLTT, including terms, values, environments,
         * evaluation, type inference, and normalization.
         */
        
        /**
         * Extracts the value from an `Option[A]`. Throws an error if the option is `None`.
         * @param <A> Type of the value.
         * @param option The `Option` instance.
         * @return The extracted value of type `A`.
         */
        type Option[A: 'Type] = inductive {
            None        // Represents the absence of a value.
            Some(A)     // Wraps a value of type A.
        }
        
        /**
         * Extracts the value from an `Option[A]`. Throws an error if the option is `None`.
         * @param <A> Type of the value.
         * @param option The `Option` instance.
         * @return The extracted value of type `A`.
         */
        def unwrap[A: 'Type](option: Option[A]): A = match option {
            case Option[A]::None => panic("Unwrapping a none option type")
            case Option[A]::Some(value) => value
        }
        
        /**
         * `Term` represents the syntax of expressions in MLTT. Each constructor corresponds
         * to a syntactic category.
         */
        type Term = inductive {
            // Variable: Represents a variable identified by its name.
            Var(String)
            // Universe Level: Represents types at a certain universe level.
            Type(Int)
            // Dependent Pi Type: `Π(x : A). B`, where `B` may depend on `x`.
            Pi(String, Term, Term)
            // Lambda Term: `λ(x : A). t`.
            Lambda(String, Term, Term)
            // Application: Applying a function to an argument.
            Apply(Term, Term)
            // Sigma Type: `Σ(x : A). B`, a dependent pair type.
            Sigma(String, Term, Term)
            // Pair Term: `(a, b)`.
            Pair(Term, Term)
            // Projection: Extracting the first or second element of a pair.
            Proj(Projection, Term)
        }
        
        type Projection = inductive {
            Fst; Snd
        }
        
        /**
         * `Value` represents the evaluated form of terms, reducing to values during evaluation.
         */
        type Value = inductive {
            // Neutral Value: A value that cannot be reduced further
            Neutral(NeutralValue)
            // Universe Level: A type at a specific universe level.
            Type(Int)
            // Lambda Function: A function value with its parameter type and body.
            Lambda(Value, Value -> Value)
            // Pi Type Value: Represents a dependent function type.
            Pi(Value, Value -> Value)
            // Sigma Type Value: Represents a dependent pair type.
            Sigma(Value, Value -> Value)
            // Pair Value: A pair of values.
            Pair(Value, Value)
        }
        
        /**
         * Types are represented as values within this implementation.
         */
        type Type = Value
        
        // **Neutral Values**
        
        /*
         * `NeutralValue` represents expressions that cannot be evaluated further due to
         * the absence of sufficient information (e.g., variables or applications of variables).
         */
        type NeutralValue = inductive {
            // Variable: A neutral value representing an unresolved variable.
            Var(String)
            // Application: Applying a neutral function to a value.
            Apply(NeutralValue, Value)
            // Projection: Extracting the first or second element of a pair.
            Proj(Projection, NeutralValue)
        }
        
        /**
         * Converts a `NeutralValue` into a `Value`.
         * @param neutral The `NeutralValue` to convert.
         * @return The resulting `Value`.
         */
        def toValue(neutral: NeutralValue): Value = Value::Neutral(neutral)
        
        /**
         * `TypedValue` pairs a value with its type, essential for type checking and
         * ensuring type safety during evaluation.
         */
        type TypedValue = record {
            value: Value    // The evaluated value.
            ty: Type        // The type of the value.
        }
        
        /**
         * `Env` represents the typing context, mapping variable names to their corresponding typed values.
         */
        type Env = inductive {
            Empty
            Cons(String, TypedValue, Env)
        }
        
        /**
         * Adds a new binding to the environment.
         * @param env The current environment.
         * @param name The variable name.
         * @param value The value to bind.
         * @param ty The type of the value.
         * @return A new environment with the added binding.
         */
        def add(env: Env, name: String, value: Value, ty: Type): Env = {
            let typedValue = TypedValue '{
                value = value  // The value associated with the name.
                ty = ty        // The type of the value.
            }
            Env::Cons(name, typedValue, env)
        }
        
        /**
         * Adds a variable to the environment as a neutral value, commonly used when introducing parameters.
         * @param env The current environment.
         * @param ident The identifier of the variable.
         * @param ty The type of the variable.
         * @return A new environment with the variable added as a neutral value.
         */
        def addVar(env: Env, ident: String, ty: Type): Env = {
            env.add(ident, NeutralValue::Var(ident).toValue, ty)
        }
        
        /**
         * Retrieves a binding from the environment by name.
         * @param env The current environment.
         * @param name The name of the variable to retrieve.
         * @return An `Option` of `TypedValue` containing the variable's type if found, or `None` if not found.
         */
        def get(env: Env, name: String): Option[TypedValue] = {
            match env {
                case Env::Empty => Option[TypedValue]::None  // Name not found.
                case Env::Cons(name', value, env') => {
                    if name' == name then Option[TypedValue]::Some(value)
                    else env'.get(name) // Search in the rest of the environment.
                }
            }
        }
        
        /**
         * Checks if a name exists in the environment.
         * @param env The current environment.
         * @param name The name to check for.
         * @return `true` if the name exists in the environment, `false` otherwise.
         */
        def contains(env: Env, name: String): Bool = match env {
            case Env::Empty => false  // Name not found.
            case Env::Cons(name', _, env') => name' == name || env'.contains(name)  // Found or continue searching.
        }
        
        /**
         * Generates a fresh identifier not present in the environment, used to avoid variable capture during substitution.
         * @param env The current environment.
         * @param cnt The starting count for generating identifiers.
         * @return A fresh identifier not currently in the environment.
         */
        def freshIdentFrom(env: Env, cnt: Int): String = {
            let ident = "$" ++ cnt.toString     // Generates identifiers like `$0`, `$1`, etc.
            if !env.contains(ident) then ident  // If not in the environment, it's fresh.
            else env.freshIdentFrom(cnt + 1)    // Try the next identifier.
        }
        
        /**
         * Generates a fresh identifier starting from `$0`.
         * @param env The current environment.
         * @return A fresh identifier.
         */
        def freshIdent(env: Env): String = env.freshIdentFrom(0)
        
        /**
         * Evaluates a `Term` in a given environment to produce a `Value`.
         * Evaluation proceeds by pattern matching on the term's structure.
         * @param env The current environment.
         * @param expr The `Term` to evaluate.
         * @return The evaluated `Value`.
         */
        def evaluate(env: Env, expr: Term): Value = match expr {
            // Look up the variable's value.
            case Term::Var(name) => env.get(name).unwrap[TypedValue].value
            // A type evaluates to itself.
            case Term::Type(univ) => Value::Type(univ)
            // Lambda Evaluation: Constructs a closure capturing the environment and parameter.
            case Term::Lambda(paramIdent, paramTypeTerm, bodyTerm) => {
                let paramType = env.evaluate(paramTypeTerm) // Evaluate parameter type.
                let closure = (arg: Value) => {
                    // Evaluate the body with the argument bound.
                    env.add(paramIdent, arg, paramType).evaluate(bodyTerm)
                }
                Value::Lambda(paramType, closure)
            }
            // Pi Type Evaluation: Similar to lambda
            case Term::Pi(paramIdent, paramTypeTerm, codomainTerm) => {
                let paramType = env.evaluate(paramTypeTerm) // Evaluate parameter type.
                let closure = (arg: Value) => {
                    // Evaluate codomain with argument bound.
                    env.add(paramIdent, arg, paramType).evaluate(codomainTerm)
                }
                Value::Pi(paramType, closure)
            }
            // Sigma Type Evaluation: Similar to lambda
            case Term::Sigma(paramIdent, paramTypeTerm, codomainTerm) => {
                let paramType = env.evaluate(paramTypeTerm) // Evaluate parameter type.
                let closure = (arg: Value) => {
                    // Evaluate codomain with argument bound.
                    env.add(paramIdent, arg, paramType).evaluate(codomainTerm)
                }
                Value::Sigma(paramType, closure)
            }
            // Function Application Evaluation
            case Term::Apply(fn, arg) => match env.evaluate(fn) {
                // Apply function to the argument.
                case Value::Lambda(_, fn) => fn(env.evaluate(arg))
                // Neutral Application: Cannot reduce further; keep it a neutral value.
                case Value::Neutral(neutral) => NeutralValue::Apply(neutral, env.evaluate(arg)).toValue
                case _ => panic("Invalid type: not a function")
            }
            // Pair Construction
            case Term::Pair(fst, snd) => Value::Pair(env.evaluate(fst), env.evaluate(snd))
        }
        
        
        /**
         * Converts a `NeutralValue` back into a `Term`, used during normalization to reconstruct
         * terms from evaluated values.
         * @param neutral The `NeutralValue` to convert.
         * @param env The current environment.
         * @return The reconstructed `Term`.
         */
        def reflect(neutral: NeutralValue, env: Env): Term = match neutral {
            // Convert variable to term.
            case NeutralValue::Var(name) => Term::Var(name)
            // Reconstruct application.
            case NeutralValue::Apply(fn, arg) => Term::Apply(fn.reflect(env), arg.reflect(env))
            // Reconstruct projection.
            case NeutralValue::Proj(proj, neutral) => Term::Proj(proj, neutral.reflect(env))
        }
        
        /**
         * Converts a `Value` back into a `Term`, effectively normalizing the term by reducing it to its simplest form.
         * @param value The `Value` to convert.
         * @param env The current environment.
         * @return The normalized `Term`.
         */
        def reflect(value: Value, env: Env): Term = match value {
            case Value::Neutral(neutral) => neutral.reflect(env)
            case Value::Type(univ) => Term::Type(univ)
            case Value::Pair(fst, snd) => Term::Pair(fst.reflect(env), snd.reflect(env))
        
            // Lambda Normalization: Generate a fresh variable to avoid capture.
            case Value::Lambda(paramType, fn) => {
                let paramIdent: String = env.freshIdent
                // Normalize parameter type.
                let paramTypeTerm = paramType.reflect(env)
                // Create variable value.
                let variable: Value = NeutralValue::Var(paramIdent).toValue
                // Extend environment.
                let updatedEnv = env.add(paramIdent, variable, env.evaluate(paramTypeTerm))
                Term::Lambda(
                    paramIdent, paramTypeTerm,         // Construct lambda term.
                    fn(variable).reflect(updatedEnv)  // Normalize the body.
                )
            }
        
            // Pi Type Normalization: Similar to lambda normalization.
            case Value::Pi(paramType, fn) => {
                // Fresh parameter name.
                let paramIdent: String = env.freshIdent
                // Normalize parameter type.
                let paramTypeTerm = paramType.reflect(env)
                // Create variable value.
                let variable: Value = NeutralValue::Var(paramIdent).toValue
                // Extend environment.
                let updatedEnv = env.add(paramIdent, variable, env.evaluate(paramTypeTerm))
                Term::Pi(
                    paramIdent, paramTypeTerm,          // Construct Pi type term.
                    fn(variable).reflect(updatedEnv)   // Normalize the codomain.
                )
            }
        
            // Sigma Type Normalization: Similar to lambda normalization.
            case Value::Sigma(paramType, fn) => {
                // Fresh parameter name.
                let paramIdent: String = env.freshIdent
                // Normalize parameter type.
                let paramTypeTerm = paramType.reflect(env)
                // Create variable value.
                let variable: Value = NeutralValue::Var(paramIdent).toValue
                // Extend environment.
                let updatedEnv = env.add(paramIdent, variable, env.evaluate(paramTypeTerm))
                Term::Sigma(
                    paramIdent, paramTypeTerm,          // Construct Sigma type term.
                    fn(variable).reflect(updatedEnv)   // Normalize the codomain.
                )
            }
        }
        
        /**
         * Retrieves the universe level from a `Type` value.
         * Universe levels are critical in MLTT to maintain consistency and avoid paradoxes.
         * @param ty The `Type` value.
         * @return The universe level as an `Int`.
         */
        def universeLevel(ty: Type): Int = match ty {
            case Value::Type(univ) => univ                                  // Extract universe level.
            case _ => panic("Failed to unwrap universe level: not a type")  // Panic if not a type.
        }
        
        /**
         * Infers the type of a `Term` within a given environment following MLTT's typing rules.
         * @param env The current environment.
         * @param expr The `Term` whose type is inferred.
         * @return The inferred type as a `Value`.
         */
        def infer(env: Env, expr: Term): Value = match expr {
        
            // Retrieve the variable's type from the environment.
            case Term::Var(name) => env.get(name).unwrap[TypedValue].ty
        
            // `Type(n)` has type `Type(n + 1)`.
            case Term::Type(univ) => Value::Type(univ + 1)
        
            // Lambda Type Inference:
            case Term::Lambda(paramIdent, paramTypeTerm, bodyTerm) => {
                // Infer parameter type's universe level.
                let paramLevel = env.infer(paramTypeTerm).universeLevel
                // Evaluate parameter type.
                let paramType: Type = env.evaluate(paramTypeTerm)
                // Create variable for parameter.
                let variable: Value = NeutralValue::Var(paramIdent).toValue
                // Extend environment with parameter.
                let bodyEnv = env.add(paramIdent, variable, paramType)
                // Infer body's type.
                let returnType: Type = bodyEnv.infer(bodyTerm)
                // The lambda's type is a Pi type from parameter to return type.
                Value::Pi(
                    paramType,
                    (arg: Value) => {
                        // Infer argument's type.
                        let argType = env.infer(arg.reflect(bodyEnv))
                        // Evaluate the body.
                        bodyEnv.add(paramIdent, arg, argType).evaluate(bodyTerm)
                    }
                )
            }
        
            // Pair Type Inference:
            case Term::Pair(fst, snd) => {
                // Infer the type of the first element.
                let fstType: Type = env.infer(fst)
                // Infer the type of the second element.
                let sndType: Type = env.infer(snd)
                // The pair type is a Sigma type of the two elements.
                Value::Sigma(fstType, (fstValue: Value) => {
                    Value::Sigma(sndType, (sndValue: Value) => Value::Pair(fstValue, sndValue))
                })
            }
        
            // Pi Type Inference:
            case Term::Pi(paramIdent, paramTypeTerm, returnTypeTerm) => {
                // Infer parameter type's universe level.
                let paramLevel = env.infer(paramTypeTerm).universeLevel
                // Evaluate parameter type.
                let paramType: Type = env.evaluate(paramTypeTerm)
                // Create variable for parameter.
                let variable: Value = NeutralValue::Var(paramIdent).toValue
                let returnTypeLevel = env.add(paramIdent, variable, paramType).infer(returnTypeTerm).universeLevel
                // The Pi type's universe level is the maximum of parameter and return types.
                Value::Type(max paramLevel returnTypeLevel)
            }
        
            // Sigma Type Inference:
            case Term::Sigma(paramIdent, paramTypeTerm, codomainTerm) => {
                // Infer parameter type's universe level.
                let paramLevel = env.infer(paramTypeTerm).universeLevel
                // Evaluate parameter type.
                let paramType: Type = env.evaluate(paramTypeTerm)
                // Create variable for parameter.
                let variable: Value = NeutralValue::Var(paramIdent).toValue
                let rhsTypeLevel = env.add(paramIdent, variable, paramType).infer(codomainTerm).universeLevel
                // The sigma type's universe level is the maximum of lhs and rhs types.
                Value::Type(max paramLevel rhsTypeLevel)
            }
        }
        
        /**
         * Normalizes a `Term` by evaluating it and converting the result back into a term.
         * Normalization is essential for comparing terms for equality and ensuring consistent behavior.
         * @param env The current environment.
         * @param expr The `Term` to normalize.
         * @return The normalized `Term`.
         */
        def normalize(env: Env, expr: Term): Term = env.evaluate(expr).reflect(env)

        def pretty(expr: Term): String = match expr {
            case Term::Var(name) => name
            case Term::Type(univ) => "Type(" ++ univ.toString ++ ")"
            case Term::Lambda(paramIdent, paramType, body) =>
                "λ(" ++ paramIdent ++ " : " ++ paramType.pretty ++ "). " ++ body.pretty
            case Term::Apply(fn, arg) => "(" ++ fn.prettyAtom ++ " " ++ arg.prettyAtom ++ ")"
            case Term::Pi(paramIdent, paramType, returnType) =>
                "Π(" ++ paramIdent ++ " : " ++ paramType.pretty ++ "). " ++ returnType.pretty
            case Term::Sigma(paramIdent, paramType, codomain) =>
                "Σ(" ++ paramIdent ++ " : " ++ paramType.pretty ++ "). " ++ codomain.pretty
            case Term::Pair(fst, snd) => "(" ++ fst.pretty ++ ", " ++ snd.pretty ++ ")"
            case Term::Proj(Projection::Fst, pair) => "fst " ++ pair.pretty
        }

        def prettyAtom(expr: Term): String = match expr {
            case Term::Var(name) => name
            case Term::Type(_) => expr.pretty
            case Term::Lambda(_, _, _) => "(" ++ expr.pretty ++ ")"
            case Term::Apply(_, _) => "(" ++ expr.pretty ++ ")"
            case Term::Pi(_, _, _) => "(" ++ expr.pretty ++ ")"
            case Term::Sigma(_, _, _) => "(" ++ expr.pretty ++ ")"
            case Term::Pair(_, _) => expr.pretty
            case Term::Proj(_, _) => "(" ++ expr.pretty ++ ")"
        }

        def var(ident: String): Value = NeutralValue::Var(ident).toValue

        def prelude: Env = Env::Empty
            .addVar("Any", Value::Type(0))
            .addVar("Nothing", Value::Type(0))
            .addVar("Bool", Value::Type(0))
            .addVar("true", var("Bool"))
            .addVar("false", var("Bool"))
            .addVar("Nat", Value::Type(0))
            .addVar("zero", var("Nat"))
            .addVar("succ", Value::Pi(var("Nat"), (n: Value) => var("Nat")))
      """
    }
    val module = compileModule(code)

    module.eval(
      """
      prelude.normalize(Term::Apply(Term::Lambda("x", Term::Var("Bool"), Term::Var("x")), Term::Var("true")))
    """) should be (module.eval(
    """
      Term::Var("true")
    """))

    // (λ(x : Bool) . false) true
    module.eval(
      """
      prelude.normalize(
        Term::Apply(
          Term::Lambda("x", Term::Var("Bool"), Term::Var("false")),
          Term::Var("true")
        )
      )
    """) should be (module.eval(
    """
      Term::Var("false")
    """))

    // (λ(f : Π(x : Bool) . Bool) . f(true)) (λ(x : Bool) . x)
    module.eval(
      """
      prelude.normalize(
        Term::Apply(
          Term::Lambda("f", Term::Pi("x", Term::Var("Bool"), Term::Var("Bool")),
            Term::Apply(Term::Var("f"), Term::Var("true"))
          ),
          Term::Lambda("x", Term::Var("Bool"), Term::Var("x"))
        )
      )
      """) should be (module.eval(
      """
      Term::Var("true")
    """))

    // (λ(x : Bool) . λ(y : Π(z : Bool) . Bool) . y) true  ===  λ(y : Π(z : Bool) . Bool) . y
    module.eval(
      """
      prelude.normalize(
        Term::Apply(
          Term::Lambda("x", Term::Var("Bool"),
            Term::Lambda("y", Term::Pi("z", Term::Var("Bool"), Term::Var("Bool")),
              Term::Var("y")
            )
          ),
          Term::Var("true")
        )
      )
    """) should be (module.eval(
      """
      prelude.normalize(Term::Lambda("y", Term::Pi("z", Term::Var("Bool"), Term::Var("Bool")), Term::Var("y")))
    """))
  }

}
