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

  test("overloaded") {
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

}
