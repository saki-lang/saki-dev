import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import saki.cli.catchError
import saki.concrete.Visitor
import saki.concrete.syntax.Definition
import saki.core.syntax.Module
import saki.grammar.{SakiLexer, SakiParser}

import scala.collection.Seq

val exampleCode: String = {
  """
  type Nat = inductive {
      Zero
      Succ(Nat)
  }

  def plus(a b : Nat): Nat = match a {
      case Nat::Zero => b
      case Nat::Succ(a') => Nat::Succ(plus(a', b))
  }

  def twoPlusThree: Nat = plus(Nat::Succ(Nat::Succ(Nat::Zero)), Nat::Succ(Nat::Succ(Nat::Succ(Nat::Zero))))

  def fib(n : Nat): Nat = match n {
      case Nat::Zero => Nat::Zero
      case Nat::Succ(Nat::Zero) => Nat::Succ(Nat::Zero)
      case Nat::Succ(Nat::Succ(n')) => plus(fib(n'), fib(Nat::Succ(n')))
  }

  def one: Nat = Nat::Succ(Nat::Zero)
  def two: Nat = Nat::Succ(Nat::Succ(Nat::Zero))
  def three: Nat = Nat::Succ(Nat::Succ(Nat::Succ(Nat::Zero)))
  def four: Nat = Nat::Succ(Nat::Succ(Nat::Succ(Nat::Succ(Nat::Zero))))
  def five: Nat = Nat::Succ(Nat::Succ(Nat::Succ(Nat::Succ(Nat::Succ(Nat::Zero)))))
  def six: Nat = Nat::Succ(Nat::Succ(Nat::Succ(Nat::Succ(Nat::Succ(Nat::Succ(Nat::Zero))))))

  def fib1: Nat = fib(one)
  def fib2: Nat = fib(two)
  def fib3: Nat = fib(three)
  def fib4: Nat = fib(four)
  def fib5: Nat = fib(five)
  def fib6: Nat = fib(six)
  """
}

@main
def main(): Unit = {
  val lexer = SakiLexer(CharStreams.fromString(exampleCode))
  val parser = SakiParser(CommonTokenStream(lexer))
  val visitor = Visitor()
  val defs: Seq[Definition] = visitor.visitProgram(parser.program())

  defs.foreach(println)

  val module = catchError(exampleCode) {
    Module.from(defs.map(_.emit))
  }

  println(module)
}
