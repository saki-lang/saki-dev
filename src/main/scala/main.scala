import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import saki.cli.catchError
import saki.concrete.Visitor
import saki.concrete.syntax.{Definition, Evaluation}
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

  eval "fib(1)"
  eval fib(one)
  eval fib(two)
  eval fib(three)
  eval fib(four)
  eval fib(five)
  eval fib(six)
  """
}

@main
def main(): Unit = {
  val lexer = SakiLexer(CharStreams.fromString(exampleCode))
  val parser = SakiParser(CommonTokenStream(lexer))
  val visitor = Visitor()
  val entities: Seq[Definition | Evaluation] = visitor.visitProgram(parser.program())
  val definitions = entities.collect { case defn: Definition => defn }

  entities.foreach(println)

  val module = catchError(exampleCode) {
    Module.from(definitions.map(_.emit))
  }

  println(module)
  println("\n\n=================================================\n\n")

  val evaluations = entities.collect { case eval: Evaluation => eval }
  evaluations.foreach { evaluation => println(module.evaluate(evaluation.expr.emit)) }
}
