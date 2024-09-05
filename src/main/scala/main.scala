import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import saki.Visitor
import saki.grammar.{SakiLexer, SakiParser}
import saki.syntax.Term

val exampleCode =
"""
  def nil[T: 'Type]: List(T) = Nil

  def List(T: 'Type): 'Type = enum {
      Nil
      Cons(T, List(T))
  }

  def append[T: 'Type](lhs rhs: List(T)): List(T) = match lhs {
      Nil => rhs
      Cons(head, tail) => Cons(head, append(tail, rhs))
  }

  def rev[T: 'Type](self: List(T)): List(T) = match self {
      Nil => Nil
      Cons(head, tail) => append(tail.rev, Cons(head, nil))
  }

  def len[T: 'Type](self: List(T)): Int = match self {
      Nil => 0
      Cons(_, tail) => tail.len.succ
  }

  def map[T R: 'Type](self: List(T), trans: T -> R): List(R) = match self {
      Nil => Nil
      Cons(head, tail) => Cons(trans(head), tail.map(trans))
  }

  def filter[T: 'Type](self: List(T), pred: T -> Bool): List(T) = match self {
      Nil => Nil
      Cons(head, tail) => {
          if pred(head) then Cons(head, tail.filter(pred))
          else tail.filter(pred)
      }
  }

  def foldRight[T R: 'Type](self: List(T), acc: R, trans: T -> R -> R): R = {
      match self {
          Nil => acc
          Cons(head, tail) => trans(head, tail.foldRight(acc, trans))
      }
  }

  def take[T: 'Type](self: List(T), n: Int): List(T) = match '(n, self) {
      (0, _) => Nil
      (_, Nil) => Nil
      (n, Cons(head, tail)) => Cons(head, tail.take(n.prec))
  }

  def drop[T: 'Type](self: List(T), n: Int): List(T) = match '(n, self) {
      (0, _) => self
      (_, Nil) => Nil
      (n, Cons(_, tail)) => tail.drop(n.prec)
  }
"""

@main
def main(): Unit = {
  val lexer = SakiLexer(CharStreams.fromString(exampleCode))
  val parser = SakiParser(CommonTokenStream(lexer))
  val visitor = Visitor()
  val ast: Seq[Term] = visitor.visitProgram(parser.program())
  ast.foreach(println)
}
