import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import saki.grammar.{SakiLexer, SakiParser}
import saki.frontend.{Term, Visitor}

val exampleCode =
"""
def fib(n: Int): Int = {
  if n.lt(2) then
    n
  else
    add(fib(n.minus(1)), fib(n.minus(2)))
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
