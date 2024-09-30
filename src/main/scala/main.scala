import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import saki.grammar.{SakiLexer, SakiParser}

val exampleCode =
"""
type Nat = inductive {
    Zero
    Succ(Nat)
}

def plus(a b : Nat): Nat = match a {
    case Nat::Zero => b
    case Nat::Succ(a') = Nat::Succ(plus(a', b))
}

def fib(n : Nat): Nat = match n {
    case Nat::Zero => Nat::Zero
    case Nat::Succ(Nat::Zero) => Nat::Succ(Nat::Zero)
    case Nat::Succ(Nat::Succ(n')) => plus(fib n', fib Nat::Succ(n'))
}
"""

@main
def main(): Unit = {
//  val lexer = SakiLexer(CharStreams.fromString(exampleCode))
//  val parser = SakiParser(CommonTokenStream(lexer))
//  val visitor = Visitor()
//  val ast: Seq[Expr] = visitor.visitProgram(parser.program())
//  ast.foreach(println)
}
