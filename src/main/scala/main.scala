import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import saki.concrete.Visitor
import saki.concrete.syntax.Definition
import saki.core.syntax.{Module, PristineDefinition}
import saki.core.{Error, InfoSpan}
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

  def fib(n : Nat): Nat = match n {
      case Nat::Zero => Nat::Zero
      case Nat::Succ(Nat::Zero) => Nat::Succ(Nat::Zero)
      case Nat::Succ(Nat::Succ(n')) => plus(fib(n'), fib(Nat::Succ(n')))
  }
  """
}


def catchError[R](source: String)(action: => R): R = {
  try action catch {
    case error: Error =>
      error.span match {
        case Some(infoSpan) =>
          println(s"Error: ${error.message}")
          printSourceWithHighlight(source, infoSpan)
        case None =>
          println(s"Error: ${error.message}")
      }
      // Rethrow the original Error after showing it
      throw error
  }
}

def printSourceWithHighlight(source: String, span: InfoSpan): Unit = {
  // Split source by lines for display
  val lines = source.split("\n").zipWithIndex

  // Calculate the line and character positions of the error
  val (startLine, endLine) = {
    val (start, end) = (span.start, span.end)
    val startLine = source.substring(0, start).count(_ == '\n')
    val endLine = source.substring(0, end).count(_ == '\n')
    (startLine, endLine)
  }

  // Print each line, and highlight the range containing the error
  lines.foreach {
    case (line, idx) if idx >= startLine && idx <= endLine =>
      println(f"$idx%4d: $line")
      // Highlight the error within the line
      if (idx == startLine) {
        val highlightStart = span.start - source.substring(0, span.start).lastIndexOf('\n') - 1
        val highlightEnd = if (startLine == endLine) span.end - span.start + highlightStart else line.length
        println(" " * (highlightStart + 6) + "^" * (highlightEnd - highlightStart + 1) + " " + span.info)
      }
    case (line, idx) =>
      println(f"$idx%4d: $line")
  }
}


@main
def main(): Unit = {
  val lexer = SakiLexer(CharStreams.fromString(exampleCode))
  val parser = SakiParser(CommonTokenStream(lexer))
  val visitor = Visitor()
  val defs: Seq[Definition] = visitor.visitProgram(parser.program())

  defs.foreach(println)

  val module = catchError(exampleCode) {
    val definitions = defs.flatMap { definition =>
      definition.emit match {
        case inductive: PristineDefinition.Inductive => Seq(inductive) // ++ inductive.constructors
        case other => Seq(other)
      }
    }
    Module.from(definitions)
  }

  println(module)
}
