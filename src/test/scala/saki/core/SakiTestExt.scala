package saki.core

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import saki.concrete.Visitor
import saki.core.context.Environment
import saki.core.domain.{NeutralValue, Value}
import saki.core.syntax.{Var, Module}
import saki.grammar.{SakiLexer, SakiParser}
import saki.tool.catchError

trait SakiTestExt {
  extension (str: String) def unary_! : Var.Local = Var.Local(str)

  extension (literal: Literal) {
    def term: Term.Primitive = Term.Primitive(literal)
    def value: Value.Primitive = Value.Primitive(literal)
  }

  extension (literalType: LiteralType) {
    def term: Term.PrimitiveType = Term.PrimitiveType(literalType)
    def value: Value.PrimitiveType = Value.PrimitiveType(literalType)
  }

  extension (value: Value) {
    def neutral: NeutralValue = value match {
      case Value.Neutral(neutral) => neutral
      case _ => throw new Exception("Not a neutral value.")
    }

    def apply(arg: Value): Value = value match {
      case Value.Lambda(_, closure) => closure(arg)
      case Value.Neutral(neutral) => Value.Neutral(NeutralValue.Apply(neutral, arg))
      case _ => throw new Exception("Not a function.")
    }
  }

  extension (module: Module) {
    def eval(code: String): Module.EvalResult = {
      val stripedCode = code.strip()
      val lexer = SakiLexer(CharStreams.fromString(stripedCode))
      val parser = SakiParser(CommonTokenStream(lexer))
      val expr = Visitor().visitExpr(parser.expr())
      catchError(stripedCode) {
        module.evaluate(expr.emit)
      }
    }
  }

  def parseExpr(code: String): Expr = {
    val stripedCode = code.strip()
    val lexer = SakiLexer(CharStreams.fromString(stripedCode))
    val parser = SakiParser(CommonTokenStream(lexer))
    Visitor().visitExpr(parser.expr()).emit
  }

  given Environment.Typed[Value] = Environment.Typed.empty[Value]

  def synthExpr(code: String): (Term, Value) = catchError(code.strip) {
    parseExpr(code).synth.unpack
  }

  def synthCodeBlock(code: String): (Term, Value) = {
    val stripedCode = "{\n" + code.strip() + "\n}"
    val lexer = SakiLexer(CharStreams.fromString(stripedCode))
    val parser = SakiParser(CommonTokenStream(lexer))
    val block = Visitor().visitBlockExpr(parser.blockExpr())
    catchError(stripedCode) {
      val expr = block.emit
      expr.synth.unpack
    }
  }
}
