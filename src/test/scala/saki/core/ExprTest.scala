package saki.core

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import saki.cli.catchError
import saki.concrete.Visitor
import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.syntax.Var
import saki.grammar.{SakiLexer, SakiParser}

class ExprTest extends AnyFlatSpec with should.Matchers {

  extension (str: String) def unary_! : Var.Local = Var.Local(str)

  extension (literal: Literal) {
    def term: Term.Primitive = Term.Primitive(literal)
    def value: Value.Primitive = Value.Primitive(literal)
  }

  extension (literalType: LiteralType) {
    def term: Term.PrimitiveType = Term.PrimitiveType(literalType)
    def value: Value.PrimitiveType = Value.PrimitiveType(literalType)
  }

  import Literal.*
  import LiteralType.*

  given Environment.Typed[Value] = Environment.Typed.empty[Value]

  def synthExpr(code: String): (Term, Value) = {
    val stripedCode = code.strip()
    val lexer = SakiLexer(CharStreams.fromString(stripedCode))
    val parser = SakiParser(CommonTokenStream(lexer))
    val exprTree = Visitor().visitExpr(parser.expr())
    catchError(stripedCode) {
      exprTree.emit.synth.unpack
    }
  }

  def synthCodeBlock(code: String): (Term, Value) = {
    val stripedCode = "{\n" + code.strip() + "\n}"
    val lexer = SakiLexer(CharStreams.fromString(stripedCode))
    val parser = SakiParser(CommonTokenStream(lexer))
    val block = Visitor().visitBlockExpr(parser.blockExpr())
    catchError(stripedCode) {
      block.emit.synth.unpack
    }
  }

  it should "synth primitive type Int" in {
    val (expr, value) = synthExpr("1")
    expr should be (Term.Primitive(IntValue(1)))
    value should be (Value.PrimitiveType(IntType))
  }

  it should "synth lambda" in {
    val (expr, value) = synthExpr("(x: Int) => x")
    expr should be (Term.Lambda(Param(!"x", IntType.term), Term.Variable(!"x")))
    value.readBack should be (Value.Pi(Value.PrimitiveType(LiteralType.IntType), _ => Value.PrimitiveType(LiteralType.IntType)).readBack)
  }

  it should "synth lambda with explicit type" in {
    val (expr, value) = synthExpr("(x: Int): Int => x")
    expr should be (Term.Lambda(Param(!"x", IntType.term), Term.Variable(!"x")))
    value.readBack should be (Value.Pi(Value.PrimitiveType(LiteralType.IntType), _ => Value.PrimitiveType(LiteralType.IntType)).readBack)
  }

  it should "synth high-order lambda with explicit type" in {
    val (expr, value) = synthExpr("(x: Int) => (y: Int) => x")
    expr should be (Term.Lambda(Param(!"x", IntType.term), Term.Lambda(Param(!"y", IntType.term), Term.Variable(!"x"))))
    value.readBack should be (
      Value.Pi(
        Value.PrimitiveType(LiteralType.IntType),
        _ => Value.Pi(Value.PrimitiveType(LiteralType.IntType), _ => Value.PrimitiveType(LiteralType.IntType))
      ).readBack
    )
  }

  it should "beta reduction" in {
    val (expr, _) = synthExpr("((x: Int) => x)(114514)")
    expr should be (IntValue(114514).term)
  }

  it should "let expr" in {
    val code = {
      """
        let x: Int = 1
        x
      """
    }
    val (expr, _) = synthCodeBlock(code)
    expr should be (IntValue(1).term)
  }

  it should "let apply" in {
    val code = {
      """
        let f: Int -> Int = (x: Int) => x
        f(114514)
      """
    }
    val (expr, _) = synthCodeBlock(code)
    expr should be (IntValue(114514).term)
  }

  it should "if expr" in {
    val (expr, _) = synthCodeBlock("if true then 114 else 514")
    expr should be (IntValue(114).term.normalize)
  }
}
