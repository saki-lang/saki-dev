package saki.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import saki.core.domain.Value

class ExprTest extends AnyFlatSpec with should.Matchers with SakiTestExt {

  import Literal.*
  import LiteralType.*

  it should "synth primitive type Int" in {
    val (term, ty) = synthExpr("1")
    term should be (Term.Primitive(IntValue(1)))
    ty should be (Value.PrimitiveType(IntType))
  }

  it should "synth lambda" in {
    val (term, ty) = synthExpr("(x: Int) => x")
    term should be (Term.Lambda(Param(!"x", IntType.term), Term.Variable(!"x")))
    ty.readBack should be (Value.Pi(Value.PrimitiveType(LiteralType.IntType), _ => Value.PrimitiveType(LiteralType.IntType)).readBack)
  }

  it should "synth lambda with explicit type" in {
    val (term, ty) = synthExpr("(x: Int): Int => x")
    term should be (Term.Lambda(Param(!"x", IntType.term), Term.Variable(!"x")))
    ty.readBack should be (Value.Pi(Value.PrimitiveType(LiteralType.IntType), _ => Value.PrimitiveType(LiteralType.IntType)).readBack)
  }

  it should "synth high-order lambda" in {
    val (term, ty) = synthExpr("(x: Int) => (y: Int) => x")
    term should be (Term.Lambda(Param(!"x", IntType.term), Term.Lambda(Param(!"y", IntType.term), Term.Variable(!"x"))))
    ty.readBack should be (
      Value.Pi(
        Value.PrimitiveType(LiteralType.IntType),
        _ => Value.Pi(Value.PrimitiveType(LiteralType.IntType), _ => Value.PrimitiveType(LiteralType.IntType))
      ).readBack
    )
  }

  it should "synth dependent typed lambda" in {
    val (term, ty) = synthExpr("(A: 'Type) => (B: 'Type) => (t: A) => (f: A -> B) => f(t)")
    term.normalize should be (
      Value.Lambda(
        Value.Universe, A => Value.Lambda(
          Value.Universe, B => Value.Lambda(
            A, t => Value.Lambda(
              Value.Pi(A, _ => B),
              f => f(t)
            )
          )
        )
      ).readBack
    )
    ty.readBack should be (
      Value.Pi(
        Value.Universe, A => Value.Pi(
          Value.Universe, B => Value.Pi(
            A, _ => Value.Pi(
              Value.Pi(A, _ => B),
              _ => B
            )
          )
        )
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
    expr.normalize should be (IntValue(114).term)
  }
}
