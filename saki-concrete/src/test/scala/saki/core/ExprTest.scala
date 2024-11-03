package saki.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import saki.core.domain.Value

class ExprTest extends AnyFunSuite with should.Matchers with SakiTestExt {

  import Literal.*
  import LiteralType.*

  test("synth primitive type Int") {
    val (term, ty) = synthExpr("1")
    term should be (Term.Primitive(IntValue(1)))
    ty should be (Value.PrimitiveType(IntType))
  }

  test("synth lambda") {
    val (term, ty) = synthExpr("(x: Int) => x")
    term should be (Term.Lambda(Param(!"x", IntType.term), Term.Variable(!"x")).normalize)
    ty.readBack should be (Value.Pi(Value.PrimitiveType(LiteralType.IntType), _ => Value.PrimitiveType(LiteralType.IntType)).readBack)
  }

  test("synth lambda with explicit type") {
    val (term, ty) = synthExpr("(x: Int): Int => x")
    term should be (Term.Lambda(Param(!"x", IntType.term), Term.Variable(!"x")).normalize)
    ty.readBack should be (Value.Pi(Value.PrimitiveType(LiteralType.IntType), _ => Value.PrimitiveType(LiteralType.IntType)).readBack)
  }

  test("synth high-order lambda") {
    val (term, ty) = synthExpr("(x: Int) => (y: Float) => (z: String): Int => x")
    term should be (
      Term.Lambda(
        Param(!"x", IntType.term),
        Term.Lambda(
          Param(!"y", FloatType.term),
          Term.Lambda(
            Param(!"z", StringType.term),
            Term.Variable(!"x")
          )
        )
      ).normalize
    )
    ty.readBack should be (
      Value.Pi(
        Value.PrimitiveType(LiteralType.IntType),
        _ => Value.Pi(
          Value.PrimitiveType(LiteralType.FloatType),
          _ => Value.Pi(
            Value.PrimitiveType(LiteralType.StringType),
            _ => Value.PrimitiveType(LiteralType.IntType)
          )
        )
      ).readBack
    )
  }

  test("synth dependent typed lambda") {
    val (term, ty) = synthExpr("(A: 'Type) => (B: 'Type) => (t: A) => (f: A -> B) => f(t)")
    term.normalize should be (
      Value.Lambda(
        Value.Universe, A => Value.Lambda(
          Value.Universe, B => Value.Lambda(
            A, t => Value.Lambda(
              Value.Pi(A, _ => B),
              f => (f)(t)
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

  test("synth simplified dependent typed lambda") {
    val (term, ty) = synthExpr("(A B: 'Type, t: A, f: A -> B) => f(t)")
    term.normalize should be(
      Value.Lambda(
        Value.Universe, A => Value.Lambda(
          Value.Universe, B => Value.Lambda(
            A, t => Value.Lambda(
              Value.Pi(A, _ => B),
              f => (f)(t)
            )
          )
        )
      ).readBack
    )
    ty.readBack should be(
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

  test("beta reduction") {
    val (expr, _) = synthExpr("((x: Int) => x)(114514)")
    expr.normalize should be (IntValue(114514).toTerm)
  }

  test("let expr") {
    val code = {
      """
        let x: Int = 1
        x
      """
    }
    val (expr, _) = synthCodeBlock(code)
    expr.normalize should be (IntValue(1).toTerm)
  }

  test("let apply") {
    val code = {
      """
        let f: Int -> Int = (x: Int) => x
        f(114514)
      """
    }
    val (expr, _) = synthCodeBlock(code)
    expr.normalize should be (IntValue(114514).toTerm)
  }

  test("if expr") {
    val (expr, _) = synthCodeBlock("if true then 114 else 514")
    expr.normalize should be (IntValue(114).toTerm)
  }

  test("pattern match") {
    val code = {
      """
        match 514 {
          case 114 => 1919
          case 514 => 810
        }
      """
    }
    val (expr, _) = synthCodeBlock(code)
    expr.normalize should be (IntValue(810).toTerm)
  }

  test("wildcard pattern match") {
    val code = {
      """
        match 514 {
          case 114 => 1919
          case _ => 810
        }
      """
    }
    val (expr, _) = synthCodeBlock(code)
    expr.normalize should be (IntValue(810).toTerm)
  }

  test("panic test") {
    val code = {
      """
        match 114 {
          case 114 => 514
          case _ => panic("test")
        }
      """
    }
    val (expr, _) = synthCodeBlock(code)
    expr.normalize should be (IntValue(514).toTerm)
  }

//  test("eq refl") {
//    val code = {
//      """
//        let eq = (A: 'Type, a b: A): 'Type => ∀(P: A -> 'Type) -> P(a) -> P(b)
//        let refl = (A: 'Type, a: A): eq(A, a, a) => (P: A -> 'Type, pa: P(a)) => pa
//        let symmetry = (A: 'Type, a b: A, e: eq(A, a, b)): eq(A, b, a) => e((b: A) => eq(A, b, a), refl(A, a))
//      """
//    }
//    synthCodeBlock(code)
//  }
}
