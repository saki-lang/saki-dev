package saki.core.domain

import saki.core.context.Environment
import saki.core.syntax.*
import saki.core.{RuntimeEntity, RuntimeEntityFactory, TypeError}

import scala.annotation.targetName
import scala.collection.Seq

type Type = Value

enum Value extends RuntimeEntity[Type] {

  case Universe

  case Primitive(value: Literal)

  case PrimitiveType(`type`: LiteralType)

  case Neutral(value: NeutralValue)

  case Pi(paramType: Type, closure: Value => Value)

  case OverloadedPi(
    states: Map[Type, Value => Value]
  ) extends Value with OverloadedLambdaLike

  case Sigma(paramType: Type, closure: Value => Value)

  case Lambda(paramType: Type, body: Value => Value)

  case OverloadedLambda(
    states: Map[Type, Value => Value]
  ) extends Value with OverloadedLambdaLike

  case Record(fields: Map[String, Value])

  case RecordType(fields: Map[String, Type])

  case InductiveType(
    inductive: Var.Defined[Term, Inductive],
    args: Seq[Value],
  )

  case InductiveVariant(
    inductive: InductiveType,
    constructor: Constructor[Term],
    args: Seq[Value],
  )

  override def infer(implicit env: Environment.Typed[Value]): Type = this.readBack.infer

  def readBack(implicit env: Environment.Typed[Value]): Term = this match {

    case Universe => Term.Universe

    case Primitive(value) => Term.Primitive(value)

    case PrimitiveType(ty) => Term.PrimitiveType(ty)

    case Neutral(value) => value.readBack

    case Pi(paramType, codomainClosure) => {
      val (param, codomain) = Value.readBackClosure(paramType, codomainClosure)
      Term.Pi(param, codomain)
    }

    case pi: OverloadedPi => Term.OverloadedPi(pi.readBackStates)

    case Sigma(paramType, codomainClosure) => {
      val (param, codomain) = Value.readBackClosure(paramType, codomainClosure)
      Term.Sigma(param, codomain)
    }

    case Lambda(paramType, bodyClosure) => {
      val (param, body) = Value.readBackClosure(paramType, bodyClosure)
      Term.Lambda(param, body)
    }

    case lambda: OverloadedLambda => Term.OverloadedLambda(lambda.readBackStates)

    case Record(fields) => Term.Record(fields.map((name, value) => (name, value.readBack)))

    case RecordType(fields) => Term.RecordType(fields.map((name, ty) => (name, ty.readBack)))

    case InductiveType(inductive, args) => {
      Term.inductiveType(inductive, args.map(_.readBack))
    }

    case InductiveVariant(inductive, constructor, args) => {
      Term.inductiveVariant(inductive.readBack, constructor, args.map(_.readBack))
    }
  }
  
  infix def unify(that: Type)(implicit env: Environment.Typed[Value]): Boolean = (this, that) match {
    case (Universe, Universe) => true
    case (Primitive(value1), Primitive(value2)) => value1 == value2
    case (PrimitiveType(ty1), PrimitiveType(ty2)) => ty1 == ty2
    case (Neutral(value1), Neutral(value2)) => value1 unify value2
    case (Pi(paramType1, closure1), Pi(paramType2, closure2)) => {
      val param = Value.variable(env.uniqueVariable)
      (paramType1 unify paramType2) && (closure1(param) unify closure2(param))
    }
    case (Sigma(paramType1, closure1), Sigma(paramType2, closure2)) => {
      val param = Value.variable(env.uniqueVariable)
      (paramType1 unify paramType2) && (closure1(param) unify closure2(param))
    }
    case (Lambda(paramType1, body1), Lambda(paramType2, body2)) => {
      val param = Value.variable(env.uniqueVariable)
      (paramType1 unify paramType2) && (body1(param) unify body2(param))
    }
    case (Record(fields1), Record(fields2)) => {
      fields1.size == fields2.size &&
      fields1.forall { case (name, value1) => fields2.get(name).exists(value1.unify) }
    }
    case (RecordType(fields1), RecordType(fields2)) => {
      fields1.size == fields2.size &&
      fields1.forall { case (name, ty1) => fields2.get(name).exists(ty1.unify) }
    }
    case (InductiveType(inductive1, args1), InductiveType(inductive2, args2)) => {
      inductive1 == inductive2 && args1.zip(args2).forall((arg1, arg2) => arg1 unify arg2)
    }
    case (InductiveVariant(inductive1, constructor1, args1), InductiveVariant(inductive2, constructor2, args2)) => {
      (inductive1 unify inductive2) && constructor1 == constructor2 &&
      (args1.zip(args2).forall((arg1, arg2) => arg1 unify arg2))
    }
    case _ => false
  }

  @targetName("subtype")
  infix def <:<(that: Type)(
    implicit env: Environment.Typed[Value]
  ): Boolean = (this, that) match {

    // Universe levels are considered cumulative
    case (Universe, Universe) => true

    // Primitive types and values must match exactly for subtyping
    case (Primitive(value1), Primitive(value2)) => value1 == value2
    case (PrimitiveType(ty1), PrimitiveType(ty2)) => ty1 == ty2

    // Neutral values
    case (Neutral(value1), Neutral(value2)) => value1 <:< value2

    // Function type (Pi type) subtyping: Covariant in return type, contravariant in parameter type
    case (Pi(paramType1, closure1), Pi(paramType2, closure2)) => {
      paramType2 <:< paramType1 && {
        val param = Value.variable(env.uniqueVariable)
        closure1(param) <:< closure2(param)
      }
    }

    // Sigma type subtyping: Covariant in both parameter type and dependent type
    case (Sigma(paramType1, closure1), Sigma(paramType2, closure2)) => {
      paramType1 <:< paramType2 && {
        val param = Value.variable(env.uniqueVariable)
        closure1(param) <:< closure2(param)
      }
    }

    // Lambda types must match in parameter type and be subtypes in their bodies
    case (Lambda(paramType1, body1), Lambda(paramType2, body2)) => {
      paramType1 <:< paramType2 && {
        val param = Value.variable(env.uniqueVariable)
        body1(param) <:< body2(param)
      }
    }

    // Record subtyping: all fields in `that` must be present in `this` and be subtypes
    case (Record(fields1), Record(fields2)) => {
      fields2.forall { case (name, value2) => fields1.get(name).exists(_ <:< value2) }
    }

    // RecordType subtyping: all fields in `that` must be present in `this` and be subtypes
    case (RecordType(fields1), RecordType(fields2)) => {
      fields2.forall { case (name, ty2) => fields1.get(name).exists(_ <:< ty2) }
    }

    // Inductive type subtyping: inductive types must match and arguments must be subtypes
    case (InductiveType(inductive1, args1), InductiveType(inductive2, args2)) => {
      inductive1 == inductive2 && args1.zip(args2).forall { case (arg1, arg2) => arg1 <:< arg2 }
    }

    // Inductive variant subtyping: constructors must match, and arguments must be subtypes
    case (InductiveVariant(inductive1, cons1, args1), InductiveVariant(inductive2, cons2, args2)) => {
      (inductive1 <:< inductive2) && cons1 == cons2 &&
      args1.zip(args2).forall { case (arg1, arg2) => arg1 <:< arg2 }
    }

    // Default case: no subtyping relationship
    case _ => false
  }

}

object Value extends RuntimeEntityFactory[Value] {

  override def unit: Type = Value.Primitive(Literal.UnitValue)

  override def unitType: Type = Value.PrimitiveType(LiteralType.UnitType)

  override def universe: Type = Universe

  override def variable(ident: Var.Local): Value = Neutral(NeutralValue.Variable(ident))

  override def functionInvoke(function: Var.Defined[Term, Function], args: Seq[Type]): Type = {
    Neutral(NeutralValue.FunctionInvoke(function, args))
  }

  override def inductiveType(inductive: Var.Defined[Term, Inductive], args: Seq[Type]): Type = {
    Value.InductiveType(inductive, args)
  }

  override def inductiveVariant(
    inductive: Value,
    constructor: Constructor[Term],
    args: Seq[Value],
  ): Type = inductive match {
    case inductive: InductiveType => Value.InductiveVariant(inductive, constructor, args)
    case _ => TypeError.error("Expected inductive type")
  }

  /**
   * Read back a closure to a lambda term with a parameter
   *
   * @param paramType The type of the parameter
   * @param closure The closure to read back
   * @param env The environment to use for reading back
   * @return 1. The parameter of the lambda term
   *         2. The body of the lambda term
   * @see [[Term.evalParameterized]]
   */
  private[core] def readBackClosure(paramType: Type, closure: Value => Value)(
    implicit env: Environment.Typed[Value]
  ): (Param[Term], Term) = {
    val paramIdent = env.uniqueVariable
    val param = Value.variable(paramIdent)
    val term = env.withLocal(paramIdent, param, paramType) {
      closure(Value.variable(paramIdent)).readBack
    }
    (Param(paramIdent, paramType.readBack), term)
  }

}

private sealed trait OverloadedLambdaLike {

  def states: Map[Type, Value => Value]

  def readBackStates(implicit env: Environment.Typed[Value]): Map[Param[Term], Term] = {
    states.map { (paramType, closure) =>
      val paramIdent = env.uniqueVariable
      val param = Value.variable(paramIdent)
      val body = env.withLocal(paramIdent, param, paramType) {
        closure(param).readBack
      }
      (Param(paramIdent, paramType.readBack), body)
    }
  }
}

