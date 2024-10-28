package saki.core.domain

import saki.core.context.{Environment, Typed}
import saki.core.syntax.*
import saki.core.{RuntimeEntity, RuntimeEntityFactory}
import saki.error.CoreErrorKind.*

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
  ) extends Value with OverloadedLambdaLike[OverloadedPi]

  case Sigma(paramType: Type, closure: Value => Value)

  case Lambda(paramType: Type, body: Value => Value)

  case OverloadedLambda(
    states: Map[Type, Value => Value]
  ) extends Value with OverloadedLambdaLike[OverloadedLambda]

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

  override def eval(implicit env: Environment.Typed[Value]): Value = this

  override def infer(implicit env: Environment.Typed[Value]): Type = this match {
    case Neutral(NeutralValue.Variable(_, ty)) => ty
    case _ => this.readBack.infer
  }

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

  def isFinal(variables: Set[Var.Local] = Set.empty)(
    implicit env: Environment.Typed[Value]
  ): Boolean = this match {
    case Neutral(neutral) => neutral.isFinal(variables)
    case Pi(ty, closure) => {
      val paramIdent = env.uniqueVariable("%")
      closure(Value.variable(paramIdent, ty)).isFinal(variables + paramIdent)
    }
    case Sigma(ty, closure) => {
      val paramIdent = env.uniqueVariable("%")
      closure(Value.variable(paramIdent, ty)).isFinal(variables + paramIdent)
    }
    case Lambda(ty, closure) => {
      val paramIdent = env.uniqueVariable("%")
      closure(Value.variable(paramIdent, ty)).isFinal(variables + paramIdent)
    }
    case overloaded: OverloadedLambdaLike[?] => overloaded.isStatesFinal(variables)
    case Record(fields) => fields.valuesIterator.forall(_.isFinal(variables))
    case RecordType(fields) => fields.valuesIterator.forall(_.isFinal(variables))
    case InductiveType(_, args) => args.forall(_.isFinal(variables))
    case InductiveVariant(inductiveType, _, args) => inductiveType.isFinal(variables) && args.forall(_.isFinal(variables))
    case _ => true
  }

  infix def unify(that: Type)(implicit env: Environment.Typed[Value]): Boolean = (this, that) match {
    case (Universe, Universe) => true
    case (Primitive(value1), Primitive(value2)) => value1 == value2
    case (PrimitiveType(ty1), PrimitiveType(ty2)) => ty1 == ty2
    case (Neutral(value1), Neutral(value2)) => value1 unify value2
    case (Pi(paramType1, closure1), Pi(paramType2, closure2)) => {
      val paramIdent = env.uniqueVariable("%")
      val param1 = Value.variable(paramIdent, paramType1)
      val param2 = Value.variable(paramIdent, paramType2)
      (paramType1 unify paramType2) && (closure1(param1) unify closure2(param2))
    }
    case (Sigma(paramType1, closure1), Sigma(paramType2, closure2)) => {
      val paramIdent = env.uniqueVariable("%")
      val param1 = Value.variable(paramIdent, paramType1)
      val param2 = Value.variable(paramIdent, paramType2)
      (paramType1 unify paramType2) && (closure1(param1) unify closure2(param2))
    }
    case (Lambda(paramType1, body1), Lambda(paramType2, body2)) => {
      val paramIdent = env.uniqueVariable("%")
      val param1 = Value.variable(paramIdent, paramType1)
      val param2 = Value.variable(paramIdent, paramType2)
      (paramType1 unify paramType2) && (body1(param1) unify body2(param2))
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
  ): Boolean = (this.readBack.forceEval, that.readBack.forceEval) match {

    case (lhs, rhs) if lhs == rhs || (lhs unify rhs) => true

    case (PrimitiveType(LiteralType.AnyType), _) => true

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
        val paramIdent = env.uniqueVariable("%")
        val param1 = Value.variable(paramIdent, paramType1)
        val param2 = Value.variable(paramIdent, paramType2)
        closure1(param1) <:< closure2(param2)
      }
    }

    // Sigma type subtyping: Covariant in both parameter type and dependent type
    case (Sigma(paramType1, closure1), Sigma(paramType2, closure2)) => {
      paramType1 <:< paramType2 && {
        val paramIdent = env.uniqueVariable("%")
        val param1 = Value.variable(paramIdent, paramType1)
        val param2 = Value.variable(paramIdent, paramType2)
        closure1(param1) <:< closure2(param2)
      }
    }

    // Lambda types must match in parameter type and be subtypes in their bodies
    case (Lambda(paramType1, body1), Lambda(paramType2, body2)) => {
      paramType1 <:< paramType2 && {
        val paramIdent = env.uniqueVariable("%")
        val param1 = Value.variable(paramIdent, paramType1)
        val param2 = Value.variable(paramIdent, paramType2)
        body1(param1) <:< body2(param2)
      }
    }

    // Record subtyping: all fields in `this` must be present in `that` and be subtypes
    case (Record(fields1), Record(fields2)) => {
      fields1.forall { case (name, value1) => fields2.get(name).exists(value1 <:< _) }
    }

    // RecordType subtyping: all fields in `that` must be present in `this` and be subtypes
    case (RecordType(fields1), RecordType(fields2)) => {
      fields1.forall { case (name, ty1) => fields2.get(name).exists(ty1 <:< _) }
    }

    // Inductive type subtyping: inductive types must match and arguments must be subtypes
    case (InductiveType(inductive1, args1), InductiveType(inductive2, args2)) => {
      inductive1 == inductive2 && args1.zip(args2).forall { case (arg1, arg2) => arg1 <:< arg2 }
    }

    // Inductive variant subtyping: constructors must match, and arguments must be subtypes
    case (InductiveVariant(inductive1, cons1, args1), InductiveVariant(inductive2, cons2, args2)) => {
      (inductive1.asInstanceOf[Type] <:< inductive2.asInstanceOf[Type]) && cons1 == cons2 &&
      args1.zip(args2).forall { case (arg1, arg2) => arg1 <:< arg2 }
    }

    // Default case: no subtyping relationship
    case _ => false
  }

  infix def leastUpperBound(that: Type)(implicit env: Environment.Typed[Value]): Type = (this, that) match {

    // Case where both types are equal: the LUB is the type itself
    case (t1, t2) if t1 == t2 => t1

    // If one of the types is Any, the LUB is Any
    case (_, PrimitiveType(LiteralType.AnyType)) => PrimitiveType(LiteralType.AnyType)
    case (PrimitiveType(LiteralType.AnyType), _) => PrimitiveType(LiteralType.AnyType)

    // If one of the types is Nothing, the LUB is the other type
    case (t1, PrimitiveType(LiteralType.NothingType)) => t1
    case (PrimitiveType(LiteralType.NothingType), t2) => t2

    // LUB for Universe levels: choose the larger Universe level
    case (Value.Universe, Value.Universe) => Value.Universe

    // LUB for Primitive types: If they match, return it, otherwise no common LUB
    case (Value.PrimitiveType(t1), Value.PrimitiveType(t2)) => {
      if (t1 == t2) Value.PrimitiveType(t1)
      else NoLeastUpperBound.raise {
        s"No least upper bound for incompatible primitive types: $t1 and $t2"
      }
    }

    // LUB for Pi types: contravariant parameter type, covariant return type
    case (Value.Pi(paramType1, closure1), Value.Pi(paramType2, closure2)) => {
      val paramLub = paramType2 leastUpperBound paramType1 // Contravariant parameter type
      val paramIdent = env.uniqueVariable("%")
      val param1 = Value.variable(paramIdent, paramType1)
      val param2 = Value.variable(paramIdent, paramType2)
      val returnLub = closure1(param1) leastUpperBound closure2(param2) // Covariant return type
      Value.Pi(paramLub, _ => returnLub)
    }

    // LUB for Sigma types: covariant parameter and closure type
    case (Value.Sigma(paramType1, closure1), Value.Sigma(paramType2, closure2)) => {
      val paramLub = paramType1 leastUpperBound paramType2 // Covariant parameter
      val paramIdent = env.uniqueVariable("%")
      val param1 = Value.variable(paramIdent, paramType1)
      val param2 = Value.variable(paramIdent, paramType2)
      val closureLub = closure1(param1) leastUpperBound closure2(param2) // Covariant closure
      Value.Sigma(paramLub, _ => closureLub)
    }

    // LUB for Lambda values: parameter types must match, and take the LUB of bodies
    case (Value.Lambda(paramType1, body1), Value.Lambda(paramType2, body2)) => {
      val paramLub = paramType1 leastUpperBound paramType2 // Parameters must match
      val paramIdent = env.uniqueVariable("%")
      val param1 = Value.variable(paramIdent, paramType1)
      val param2 = Value.variable(paramIdent, paramType2)
      val bodyLub = body1(param1) leastUpperBound body2(param2) // LUB of bodies
      Value.Lambda(paramLub, _ => bodyLub)
    }

    // LUB for Records: combine fields if types match
    case (Value.Record(fields1), Value.Record(fields2)) => {
      val commonFields = fields1.keySet.intersect(fields2.keySet)
      val lubFields = commonFields.map { key =>
        key -> (fields1(key) leastUpperBound fields2(key))
      }.toMap
      Value.Record(lubFields)
    }

    // LUB for Record Types: similar strategy to records, fields must be present in both and have a LUB
    case (Value.RecordType(fields1), Value.RecordType(fields2)) => {
      val commonFields = fields1.keySet.intersect(fields2.keySet)
      val lubFields = commonFields.map { key =>
        key -> (fields1(key) leastUpperBound fields2(key))
      }.toMap
      Value.RecordType(lubFields)
    }

    // LUB for Inductive Types: if they match, take LUB of arguments
    case (InductiveType(ind1, args1), InductiveType(ind2, args2)) if ind1.name == ind2.name => {
      val lubArgs = args1.zip(args2).map { case (arg1, arg2) => arg1 leastUpperBound arg2 }
      Value.InductiveType(ind1, lubArgs)
    }

    // LUB for Inductive Variants: constructors and types must match, arguments need LUB
    case (
      Value.InductiveVariant(ind1, cons1, args1),
      Value.InductiveVariant(ind2, cons2, args2),
    ) if (ind1 unify ind2) && cons1.ident == cons2.ident => {
      val lubArgs = args1.zip(args2).map { case (arg1, arg2) => arg1 leastUpperBound arg2 }
      Value.InductiveVariant(ind1, cons1, lubArgs)
    }

    // No common LUB for incompatible types
    case _ => NoLeastUpperBound.raise {
      s"No least upper bound exists between: $this and $that"
    }
  }

  @deprecatedOverriding("For debugging purposes only, don't call it in production code")
  override def toString: String = try this.readBack(Environment.Typed.empty).toString catch {
    case _: Throwable => super.toString
  }
}

object Value extends RuntimeEntityFactory[Value] {

  override def unit: Type = Value.Primitive(Literal.UnitValue)

  override def unitType: Type = Value.PrimitiveType(LiteralType.UnitType)

  override def universe: Type = Universe

  override def variable(ident: Var.Local, ty: Type): Value = Neutral(NeutralValue.Variable(ident, ty))

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
    case _ => TypeNotMatch.raise("Expected inductive type")
  }

  /**
   * Read back a closure to a lambda term with a parameter
   *
   * @param paramType The type of the parameter
   * @param closure   The closure to read back
   * @param env       The environment to use for reading back
   * @return 1. The parameter of the lambda term
   *         2. The body of the lambda term
   * @see [[Term.evalParameterized]]
   */
  private[core] def readBackClosure(paramType: Type, closure: Value => Value)(
    implicit env: Environment.Typed[Value]
  ): (Param[Term], Term) = {
    val paramIdent = env.uniqueVariable
    val paramVariable = Value.variable(paramIdent, paramType)
    env.withLocal(paramIdent, paramVariable, paramType) { implicit env =>
      val term = closure(paramVariable).readBack(env)
      (Param(paramIdent, paramType.readBack), term)
    }
  }

  extension (self: InductiveType) {
    def argsMap(implicit env: Environment.Typed[Value]): Map[Var.Local, Typed[Type]] = {
      self.args.zip(self.inductive.definition.get.params).map { (arg, param) =>
        param.ident -> Typed(arg, param.`type`.eval)
      }.toMap
    }
  }

}

private sealed trait OverloadedLambdaLike[S <: OverloadedLambdaLike[S] & Value] {

  def states: Map[Type, Value => Value]

  def readBackStates(implicit env: Environment.Typed[Value]): Map[Param[Term], Term] = {
    states.map { (paramType, closure) =>
      val paramIdent = env.uniqueVariable
      val param = Value.variable(paramIdent, paramType)
      env.withLocal(paramIdent, param, paramType) { implicit env =>
        val body = closure(param).readBack
        (Param(paramIdent, paramType.readBack), body)
      }
    }
  }

  def isStatesFinal(variables: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean = {
    states.forall { (paramType, closure) =>
      val paramIdent = env.uniqueVariable
      val param = Value.variable(paramIdent, paramType)
      closure(param).isFinal(variables + paramIdent)
    }
  }

  def applyArgument(
    arg: Value, argType: Type,
    constructor: Map[Type, Value => Value] => S,
    unwrapStates: Value => Map[Type, Value => Value],
  )(implicit env: Environment.Typed[Value]): Value = {

    val candidateStates = states.filter {
      case (paramType, _) => paramType <:< argType
    }

    if candidateStates.isEmpty then NoSuchOverloading.raise {
      s"No overloading found for argument ${arg.readBack} with type: ${argType.readBack}"
    }

    if candidateStates.size == 1 then {
      // If there is only one state that matches the argument type, evaluate it
      val (_, closure) = candidateStates.head
      closure(arg)
    } else {
      // If there are multiple states that match the argument type, evaluate all of them
      // Evaluate each state and merge the results
      val newStates = candidateStates.flatMap { (_, closure) =>
        // Since the parameter type is checked to be a subtype of the argument type,
        // we don't need to check the type of the argument value again
        unwrapStates(closure(arg))
      }
      // Merge the new states by constructing a new overloaded type
      constructor(newStates)
    }
  }
}

private[core] def neutralClosure(
  param: Param[Value], action: Environment.Typed[Value] => Value, env: Environment.Typed[Value],
)(arg: Value): Value = env.withLocal(param.ident, Typed[Value](arg, param.`type`)) {
  implicit env => action(env)
}
