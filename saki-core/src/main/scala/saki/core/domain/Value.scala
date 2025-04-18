package saki.core.domain

import saki.core.{RuntimeEntity, RuntimeEntityFactory}
import saki.core.context.{Environment, Typed}
import saki.core.syntax.*
import saki.core.syntax
import saki.core.term
import saki.core.term.Term
import saki.error.CoreErrorKind.*

import scala.annotation.targetName
import scala.collection.Seq

type Type = Value

enum Value extends RuntimeEntity[Type] {

  case Universe

  case Primitive(value: Literal)

  case PrimitiveType(`type`: LiteralType)

  case Neutral(value: NeutralValue)

  case Union(types: Set[Type])

  case Intersection(types: Set[Type])

  case Pi(paramType: Type, closure: CodomainClosure)

  case OverloadedPi(
    states: Map[Type, CodomainClosure]
  ) extends Value with OverloadedLambdaLike[OverloadedPi]

  case Sigma(paramType: Type, closure: CodomainClosure)

  case Lambda(paramType: Type, body: CodomainClosure)

  case OverloadedLambda(
    states: Map[Type, CodomainClosure]
  ) extends Value with OverloadedLambdaLike[OverloadedLambda]

  case Pair(first: Value, second: Value)

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

    case Universe => Universe

    case Primitive(value) => PrimitiveType(value.ty)

    case PrimitiveType(_) => Universe

    case Neutral(neutral) => neutral.infer

    case Union(types) => types.map(_.infer).reduce(_ \/ _)

    case Intersection(types) => types.map(_.infer).reduce(_ /\ _)

    case Pi(_, _) => Universe

    case OverloadedPi(_) => Universe

    case Sigma(_, _) => Universe

    case Lambda(paramType, body) => {
      val bodyType = env.withNewUnique(paramType) {
        (env, ident, ty) => body(Value.variable(ident, ty)).infer(env)
      }
      Pi(paramType, _ => bodyType)
    }

    case OverloadedLambda(states) => Value.OverloadedPi(states.map { (paramType, body) =>
      val bodyType = env.withNewUnique(paramType) {
        (env, ident, ty) => body(Value.variable(ident, ty)).infer(env)
      }
      (paramType, _ => bodyType): (Type, CodomainClosure)
    })
    case Pair(first, second) => Sigma(first.infer, _ => second.infer)
    case Record(fields) => RecordType(fields.map((name, value) => (name, value.infer)))
    case RecordType(_) => Universe
    case InductiveType(_, _) => Universe
    case InductiveVariant(inductiveType, _, _) => inductiveType
  }

  def isNeutral: Boolean = this match {
    case Neutral(_) => true
    case _ => false
  }

  def reflect(implicit env: Environment.Typed[Value]): Term = this match {

    case Universe => term.Universe

    case Primitive(value) => term.Primitive(value)

    case PrimitiveType(ty) => term.PrimitiveType(ty)

    case Neutral(value) => value.reflect

    case Union(types) => term.Union(types.map(_.reflect))

    case Intersection(types) => term.Intersection(types.map(_.reflect))

    case Pi(paramType, codomainClosure) => {
      val (param, codomain) = Value.readBackClosure(paramType, codomainClosure)
      term.Pi(param, codomain)
    }

    case pi: OverloadedPi => term.OverloadedPi(pi.readBackStates)

    case Sigma(paramType, codomainClosure) => {
      val (param, codomain) = Value.readBackClosure(paramType, codomainClosure)
      term.Sigma(param, codomain)
    }

    case Lambda(paramType, bodyClosure) => {
      val (param, body) = Value.readBackClosure(paramType, bodyClosure)
      term.Lambda(param, body)
    }

    case lambda: OverloadedLambda => term.OverloadedLambda(lambda.readBackStates)

    case Pair(first, second) => term.Pair(first.reflect, second.reflect)

    case Record(fields) => term.Record(fields.map((name, value) => (name, value.reflect)))

    case RecordType(fields) => term.RecordType(fields.map((name, ty) => (name, ty.reflect)))

    case InductiveType(inductive, args) => {
      Term.inductiveType(inductive, args.map(_.reflect))
    }

    case InductiveVariant(inductive, constructor, args) => {
      Term.inductiveVariant(inductive.reflect, constructor, args.map(_.reflect))
    }
  }

  def containsMatching(implicit env: Environment.Typed[Value]): Boolean = this match {
    case Universe | Primitive(_) | PrimitiveType(_) => false
    case Neutral(neutral) => neutral.containsMatching
    case Union(types) => types.exists(_.containsMatching)
    case Intersection(types) => types.exists(_.containsMatching)
    case Pi(paramType, closure) => {
      env.withNewUnique(paramType) { (env, ident, ty) =>
        closure(Value.variable(ident, ty)).containsMatching(env)
      }
    }
    case Sigma(paramType, closure) => {
      env.withNewUnique(paramType) { (env, ident, ty) =>
        closure(Value.variable(ident, ty)).containsMatching(env)
      }
    }
    case Lambda(paramType, closure) => {
      env.withNewUnique(paramType) { (env, ident, ty) =>
        closure(Value.variable(ident, ty)).containsMatching(env)
      }
    }
    case overloaded: OverloadedLambdaLike[?] => {
      // TODO: Structures like this should be refactored to put the variable in the environment
      overloaded.states.forall { (paramType, closure) =>
        env.withNewUnique(paramType) { (env, ident, ty) =>
          closure(Value.variable(ident, ty)).containsMatching(env)
        }
      }
    }
    case Pair(first, second) => first.containsMatching || second.containsMatching
    case Record(fields) => fields.valuesIterator.exists(_.containsMatching)
    case RecordType(fields) => fields.valuesIterator.exists(_.containsMatching)
    case InductiveType(_, args) => args.exists(_.containsMatching)
    case InductiveVariant(_, _, args) => args.exists(_.containsMatching)
  }

  def isFinal(variables: Set[Var.Local] = Set.empty)(
    implicit env: Environment.Typed[Value]
  ): Boolean = this match {
    case Neutral(neutral) => neutral.isFinal(variables)
    case Pi(paramType, closure) => {
      env.withNewUnique(paramType) { (env, ident, ty) =>
        closure(Value.variable(ident, ty)).isFinal(variables + ident)(env)
      }
    }
    case Sigma(paramType, closure) => {
      env.withNewUnique(paramType) { (env, ident, ty) =>
        closure(Value.variable(ident, ty)).isFinal(variables + ident)(env)
      }
    }
    case Lambda(paramType, closure) => {
      env.withNewUnique(paramType) { (env, ident, ty) =>
        closure(Value.variable(ident, ty)).isFinal(variables + ident)(env)
      }
    }
    case overloaded: OverloadedLambdaLike[?] => overloaded.isStatesFinal(variables)
    case Record(fields) => fields.valuesIterator.forall(_.isFinal(variables))
    case RecordType(fields) => fields.valuesIterator.forall(_.isFinal(variables))
    case InductiveType(_, args) => args.forall(_.isFinal(variables))
    case InductiveVariant(inductiveType, _, args) => inductiveType.isFinal(variables) && args.forall(_.isFinal(variables))
    case _ => true
  }

  infix def unify(that: Type)(implicit env: Environment.Typed[Value]): Boolean = (this, that) match {

    case (lhs, rhs) if lhs == rhs => true

    case (Universe, Universe) => true

    case (Primitive(value1), Primitive(value2)) => value1 == value2

    case (PrimitiveType(ty1), PrimitiveType(ty2)) => ty1 == ty2

    case (Neutral(value1), Neutral(value2)) => value1 unify value2

    case (Pi(paramType1, closure1), Pi(paramType2, closure2)) => {
      (paramType1 unify paramType2) && env.withNewUnique(paramType1) {
        implicit (env, ident, ty) => {
          given Environment.Typed[Value] = env
          val neutral = Value.variable(ident, ty)
          closure1(neutral) unify closure2(neutral)
        }
      }
    }

    case (Sigma(paramType1, closure1), Sigma(paramType2, closure2)) => {
      (paramType1 unify paramType2) && env.withNewUnique(paramType1) {
        implicit (env, ident, ty) => {
          given Environment.Typed[Value] = env
          val neutral = Value.variable(ident, ty)
          closure1(neutral) unify closure2(neutral)
        }
      }
    }

    case (Lambda(paramType1, body1), Lambda(paramType2, body2)) => {
      (paramType1 unify paramType2) && env.withNewUnique(paramType1) {
        implicit (env, ident, ty) => {
          given Environment.Typed[Value] = env
          val neutral = Value.variable(ident, ty)
          body1(neutral) unify body2(neutral)
        }
      }
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

    case (lhs, rhs) if lhs == rhs || (lhs unify rhs) => true

    case (PrimitiveType(LiteralType.AnyType), _) => true

    // Universe levels are considered cumulative
    case (Universe, Universe) => true

    // Primitive types and values must match exactly for subtyping
    case (Primitive(value1), Primitive(value2)) => value1 == value2
    case (PrimitiveType(ty1), PrimitiveType(ty2)) => ty1 == ty2

    // Neutral values
    case (Neutral(value1), Neutral(value2)) => value1 <:< value2

    case (Union(types1), Union(types2)) => types1.forall(t1 => types2.exists(t1 <:< _))

    case (Union(types), rhs) => types.exists(_ <:< rhs)

    case (lhs, Union(types)) => types.forall(lhs <:< _)

    // Function type (Pi type) subtyping: Covariant in return type, contravariant in parameter type
    case (Pi(paramType1, closure1), Pi(paramType2, closure2)) => {
      paramType2 <:< paramType1 && env.withNewUnique(paramType2 \/ paramType1) {
        implicit (env, ident, ty) => {
          given Environment.Typed[Value] = env
          val neutral = Value.variable(ident, ty)
          closure1(neutral) <:< closure2(neutral)
        }
      }
    }

    // Sigma type subtyping: Covariant in both parameter type and dependent type
    case (Sigma(paramType1, closure1), Sigma(paramType2, closure2)) => {
      paramType1 <:< paramType2 && env.withNewUnique(paramType1 \/ paramType2) {
        implicit (env, ident, ty) => {
          given Environment.Typed[Value] = env
          val neutral = Value.variable(ident, ty)
          closure1(neutral) <:< closure2(neutral)
        }
      }
    }

    // Lambda types must match in parameter type and be subtypes in their bodies
    case (Lambda(paramType1, body1), Lambda(paramType2, body2)) => {
      paramType1 <:< paramType2 && env.withNewUnique(paramType1 \/ paramType2) {
        implicit (env, ident, ty) => {
          given Environment.Typed[Value] = env
          val neutral = Value.variable(ident, ty)
          body1(neutral) <:< body2(neutral)
        }
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

  @targetName("leastUpperBound")
  infix def \/(that: Type)(implicit env: Environment.Typed[Value]): Type = (this, that) match {

    // Case where both types are equal: the LUB is the type itself
    case (t1, t2) if t1 unify t2 => t1

    // If one of the types is Any, the LUB is Any
    case (_, PrimitiveType(LiteralType.AnyType)) => PrimitiveType(LiteralType.AnyType)
    case (PrimitiveType(LiteralType.AnyType), _) => PrimitiveType(LiteralType.AnyType)

    // If one of the types is Nothing, the LUB is the other type
    case (t1, PrimitiveType(LiteralType.NothingType)) => t1
    case (PrimitiveType(LiteralType.NothingType), t2) => t2

    // LUB for Universe levels: choose the larger Universe level
    case (Value.Universe, Value.Universe) => Value.Universe

    // LUB for Primitive types: If they match, return it, otherwise no common LUB
    case (Value.PrimitiveType(t1), Value.PrimitiveType(t2)) if t1 == t2 => {
      Value.PrimitiveType(t1)
    }

    case (Value.Union(types1), Value.Union(types2)) => (types1 ++ types2).reduce(_ \/ _)

    case (lhs, Value.Union(types)) => {
      if types.exists(lhs <:< _) then lhs
      else Value.Union(types + lhs)
    }

    case (Value.Union(types), rhs) => {
      if types.exists(_ <:< rhs) then Value.Union(types)
      else Value.Union(types + rhs)
    }

    // LUB for Pi types: contravariant parameter type, covariant return type
    case (Value.Pi(paramType1, closure1), Value.Pi(paramType2, closure2)) => {
      val paramLub = paramType1 /\ paramType2 // Contravariant parameter type
      val return1 = env.withNewUnique(paramType1) {
        implicit (env, ident, ty) => closure1(Value.variable(ident, ty))
      }
      val return2 = env.withNewUnique(paramType2) {
        implicit (env, ident, ty) => closure2(Value.variable(ident, ty))
      }
      Value.Pi(paramLub, _ => return1 \/ return2) // Covariant return type
    }

    // LUB for Sigma types: covariant parameter and closure type
    case (Value.Sigma(paramType1, closure1), Value.Sigma(paramType2, closure2)) => {
      val paramLub = paramType1 \/ paramType2 // Covariant parameter
      val return1 = env.withNewUnique(paramType1) {
        implicit (env, ident, ty) => closure1(Value.variable(ident, ty))
      }
      val return2 = env.withNewUnique(paramType2) {
        implicit (env, ident, ty) => closure2(Value.variable(ident, ty))
      }
      Value.Sigma(paramLub, _ => return1 \/ return2)
    }

    // LUB for Lambda values: parameter types must match, and take the LUB of bodies
    case (Value.Lambda(paramType1, body1), Value.Lambda(paramType2, body2)) => {
      val paramLub = paramType1 \/ paramType2 // Parameters must match
      val return1 = env.withNewUnique(paramType1) {
        implicit (env, ident, ty) => body1(Value.variable(ident, ty))
      }
      val return2 = env.withNewUnique(paramType2) {
        implicit (env, ident, ty) => body2(Value.variable(ident, ty))
      }
      Value.Lambda(paramLub, _ => return1 \/ return2)
    }

    // LUB for Records: combine fields if types match
    case (Value.Record(fields1), Value.Record(fields2)) => {
      val commonFields = fields1.keySet.intersect(fields2.keySet)
      val lubFields = commonFields.map { key =>
        key -> (fields1(key) \/ fields2(key))
      }.toMap
      Value.Record(lubFields ++ (fields1 -- commonFields) ++ (fields2 -- commonFields))
    }

    // LUB for Record Types: similar strategy to records, fields must be present in both and have a LUB
    case (Value.RecordType(fields1), Value.RecordType(fields2)) => {
      val commonFields = fields1.keySet.intersect(fields2.keySet)
      val lubFields = commonFields.map { key =>
        key -> (fields1(key) \/ fields2(key))
      }.toMap
      Value.RecordType(lubFields ++ (fields1 -- commonFields) ++ (fields2 -- commonFields))
    }

    // LUB for Inductive Types: if they match, take LUB of arguments
    case (InductiveType(ind1, args1), InductiveType(ind2, args2)) if ind1.name == ind2.name => {
      val lubArgs = args1.zip(args2).map { case (arg1, arg2) => arg1 \/ arg2 }
      Value.InductiveType(ind1, lubArgs)
    }

    // LUB for Inductive Variants: constructors and types must match, arguments need LUB
    case (
      Value.InductiveVariant(ind1, cons1, args1),
      Value.InductiveVariant(ind2, cons2, args2),
    ) if (ind1 unify ind2) && cons1.ident == cons2.ident => {
      val lubArgs = args1.zip(args2).map { case (arg1, arg2) => arg1 \/ arg2 }
      Value.InductiveVariant(ind1, cons1, lubArgs)
    }

    // No common LUB for incompatible types
    case (lhs, rhs) => Value.Union(Set(lhs, rhs))
  }

  @targetName("greatestLowerBound")
  infix def /\(that: Type)(implicit env: Environment.Typed[Value]): Type = (this, that) match {

    // Case where both types are equal: the GLB is the type itself
    case (t1, t2) if t1 unify t2 => t1

    // GLB involving Nothing: Nothing is the bottom element
    case (_, PrimitiveType(LiteralType.NothingType)) => PrimitiveType(LiteralType.NothingType)
    case (PrimitiveType(LiteralType.NothingType), _) => PrimitiveType(LiteralType.NothingType)

    // GLB for Universe levels: Universe is the top level, so we return Universe
    case (Value.Universe, Value.Universe) => Value.Universe

    // GLB for Primitive types: If they match, return it, otherwise return Nothing
    case (Value.PrimitiveType(t1), Value.PrimitiveType(t2)) if t1 == t2 => {
      Value.PrimitiveType(t1)
    }
    case (_: Value.PrimitiveType, _: Value.PrimitiveType) => PrimitiveType(LiteralType.NothingType)

    // GLB for Union types: take GLB for each combination of the elements
    case (Value.Union(types1), Value.Union(types2)) => {
      Value.Union(for (t1 <- types1; t2 <- types2) yield t1 /\ t2)
    }

    case (Value.Union(types), rhs) => {
      Value.Union(types.map(_ /\ rhs))
    }

    case (lhs, Value.Union(types)) => {
      Value.Union(types.map(lhs /\ _))
    }

    // GLB for Intersection types: merge the sets
    case (Value.Intersection(types1), Value.Intersection(types2)) => {
      Value.Intersection(types1 ++ types2)
    }

    case (lhs, Value.Intersection(types)) => {
      if types.forall(lhs <:< _) then lhs
      else Value.Intersection(types + lhs)
    }

    case (Value.Intersection(types), rhs) => {
      if types.forall(_ <:< rhs) then rhs
      else Value.Intersection(types + rhs)
    }

    // GLB for Pi types: contravariant parameter type, covariant return type
    case (Value.Pi(paramType1, closure1), Value.Pi(paramType2, closure2)) => {
      val paramGlb = paramType1 \/ paramType2 // Contravariant parameter type
      val return1 = env.withNewUnique(paramType1) {
        implicit (env, ident, ty) => closure1(Value.variable(ident, ty))
      }
      val return2 = env.withNewUnique(paramType2) {
        implicit (env, ident, ty) => closure2(Value.variable(ident, ty))
      }
      Value.Pi(paramGlb, _ => return1 /\ return2) // Covariant return type
    }

    // GLB for Sigma types: covariant parameter and closure type
    case (Value.Sigma(paramType1, closure1), Value.Sigma(paramType2, closure2)) => {
      val paramGlb = paramType1 /\ paramType2 // Covariant parameter
      val return1 = env.withNewUnique(paramType1) {
        implicit (env, ident, ty) => closure1(Value.variable(ident, ty))
      }
      val return2 = env.withNewUnique(paramType2) {
        implicit (env, ident, ty) => closure2(Value.variable(ident, ty))
      }
      Value.Sigma(paramGlb, _ => return1 /\ return2)
    }

    // GLB for Records: only fields present in both are retained
    case (Value.Record(fields1), Value.Record(fields2)) => {
      val commonFields = fields1.keySet.intersect(fields2.keySet)
      val glbFields = commonFields.map { key =>
        key -> (fields1(key) /\ fields2(key))
      }.toMap
      Value.Record(glbFields)
    }

    // GLB for Record Types: similar to records, fields must be present in both and have a GLB
    case (Value.RecordType(fields1), Value.RecordType(fields2)) => {
      val commonFields = fields1.keySet.intersect(fields2.keySet)
      val glbFields = commonFields.map { key =>
        key -> (fields1(key) /\ fields2(key))
      }.toMap
      Value.RecordType(glbFields)
    }

    // GLB for Inductive Types: if they match, take GLB of arguments
    case (InductiveType(ind1, args1), InductiveType(ind2, args2)) if ind1.name == ind2.name => {
      val glbArgs = args1.zip(args2).map { case (arg1, arg2) => arg1 /\ arg2 }
      Value.InductiveType(ind1, glbArgs)
    }

    // GLB for Inductive Variants: constructors and types must match, arguments need GLB
    case (
      Value.InductiveVariant(ind1, cons1, args1),
      Value.InductiveVariant(ind2, cons2, args2),
    ) if (ind1 unify ind2) && cons1.ident == cons2.ident => {
      val glbArgs = args1.zip(args2).map { case (arg1, arg2) => arg1 /\ arg2 }
      Value.InductiveVariant(ind1, cons1, glbArgs)
    }

    // Default case: if types are incompatible, return Nothing
    case _ => PrimitiveType(LiteralType.NothingType)
  }

  @deprecatedOverriding("For debugging purposes only, don't call it in production code")
  override def toString: String = try this.reflect(Environment.Typed.empty).toString catch {
    case _: Throwable => super.toString
  }
}

given RuntimeEntityFactory[Value] = Value

object Value extends RuntimeEntityFactory[Value] {

  override def unit: Type = Value.Primitive(Literal.UnitValue)

  override def unitType: Type = Value.PrimitiveType(LiteralType.UnitType)

  override def universe: Type = Universe

  override def variable(ident: Var.Local, ty: Type): Value = Neutral(NeutralValue.Variable(ident, ty))

  override def typeBarrier(value: Value, ty: Type): Type = Neutral(NeutralValue.TypeBarrier(value, ty))

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
   * @see [[term.evalParameterized]]
   */
  private[core] def readBackClosure(paramType: Type, closure: CodomainClosure)(
    implicit env: Environment.Typed[Value]
  ): (Param[Term], Term) = {
    val paramIdent = env.uniqueVariable
    val paramVariable = Value.variable(paramIdent, paramType)
    val paramTypeTerm = paramType.reflect
    env.withLocal(paramIdent, paramVariable, paramType) { implicit env =>
      val term = closure(paramVariable).reflect(env)
      (Param(paramIdent, paramTypeTerm), term)
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

  def states: Map[Type, CodomainClosure]

  def readBackStates(implicit env: Environment.Typed[Value]): Map[Param[Term], Term] = {
    states.map { (paramType, closure) =>
      val paramIdent = env.uniqueVariable
      val paramVariable = Value.variable(paramIdent, paramType)
      val paramTypeTerm = paramType.reflect
      env.withLocal(paramIdent, paramVariable, paramType) { implicit env =>
        val body = closure(paramVariable).reflect(env)
        (Param(paramIdent, paramTypeTerm), body)
      }
    }
  }

  def isStatesFinal(variables: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean = {
    states.forall { (paramType, closure) =>
      env.withNewUnique(paramType) {
        implicit (env, ident, ty) => {
          given Environment.Typed[Value] = env
          closure(Value.variable(ident, ty)).isFinal(variables + ident)
        }
      }
    }
  }

  def applyArgument(
    arg: Value, argType: Type,
    constructor: Map[Type, CodomainClosure] => S,
    unwrapStates: Value => Map[Type, CodomainClosure],
  )(implicit env: Environment.Typed[Value]): Value = {

    val candidateStates = states.filter {
      case (paramType, _) => paramType <:< argType
    }

    if candidateStates.isEmpty then NoSuchOverloading.raise {
      s"No overloading found for argument ${arg.reflect} with type: ${argType.reflect}"
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
