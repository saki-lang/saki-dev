package saki.core.term

import saki.core.{RuntimeEntity, RuntimeEntityFactory}
import saki.core.context.{Environment, Typed}
import saki.core.domain.{NeutralValue, Type, Value}
import saki.core.syntax.{buildTypeMapping, getOverload, tryMatch, Argument, Clause, CodomainClosure, Constructor, Declaration, DefinedFunction, Function, Inductive, Literal, LiteralType, NativeFunction, Overloaded, Param, ParameterizedClosure, ParamList, Var}
import saki.error.CoreErrorKind.*
import saki.error.PanicError
import saki.util.unreachable

import scala.collection.Seq

trait Term extends RuntimeEntity[Type] {
  def eval(evalMode: EvalMode = EvalMode.Normal)(implicit env: Environment.Typed[Value]): Value
  def eval(implicit env: Environment.Typed[Value]): Value = this.eval(evalMode = EvalMode.Normal)
  def forceEval(implicit env: Environment.Typed[Value]): Value = this.eval(evalMode = EvalMode.Force)
  final def substitute(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term = this match {
    case `from` if this unify from => to
    case _ => this.substituteTerm(from, to)
  }
  def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term
  def normalize(implicit env: Environment.Typed[Value]): Term = this.eval.readBack
  infix def unify(that: Term)(implicit env: Environment.Typed[Value]): Boolean = this.eval unify that.eval
  override def infer(implicit env: Environment.Typed[Value]): Value = this.eval.infer
  def contains(ident: Var.Local, shadowed: Set[Var.Local] = Set.empty)(
    implicit env: Environment.Typed[Value]
  ): Boolean
  def evalString(implicit env: Environment.Typed[Value]): String = ""
}

enum EvalMode {
  case Partial, Normal, Force
}

case object Universe extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value = Value.Universe
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term = this
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean = false
}

case class Primitive(value: Literal) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value = Value.Primitive(value)
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term = this
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean = false
}

case class PrimitiveType(`type`: LiteralType) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value = Value.PrimitiveType(`type`)
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term = this
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean = false
}

case class Variable(variable: Var.Local) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value = env.getValue(variable) match {
    case Some(value) => value
    case None => throw new IllegalArgumentException(s"Variable not found: ${variable.name}")
  }
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term = env.getTyped(variable) match {
    case Some(typed) if typed.value.readBack unify from => to
    case _ => this
  }
  // Just a simple trick, avoid unnecessary evaluation, and should be removed if causing any problem
  override def infer(implicit env: Environment.Typed[Value]): Value = env.locals.get(variable) match {
    case Some(value) => value.`type`
    case None => throw new IllegalArgumentException(s"Variable not found: ${variable.name}")
  }
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    variable == ident && !shadowed.contains(variable)
}

case class TypeBarrier(term: Term, `type`: Term) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value = {
    val value: Value = term.eval(evalMode)
    val termType: Type = value.infer
    val expectedType: Type = `type`.eval(evalMode)
    if expectedType <:< termType then term.eval(evalMode)
    else Value.typeBarrier(term.eval(evalMode), expectedType)
  }
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term =
    TypeBarrier(term.substitute(from, to), `type`.substitute(from, to))
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    term.contains(ident, shadowed)
}

case class Union(types: Set[Term]) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value = {
    val typeValues = types.map(_.eval(evalMode))
    typeValues.reduce((a, b) => a \/ b)
  }
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term =
    Union(types.map(_.substitute(from, to)))
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    types.exists(_.contains(ident, shadowed))
}

case class Intersection(types: Set[Term]) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value = {
    val typeValues = types.map(_.eval(evalMode))
    typeValues.reduce((a, b) => a /\ b)
  }
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term =
    Intersection(types.map(_.substitute(from, to)))
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    types.exists(_.contains(ident, shadowed))
}

case class FunctionInvoke(fn: Var.Defined[Term, Function], args: Seq[Term]) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value = {
    lazy val argsValue: Seq[Value] = args.map(_.eval(evalMode))
    if evalMode == EvalMode.Partial then return Value.functionInvoke(fn, argsValue)
    val function: Function[Term] = env.getSymbol(fn).get match {
      case function: Function[Term] => function
      case overloaded: Overloaded[Term] => fn.definition.toOption match {
        case Some(function) => function
        case None => overloaded.getOverload(args).asInstanceOf[Function[Term]]
      }
      // If the function is a pre-declared function, keep it as a neutral value
      case _: Declaration[Term, Function] @unchecked => return Value.functionInvoke(fn, argsValue)
      case _ => DefinitionNotMatch.raise(s"Expected function, but got: ${fn.name}")
    }
    lazy val allArgumentsFinal = argsValue.forall(_.isFinal(Set.empty))
    lazy val (bodyEnv, argVarList) = function.mapArgs(argsValue).foldLeft((env, Seq.empty[(Var.Local, Typed[Value])])) {
      case ((env, argList), (param, arg)) => {
        val paramType = param.`type`.eval(evalMode)(env)
        val updatedEnv = env.add(param.ident, arg, paramType)
        val argPair: (Var.Local, Typed[Value]) = (param.ident, Typed[Value](arg, paramType))
        (updatedEnv, argList :+ argPair)
      }
    }
    lazy val evaluatedFunctionBody: Value = function match {
      case fn: DefinedFunction[Term] => bodyEnv.invokeFunction(fn.ident) { implicit env => fn.body.get.eval(evalMode)(env) }
      case fn: NativeFunction[Term] => if allArgumentsFinal then {
        try fn.invoke(argVarList.map { (_, typed) => Argument(typed.value) }) catch {
          case e: PanicError => if evalMode == EvalMode.Force then throw e else Value.functionInvoke(fn.ident, argsValue)
        }
      } else Value.functionInvoke(fn.ident, argsValue)
    }
    env.currentDefinition match {
      case Some(current: Var.Defined[Term, Function] @unchecked) => {
        if !function.isRecursive || !function.dependencies.contains(current) || allArgumentsFinal then {
          if evaluatedFunctionBody.containsMatching then Value.functionInvoke(fn, argsValue) else evaluatedFunctionBody
        } else Value.functionInvoke(function.ident, argsValue)
      }
      case None | Some(_) => if evaluatedFunctionBody.containsMatching then Value.functionInvoke(fn, argsValue) else evaluatedFunctionBody
    }
  }
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term =
    FunctionInvoke(fn, args.map(_.substitute(from, to)))
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    args.exists(_.contains(ident, shadowed))
}

case class OverloadInvoke(fn: Var.Defined[Term, Overloaded], args: Seq[Term]) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value =
    Term.functionInvoke(fn.definition.get.getOverload(args).asInstanceOf, args).eval(evalMode)
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term =
    OverloadInvoke(fn, args.map(_.substitute(from, to)))
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    args.exists(_.contains(ident, shadowed))
}

case class InductiveType(inductive: Var.Defined[Term, Inductive], args: Seq[Term]) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value = {
    val argsValue: Seq[Value] = args.map(_.eval(evalMode))
    Value.inductiveType(inductive, argsValue)
  }
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term =
    InductiveType(inductive, args.map(_.substitute(from, to)))
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    args.exists(_.contains(ident, shadowed))
}

case class InductiveVariant(inductive: Term, constructor: Constructor[Term], args: Seq[Term]) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value = inductive.eval(evalMode) match {
    case inductiveType: Value.InductiveType => {
      val argValues = env.withLocals(inductiveType.argsMap) { implicit env => args.map(_.eval(evalMode)(env)) }
      Value.inductiveVariant(inductiveType, constructor, argValues)
    }
    case ty => TypeNotMatch.raise(s"Expected inductive type, but got: ${ty.readBack}")
  }
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term =
    InductiveVariant(inductive.substitute(from, to), constructor, args.map(_.substitute(from, to)))
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    inductive.contains(ident, shadowed) || args.exists(_.contains(ident, shadowed))
}

case class Match(scrutinees: Seq[Term], clauses: Seq[Clause[Term]]) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value = {
    val scrutineesValue = scrutinees.map(_.eval(evalMode))
    // Try to match the scrutinees with the clauses
    clauses.tryMatch(scrutineesValue, evalMode).getOrElse {
      // If all scrutinees are final and no match is found, raise an error
      MatchNotExhaustive.raise(s"Match is not exhaustive for scrutinee: ${scrutineesValue.map(_.readBack).mkString(", ")}")
    }
  }
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term = {
    val newScrutinees = scrutinees.map(_.substitute(from, to))
    val newClauses = clauses.map { clause =>
      val bindings: Seq[(Var.Local, Typed[Value])] = scrutinees.zip(clause.patterns).flatMap {
        (scrutinee, pattern) => pattern.buildTypeMapping(scrutinee.infer)
      }.map { case (param, ty) => (param, Typed[Value](Value.variable(param, ty), ty)) }
      val body = env.withLocals(bindings.toMap) { implicit env => clause.body.substitute(from, to) }
      Clause(clause.patterns.map(_.map(_.substitute(from, to))), body)
    }
    Match(newScrutinees, newClauses)
  }
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean = {
    scrutinees.exists(_.contains(ident, shadowed)) || clauses.exists { clause =>
      val bindings = clause.patterns.zip(scrutinees).flatMap {
        (pattern, scrutinee) => pattern.buildTypeMapping(scrutinee.infer)
      }.map { (param, _) => param }
      clause.body.contains(ident, shadowed ++ bindings)
    }
  }
}

case class Pi(param: Param[Term], codomain: Term) extends Term with ApplicableTypeTerm {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value =
    this.eval(Value.Pi.apply, evalMode)
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term = {
    val newParam = param.map(_.substitute(from, to))
    val newCodomain = codomain.substitute(from, to)
    if (newParam.`type` unify from) Pi(newParam, codomain) else Pi(newParam, newCodomain)
  }
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    codomain.contains(ident, shadowed + param.ident)
}

case class OverloadedPi(states: Map[Param[Term], Term]) extends Term with OverloadedTerm[OverloadedPi] {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value =
    this.eval(Value.OverloadedPi.apply, evalMode)
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term =
    OverloadedPi(states.view.mapValues(_.substitute(from, to)).toMap)
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    states.exists((param, codomain) => codomain.contains(ident, shadowed + param.ident))
}

case class Sigma(param: Param[Term], codomain: Term) extends Term with ApplicableTypeTerm {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value =
    this.eval(Value.Sigma.apply, evalMode)
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term = {
    val newParam = param.map(_.substitute(from, to))
    val newCodomain = codomain.substitute(from, to)
    if newParam.`type` unify from then Sigma(newParam, codomain) else Sigma(newParam, newCodomain)
  }
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    codomain.contains(ident, shadowed + param.ident)
}

case class Record(fields: Map[String, Term]) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value =
    Value.Record(fields.map((name, term) => (name, term.eval(evalMode))))
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term =
    Record(fields.view.mapValues(_.substitute(from, to)).toMap)
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    fields.values.exists(_.contains(ident, shadowed))
}

case class RecordType(fields: Map[String, Term]) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value =
    Value.RecordType(fields.map((name, ty) => (name, ty.eval(evalMode))))
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term =
    RecordType(fields.view.mapValues(_.substitute(from, to)).toMap)
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    fields.values.exists(_.contains(ident, shadowed))
}

case class Apply(fn: Term, arg: Term) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value = fn.eval(evalMode) match {
    case Value.Lambda(paramType, closure) => {
      if !(paramType <:< arg.infer) then TypeNotMatch.raise(s"Expected argument type: ${paramType.readBack}, but got: ${arg.infer.readBack}")
      closure(arg.eval(evalMode))
    }
    case overloaded: Value.OverloadedLambda => {
      val argValue: Value = arg.eval(evalMode)
      overloaded.applyArgument(
        argValue, arg.infer, Value.OverloadedLambda.apply,
        unwrapStates = {
          case Value.OverloadedLambda(states) => states
          case value => TypeNotMatch.raise(s"Expected an overloaded lambda, but got: ${value.readBack}")
        }
      )
    }
    // If the evaluation of the function stuck, the whole application is stuck
    // Thus, no need for considering the situation that function is a global call
    // because it is tried to be evaluated before but failed
    case Value.Neutral(neutral) => Value.Neutral(NeutralValue.Apply(neutral, arg.eval(evalMode)))
    case _ => TypeNotMatch.raise(s"Cannot apply an argument to a non-function value: $fn")
  }
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term =
    Apply(fn.substitute(from, to), arg.substitute(from, to))
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    fn.contains(ident, shadowed) || arg.contains(ident, shadowed)
}

case class Lambda(param: Param[Term], body: Term) extends Term with ApplicableTerm {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value =
    this.eval(Value.Lambda.apply, evalMode)
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term = {
    val newParam = param.map(_.substitute(from, to))
    if (newParam.`type` unify from) Lambda(newParam, body) else Lambda(newParam, body.substitute(from, to))
  }
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    body.contains(ident, shadowed + param.ident)
}

case class OverloadedLambda(states: Map[Param[Term], Term]) extends Term with OverloadedTerm[OverloadedLambda] {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value =
    this.eval(Value.OverloadedLambda.apply, evalMode)
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term =
    OverloadedLambda(states.view.mapValues(_.substitute(from, to)).toMap)
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    states.exists((param, body) => body.contains(ident, shadowed + param.ident))
}

case class Pair(first: Term, second: Term) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value =
    Value.Pair(first.eval(evalMode), second.eval(evalMode))
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term =
    Pair(first.substitute(from, to), second.substitute(from, to))
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    first.contains(ident, shadowed) || second.contains(ident, shadowed)
}

case class Projection(record: Term, field: String) extends Term {
  override def eval(evalMode: EvalMode)(implicit env: Environment.Typed[Value]): Value = record.eval match {
    case Value.Record(fields) => fields.getOrElse(field, RecordMissingField.raise(s"Field $field not found in record"))
    case neutral: Value.Neutral => Value.Neutral(NeutralValue.Projection(neutral, field))
    case _ => TypeNotMatch.raise(s"Cannot project from non-record value: $record")
  }
  override def substituteTerm(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term =
    Projection(record.substitute(from, to), field)
  override def contains(ident: Var.Local, shadowed: Set[Var.Local])(implicit env: Environment.Typed[Value]): Boolean =
    record.contains(ident, shadowed)
}

given termFactory: RuntimeEntityFactory[Term] = Term

object Term extends RuntimeEntityFactory[Term] {

  override def unit: Term = Primitive(Literal.UnitValue)

  override def unitType: Term = PrimitiveType(LiteralType.UnitType)

  override def universe: Term = Universe

  override def variable(ident: Var.Local, ty: Term): Term = Variable(ident)

  override def typeBarrier(value: Term, ty: Term): Term = TypeBarrier(value, ty)

  override def inductiveType(
    inductive: Var.Defined[Term, Inductive], args: Seq[Term]
  ): Term = InductiveType(inductive, args)

  override def functionInvoke(
    function: Var.Defined[Term, Function], args: Seq[Term]
  ): Term = FunctionInvoke(function, args)

  override def inductiveVariant(
    inductive: Term, constructor: Constructor[Term], args: Seq[Term]
  ): Term = InductiveVariant(inductive, constructor, args)

  def overloaded[T <: Term & OverloadedTerm[T]](
    constructor: Map[Param[Term], Term] => T,
    paths: Seq[(Seq[Param[Term]], Term)],
  ): T = paths.foldLeft(constructor(Map.empty)) {
    case (overloaded, (path, body)) => addOverloadedPath(overloaded, path, body)
  }

  private def addOverloadedPath[T <: Term & OverloadedTerm[T]](
    overloaded: T, path: Seq[Param[Term]], body: Term,
  ): T = path match {
    case Nil => overloaded
    case head +: tail => {
      val updatedState: Term = overloaded.states.get(head) match {
        case Some(term: OverloadedLambda) => addOverloadedPath(term, tail, body)
        case Some(term: OverloadedPi) => addOverloadedPath(term, tail, body)
        case Some(_) => OverloadingAmbiguous.raise {
          s"Ambiguous overloading for function: ${head.`type`}"
        }
        case None => if tail.isEmpty then body else addOverloadedPath(overloaded.copy(Map.empty), tail, body)
      }
      overloaded.copy(overloaded.states + (head -> updatedState))
    }
  }

  /**
   * Evaluates a parameterized term and returns a closure.
   *
   * @param param The parameter of the term.
   * @param term  The term to be evaluated.
   * @param env   The environment in which the term is evaluated.
   * @return 1. Evaluated type of the parameter
   *         2. A closure that takes a value and returns a value.
   * @see [[Value.readBackClosure]]
   */
  private[core] def evalParameterized(param: Param[Term], term: Term, evalMode: EvalMode)(
    implicit env: Environment.Typed[Value]
  ): (Type, CodomainClosure) = {
    val typedParam = param.map(_.eval(evalMode))
    val closure = ParameterizedClosure(typedParam, env) { implicit env => term.eval(evalMode) }
    (typedParam.`type`, closure)
  }

  private[core] def evalParameterized(paramType: Type, term: Term, evalMode: EvalMode)(
    implicit env: Environment.Typed[Value]
  ): (Type, CodomainClosure) = {
    val paramIdent = env.uniqueVariable
    val typedParam = Param[Type](paramIdent, paramType)
    env.withLocal(paramIdent, Value.variable(paramIdent, paramType), paramType) { implicit env =>
      val closure = ParameterizedClosure(typedParam, env) { implicit env => term.eval(evalMode) }
      (typedParam.`type`, closure)
    }
  }

}

