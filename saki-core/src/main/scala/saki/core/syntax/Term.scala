package saki.core.syntax

import saki.core.context.{Environment, Typed}
import saki.core.domain.{NeutralValue, Type, Value}
import saki.core.{RuntimeEntity, RuntimeEntityFactory, TypeError}

import scala.collection.Seq

enum Term extends RuntimeEntity[Type] {

  case Universe
  case Primitive(value: Literal)
  case PrimitiveType(`type`: LiteralType)
  case Variable(variable: Var.Local)
  case FunctionInvoke(fn: Var.Defined[Term, Function], args: Seq[Term])
  case OverloadInvoke(
    fn: Var.Defined[Term, Overloaded], args: Seq[Term]
  ) extends Term with OverloadInvokeExt
  case InductiveType(inductive: Var.Defined[Term, Inductive], args: Seq[Term])
  case InductiveVariant(cons: Var.Defined[Term, Constructor], consArgs: Seq[Term], inductiveArgs: Seq[Term])
  case Match(scrutinees: Seq[Term], clauses: Seq[Clause[Term]])
  case Pi(param: Param[Term], codomain: Term) extends Term with PiLikeTerm
  case OverloadedPi(states: Set[(Param[Term], Term)]) extends Term with OverloadedTerm
  case Sigma(param: Param[Term], codomain: Term) extends Term with PiLikeTerm
  case Record(fields: Map[String, Term])
  case RecordType(fields: Map[String, Term])
  case Apply(fn: Term, arg: Term)
  case Lambda(param: Param[Term], body: Term) extends Term with LambdaLikeTerm
  case OverloadedLambda(states: Set[(Param[Term], Term)]) extends Term with OverloadedTerm

  case Projection(record: Term, field: String)

  def apply(args: Term*)(implicit env: Environment.Typed[Value]): Term = {
    args.foldLeft(this) {
      case (Lambda(param, body), arg) => {
        env.withLocal(param.ident, Typed[Value](arg.eval, arg.infer)) { env =>
          body.eval(env).readBack(env)
        }
      }
      case (fn, arg) => Apply(fn, arg)
    }
  }

  override def toString: String = this match {
    case Universe => s"#Universe"
    case Primitive(value) => value.toString
    case PrimitiveType(ty) => ty.toString
    case Variable(variable) => variable.name
    case FunctionInvoke(fn, args) => s"${fn.name}(${args.mkString(", ")})"
    case OverloadInvoke(fn, args) => s"${fn.name}(${args.mkString(", ")})"
    case InductiveType(inductive, args) => {
      val argsStr = if args.nonEmpty then s"(${args.mkString(", ")})" else ""
      s"${inductive.name}$argsStr"
    }
    case InductiveVariant(cons, args, inductiveArgs) => {
      val inductiveArgsStr = if inductiveArgs.nonEmpty then s"(${inductiveArgs.mkString(", ")})" else ""
      val argsStr = if args.nonEmpty then s"(${args.mkString(", ")})" else ""
      s"${cons.name}$inductiveArgsStr$argsStr"
    }
    case Match(scrutinees, clauses) => {
      val scrutineesStr = if scrutinees.size > 1 then {
        s"(${scrutinees.mkString(", ")})"
      } else scrutinees.head.toString
      s"match $scrutineesStr { ${clauses.mkString(" | ")} }"
    }
    case Pi(param, codomain) => s"Π(${param.name} : ${param.`type`}) -> $codomain"
    case OverloadedPi(_) => s"#SuperPositionPi"
    case Sigma(param, codomain) => s"Σ(${param.name} : ${param.`type`}) -> $codomain"
    case Record(fields) => s"{ ${fields.map { case (k, v) => s"$k = $v" }.mkString(", ")} }"
    case RecordType(fields) => s"record { ${fields.map { case (k, v) => s"$k: $v" }.mkString(", ")} }"
    case Apply(fn, arg) => s"$fn($arg)"
    case Lambda(param, body) => s"λ(${param.name} : ${param.`type`}) => $body"
    case OverloadedLambda(_) => s"#SuperPositionLambda"
    case Projection(record, field) => s"$record.$field"
  }

  def normalize(
    implicit env: Environment.Typed[Value]
  ): Term = this.eval.readBack

  /**
   * Infer the type of the term
   * @param env The environment
   * @return The inferred type
   */
  override def infer(implicit env: Environment.Typed[Value]): Value = this match {

    case Universe => Value.Universe

    case Primitive(value) => Value.PrimitiveType(value.ty)

    case PrimitiveType(_) => Value.Universe

    case Variable(variable) => env.getTyped(variable).get.`type`

    case FunctionInvoke(fnRef, _) => {
      env.definitions(fnRef).asInstanceOf[Function[Term]].resultType.eval
    }

    case invoke: OverloadInvoke => {
      val (func, _) = invoke.getOverload
      val paramMap = func.params.map { param =>
        (param.ident, Typed[Value](Value.variable(param.ident), param.`type`.eval))
      }.toMap
      env.withLocals(paramMap) { implicit env => func.resultType.eval(env) }
    }

    case InductiveType(indRef, _) => {
      env.definitions(indRef).asInstanceOf[Inductive[Term]].resultType.eval
    }

    case InductiveVariant(consRef, _, _) => {
      env.definitions(consRef).asInstanceOf[Constructor[Term]].resultType.eval
    }

    case Match(_, clauses) => {
      val clausesType: Seq[Value] = clauses.map(_.body.infer)
      if clausesType.tail.forall(_ unify clausesType.head) then clausesType.head
      else throw TypeError("Clauses have different types", None)
      clausesType.head
    }

    case Pi(_, _) => Value.Universe

    case OverloadedPi(_) => Value.Universe

    case Sigma(_, _) => Value.Universe

    case Record(fields) => Value.RecordType(fields.map {
      (name, term) => (name, term.infer)
    })

    case RecordType(_) => Value.Universe

    case Apply(fn, arg) => fn.infer match {

      case Value.Pi(paramType, codomain) => {
        val argType = arg.infer
        if !(argType unify paramType) then {
          throw TypeError(s"Type mismatch: $paramType != $argType", None)
        }
        // To obtain the concrete return type, feed the concrete argument to the codomain closure
        codomain(arg.eval)
      }

      case Value.OverloadedPi(states) => {
        val argType = arg.infer
        // Find the states that the argument type is a subtype of the parameter type
        val candidateStates = states.filter {
          (paramType, _) => paramType <:< argType
        }
        if candidateStates.isEmpty then {
          throw TypeError("No matching state in superposition", None)
        }
        // Find the states that there is no other state that is closer to the argument type
        val validStates = candidateStates.filter {
          (paramType, _) => !candidateStates.exists(_._1 <:< paramType)
        }
        if validStates.size > 1 then {
          throw TypeError("Multiple valid states in superposition", None)
        }
        val (_, codomain) = validStates.head
        codomain(arg.eval)
      }
      case _ => throw TypeError("Cannot apply non-function", None)
    }

    // Lambda returns a dependent function type
    case Lambda(param, body) => {
      val paramType = param.`type`.infer
      def closure(arg: Value): Value = {
        val argVar = Typed[Value](arg, paramType)
        env.withLocal(param.ident, argVar) { implicit env => body.infer(env) }
      }
      Value.Pi(paramType, closure)
    }

    case OverloadedLambda(states) => Value.OverloadedPi(states.map { (param, body) =>
      val paramType = param.`type`.infer
      def closure(arg: Value): Value = {
        val argVar = Typed[Value](arg, paramType)
        env.withLocal(param.ident, argVar) { implicit env => body.infer(env) }
      }
      (paramType, closure)
    })

    case Projection(record, field) => record.infer match {
      case Value.RecordType(fields) => fields.getOrElse(field, {
        throw TypeError(s"Missing field: $field", None)
      })
      case _ => throw TypeError("Cannot project from non-record", None)
    }

  }

  def eval(implicit env: Environment.Typed[Value]): Value = this match {

    case Universe => Value.Universe

    case Primitive(value) => Value.Primitive(value)

    case PrimitiveType(ty) => Value.PrimitiveType(ty)

    case Variable(variable) => env.getValue(variable).get

    case FunctionInvoke(fnRef, argTerms) => {
      val fn = env.definitions(fnRef).asInstanceOf[Function[Term]]
      env.currentDefinition match {
        case Some(current) if current.name == fnRef.name => {
          // Recursive call, keep it a neutral value
          Value.functionInvoke(fn.ident, argTerms.map(_.eval))
        }
        case None | Some(_) => {
          val argsValue: Seq[Value] = argTerms.map(_.eval)
          // TODO: this need to be optimized
          if !fn.isRecursive || argsValue.forall(_.readBack.isFinal(Set.empty)) then {
            val argVarList: Seq[(Var.Local, Typed[Value])] = fn.arguments(argsValue).map {
              (param, arg) => (param, Typed[Value](arg, arg.infer))
            }
            val body: Term = fn.body.get
            env.withLocals(argVarList.toMap) { implicit env => body.eval(env) }
          } else {
            Value.functionInvoke(fn.ident, argsValue)
          }
        }
      }
    }

    case invoke: OverloadInvoke => {
      val (function, argValues) = invoke.getOverload
      if !function.isRecursive || argValues.forall(_.readBack.isFinal(Set.empty)) then {
        val argVarList: Seq[(Var.Local, Typed[Value])] = function.params.zip(argValues).map {
          (param, arg) => (param.ident, Typed[Value](arg, arg.infer))
        }
        env.withLocals(argVarList.toMap) {
          implicit env => function.body.get.eval(env)
        }
      } else {
        Value.functionInvoke(function.ident, argValues)
      }
    }

    case InductiveType(indRef, argTerms) => {
      val argsValue: Seq[Value] = argTerms.map(_.eval)
      Value.inductiveType(indRef, argsValue)
    }

    case InductiveVariant(consRef, consArgs, inductiveArgs) => {
      val consArgsValue: Seq[Value] = consArgs.map(_.eval)
      val inductiveArgsValue: Seq[Value] = inductiveArgs.map(_.eval)
      Value.inductiveVariant(consRef, consArgsValue, inductiveArgsValue)
    }

    case Match(scrutinees, clauses) => {
      val scrutineesNorm = scrutinees.map(_.eval)
      clauses.tryMatch(scrutineesNorm).getOrElse {
        Value.Neutral(NeutralValue.Match(scrutineesNorm, clauses.map(_.map(_.eval))))
      }
    }

    case piType: Pi => piType.eval(Value.Pi.apply)

    case piType: OverloadedPi => piType.eval(Value.OverloadedPi.apply)

    case sigmaType: Sigma => sigmaType.eval(Value.Sigma.apply)

    case Record(fields) => {
      val fieldsValue: Map[String, Value] = fields.map {
        (name, term) => (name, term.eval)
      }
      Value.Record(fieldsValue)
    }

    case RecordType(fields) => {
      val fieldsType: Map[String, Type] = fields.map {
        (name, term) => (name, term.infer)
      }
      Value.RecordType(fieldsType)
    }

    case Apply(fn, arg) => fn.eval match {
      
      case Value.Lambda(paramType, bodyClosure) => {
        val argType = arg.infer
        if !(paramType <:< argType) then {
          throw TypeError(s"Type mismatch: $paramType != $argType", None)
        }
        bodyClosure(arg.eval)
      }
      
      case Value.OverloadedLambda(states) => {

        val argType = arg.infer
        val candidateStates = states.filter {
          case (paramType, _) => paramType <:< argType
        }
        
        if candidateStates.isEmpty then {
          throw TypeError("No matching state in superposition", None)
        }
        
        if candidateStates.size == 1 then {
          // If there is only one state that matches the argument type, evaluate it
          val (_, closure) = candidateStates.head
          closure(arg.eval)
        } else {
          // If there are multiple states that match the argument type, evaluate all of them
          val argValue = arg.eval
          // Evaluate each state and merge the results
          val newStates = candidateStates.flatMap { (_, closure) =>
            // Since the parameter type is checked to be a subtype of the argument type,
            // we don't need to check the type of the argument value again
            closure(argValue) match {
              case Value.OverloadedLambda(states) => states
              case _ => throw TypeError("Invalid state in superposition", None)
            }
          }
          // Merge the new states
          Value.OverloadedLambda(newStates)
        }
      }

      // If the evaluation of the function stuck, the whole application is stuck
      // Thus, no need for considering the situation that function is a global call
      // because it is tried to be evaluated before but failed
      case Value.Neutral(neutral) => Value.Neutral(NeutralValue.Apply(neutral, arg.eval))

      case _ => throw TypeError(s"Cannot apply non-function: $fn", None)
    }

    case lambda: Lambda => lambda.eval(Value.Lambda.apply)

    case lambda: OverloadedLambda => lambda.eval(Value.OverloadedLambda.apply)

    case Projection(record, field) => record.eval match {
      case Value.Record(fields) => fields.getOrElse(field, {
        throw TypeError(s"Missing field: $field", None)
      })
      case neutral: Value.Neutral => Value.Neutral(NeutralValue.Projection(neutral, field))
      case _ => throw TypeError(s"Cannot project from non-record: $record", None)
    }
  }

  def isFinal(implicit localVars: Set[Var.Local]): Boolean = this match {
    case Variable(variable) => localVars.contains(variable)
    case Universe | Primitive(_) | PrimitiveType(_) => true
    case FunctionInvoke(_, args) => args.forall(_.isFinal)
    case OverloadInvoke(_, args) => args.forall(_.isFinal)
    case InductiveType(_, args) => args.forall(_.isFinal)
    case InductiveVariant(_, consArgs, inductiveArgs) => consArgs.forall(_.isFinal) && inductiveArgs.forall(_.isFinal)
    case Match(scrutinees, clauses) => scrutinees.forall(_.isFinal) && clauses.forall(_.forall(_.isFinal))
    case Pi(param, codomain) => param.`type`.isFinal && codomain.isFinal
    case OverloadedPi(states) => states.forall {
      (param, body) => param.`type`.isFinal && body.isFinal(localVars + param.ident)
    }
    case Sigma(param, codomain) => param.`type`.isFinal && codomain.isFinal
    case Record(fields) => fields.values.forall(_.isFinal)
    case RecordType(fields) => fields.values.forall(_.isFinal)
    case Apply(fn, arg) => fn.isFinal && arg.isFinal
    case Lambda(param, body) => body.isFinal(localVars + param.ident)
    case OverloadedLambda(states) => states.forall {
      (param, body) => param.`type`.isFinal && body.isFinal(localVars + param.ident)
    }
    case Projection(record, _) => record.isFinal
  }
}

extension (params: ParamList[Term]) {
  def buildPiType(body: Term): Term = params.foldRight(body) {
    case (param, body) => Term.Pi(param, body)
  }

  def buildLambda(body: Term): Term = params.foldRight(body) {
    case (param, body) => Term.Lambda(param, body)
  }
}

given RuntimeEntityFactory[Term] = Term

object Term extends RuntimeEntityFactory[Term] {

  override def unit: Term = Primitive(Literal.UnitValue)

  override def unitType: Term = PrimitiveType(LiteralType.UnitType)

  override def universe: Term = Universe

  override def variable(ident: Var.Local): Term = Variable(ident)

  override def inductiveType(
    inductive: Var.Defined[Term, Inductive], args: Seq[Term]
  ): Term = InductiveType(inductive, args)

  override def functionInvoke(
    function: Var.Defined[Term, Function], args: Seq[Term]
  ): Term = FunctionInvoke(function, args)

  override def inductiveVariant(
    cons: Var.Defined[Term, Constructor], consArgs: Seq[Term], inductiveArgs: Seq[Term]
  ): Term = InductiveVariant(cons, consArgs, inductiveArgs)
}

/**
 * Evaluates a parameterized term and returns a closure.
 *
 * @param param The parameter of the term.
 * @param term The term to be evaluated.
 * @param env The environment in which the term is evaluated.
 * @return A tuple containing the evaluated type of the parameter
 *         and a closure that takes a value and returns a value.
 */
private def evalParameterizedTermToClosure(param: Param[Term], term: Term)(
  implicit env: Environment.Typed[Value]
): (Type, Value => Value) = {
  val paramType = param.`type`.eval
  def closure(arg: Value): Value = {
    val argVar = Typed[Value](arg, paramType)
    env.withLocal(param.ident, argVar) { implicit env => term.eval(env) }
  }
  (paramType, closure)
}

private sealed trait LambdaLikeTerm {
  def param: Param[Term]
  def body: Term

  def eval(constructor: (Type, Value => Value) => Value)(
    implicit env: Environment.Typed[Value]
  ): Value = {
    val (paramType, closure) = evalParameterizedTermToClosure(param, body)
    constructor(paramType, closure)
  }
}

private sealed trait PiLikeTerm extends LambdaLikeTerm {
  def codomain: Term
  override def body: Term = codomain
}

private sealed trait OverloadedTerm {
  def states: Set[(Param[Term], Term)]

  def eval(constructor: Set[(Type, Value => Value)] => Value)(
    implicit env: Environment.Typed[Value]
  ): Value = {
    val statesValue = states.map(evalParameterizedTermToClosure)
    constructor(statesValue)
  }
}

trait OverloadInvokeExt {
  def fn: Var.Defined[Term, Overloaded]
  def args: Seq[Term]

  /**
   * Get the most suitable state of the overloaded function that matches the argument types.
   * @param env The environment
   * @return 1. The most suitable eigenstate of the overloaded function body
   *         2. The parameters of the state
   */
  def getOverload(
    implicit env: Environment.Typed[Value]
  ): (Function[Term], Seq[Value]) = {
    val fn = env.definitions(this.fn).asInstanceOf[Overloaded[Term]]
    val argsValue: Seq[Value] = this.args.map(_.eval)

    // Filter out the candidate overloads that fit the argument types
    val candidateOverloads = fn.overloads.filter { overload =>
      val params = overload.params
      if params.size != argsValue.size then false
      else params.zip(argsValue).forall {
        (param, arg) => param.`type`.eval <:< arg.infer
      }
    }

    // If no candidate overload fits the argument types, throw an error
    if candidateOverloads.isEmpty then {
      throw TypeError.overloadingNoMatch(this.fn.name, None)
    }

    // Otherwise, find the most suitable one
    // The most suitable one is the one that has the most specific parameter types

    // Iterate through each parameter position, eliminating branches that are not
    // the most specific at that position
    var remainingOverloads: Seq[Function[Term]] = candidateOverloads
    val numParams = remainingOverloads.head.params.length

    for (i <- 0 until numParams if remainingOverloads.size > 1) {
      // Cache the current parameter type at position i for each overload
      val currentOtherParamTypes: Seq[Type] = remainingOverloads.map(_.params(i).`type`.eval)
      // Filter branches to keep those with the most specific parameter type at position i
      remainingOverloads = remainingOverloads.filter { overload =>
        val currentSelfParamType = overload.params(i).`type`.eval
        // Check whether the current parameter type is more specific than or equal to all other parameter types
        // at this position across the remaining branches.
        currentOtherParamTypes.forall { otherParamType =>
          currentSelfParamType <:< otherParamType || !(otherParamType <:< currentSelfParamType)
        }
      }
    }

    if (remainingOverloads.size != 1) {
      throw TypeError(s"Failed to determine the most specific overload for function ${fn.ident.name}", None)
    }

    (remainingOverloads.head, argsValue)
  }
}
