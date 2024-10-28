package saki.core.syntax

import saki.core.context.{Environment, Typed}
import saki.core.domain.{NeutralValue, Type, Value}
import saki.core.domain.neutralClosure
import saki.core.{RuntimeEntity, RuntimeEntityFactory}
import saki.util.{unreachable, OptionCache}
import saki.error.CoreErrorKind.*

import scala.collection.Seq

enum Term extends RuntimeEntity[Type] {

  case Universe
  case Primitive(value: Literal)
  case PrimitiveType(`type`: LiteralType)
  case Variable(variable: Var.Local, `type`: OptionCache[Type] = None)
  case FunctionInvoke(fn: Var.Defined[Term, Function], args: Seq[Term])
  case OverloadInvoke(
    fn: Var.Defined[Term, Overloaded], args: Seq[Term]
  ) extends Term with OverloadInvokeExt
  case InductiveType(inductive: Var.Defined[Term, Inductive], args: Seq[Term])
  case InductiveVariant(inductive: Term, constructor: Constructor[Term], args: Seq[Term])
  case Match(scrutinees: Seq[Term], clauses: Seq[Clause[Term]])
  case Pi(param: Param[Term], codomain: Term) extends Term with PiLikeTerm
  case OverloadedPi(states: Map[Param[Term], Term]) extends Term with OverloadedTermExt[OverloadedPi]
  case Sigma(param: Param[Term], codomain: Term) extends Term with PiLikeTerm
  case Record(fields: Map[String, Term])
  case RecordType(fields: Map[String, Term])
  case Apply(fn: Term, arg: Term)
  case Lambda(param: Param[Term], body: Term) extends Term with LambdaLikeTerm
  case OverloadedLambda(states: Map[Param[Term], Term]) extends Term with OverloadedTermExt[OverloadedLambda]

  case Projection(record: Term, field: String)

  def apply(args: Term*)(implicit env: Environment.Typed[Value]): Term = args.foldLeft(this) {

    case (Lambda(param, body), arg) => {
      env.withLocal(param.ident, Typed[Value](arg.eval, arg.infer)) {
        implicit env => body.normalize
      }
    }

    case (OverloadedLambda(states), arg) => {
      val argType = arg.infer
      val candidateStates = states.filter {
        (param, _) => param.`type`.eval <:< argType
      }
      if candidateStates.isEmpty then OverloadingNotMatch.raise {
        s"No matched overloading in overloaded lambda type of argument with type: ${argType.readBack}"
      }
      val validStates = candidateStates.filter {
        (param, _) => !candidateStates.exists { (param2, _) =>
          param != param2 && param2.`type`.eval <:< param.`type`.eval
        }
      }
      if validStates.size > 1 then OverloadingAmbiguous.raise {
        s"Ambiguous overloading in overloaded lambda type of argument with type: ${argType.readBack}"
      }
      val (param, body) = validStates.head
      env.withLocal(param.ident, Typed[Value](arg.eval, arg.infer)) {
        implicit env => body.normalize
      }
    }

    case (fn, arg) => Apply(fn, arg)
  }

  override def toString: String = this match {
    case Universe => s"'Type"
    case Primitive(value) => value.toString
    case PrimitiveType(ty) => ty.toString
    case Variable(variable, _) => variable.name
    case FunctionInvoke(fn, args) => s"${fn.name}(${args.mkString(", ")})"
    case OverloadInvoke(fn, args) => s"${fn.name}(${args.mkString(", ")})"
    case InductiveType(inductive, args) => {
      val argsStr = if args.nonEmpty then s"(${args.mkString(", ")})" else ""
      s"${inductive.name}$argsStr"
    }
    case InductiveVariant(inductive, constructor, args) => {
      val argsStr = if args.nonEmpty then s"(${args.mkString(", ")})" else ""
      s"$inductive::${constructor.ident}$argsStr"
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
  ): Term = {
    val evalResult = this.eval
    evalResult.readBack
  }

  /**
   * Infer the type of the term
   * @param env The environment
   * @return The inferred type
   */
  override def infer(implicit env: Environment.Typed[Value]): Value = this match {

    case Universe => Value.Universe

    case Primitive(value) => Value.PrimitiveType(value.ty)

    case PrimitiveType(_) => Value.Universe

    case Variable(variable, ty) => ty.toOption match {
      case Some(ty) => ty.readBack.eval
      case None => env.getTyped(variable).get.`type`
    }

    case FunctionInvoke(fn, args) => env.getSymbol(fn).get match {
      case fn: Function[Term] => fn.resultType.eval
      case decl: NaiveDeclaration[Term, ?] => decl.resultType.eval
      case overloaded: Overloaded[Term] => Term.OverloadInvoke(overloaded.ident, args).infer
      case overloaded: OverloadedDeclaration[Term] => Term.OverloadInvoke(overloaded.ident, args).infer 
      case _: Inductive[Term] => Value.Universe
    }

    case invoke: OverloadInvoke => {
      val func = invoke.getOverload
      val paramMap = func.params.map { param =>
        val paramType = param.`type`.eval
        (param.ident, Typed[Value](Value.variable(param.ident, paramType), paramType))
      }.toMap
      env.withLocals(paramMap) { implicit env => func.resultType.eval(env) }
    }

    case InductiveType(indRef, _) => {
      env.definitions(indRef).asInstanceOf[Inductive[Term]].resultType.eval
    }

    case InductiveVariant(inductive, _, _) => inductive.eval

    case Match(scrutinees, clauses) => {
      val scrutineesType = scrutinees.map(_.infer)
      val clausesType: Seq[Value] = clauses.map { clause =>
        val bindings: Seq[(Var.Local, Typed[Value])] = scrutineesType.zip(clause.patterns).flatMap {
          (scrutinee, pattern) => pattern.buildMatchBindings(scrutinee)
        }.map {
          case (param, ty) => (param, Typed[Value](Value.variable(param, ty), ty))
        }
        env.withLocals(bindings.toMap) { implicit env => clause.body.infer(env) }
      }
      clausesType.reduce((a, b) => a leastUpperBound b)
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
        if !(paramType <:< argType) then TypeNotMatch.raise {
          s"Expected argument type: ${paramType.readBack}, but got: ${argType.readBack}"
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
        if candidateStates.isEmpty then OverloadingNotMatch.raise {
          s"No matched overloading in overloaded Pi type of argument with type: ${argType.readBack}"
        }
        // Find the states that there is no other state that is closer to the argument type
        val validStates = candidateStates.filter {
          (paramType, _) => !candidateStates.exists { (paramType2, _) =>
            paramType2 != paramType && paramType2 <:< paramType
          }
        }
        if validStates.size > 1 then OverloadingAmbiguous.raise {
          s"Ambiguous overloading in overloaded Pi type of argument with type: ${argType.readBack}"
        }
        val (_, codomain) = validStates.head
        codomain(arg.eval)
      }

      case _ => TypeNotMatch.raise {
        s"Cannot apply an argument to a non-function value: $fn"
      }
    }

    // Lambda returns a dependent function type
    case Lambda(param, body) => {
      val typedParam = param.map(_.eval)
      Value.Pi(typedParam.`type`, neutralClosure(typedParam, implicit env => body.infer, env))
    }

    case OverloadedLambda(states) => Value.OverloadedPi(states.map { (param, body) =>
      val typedParam = param.map(_.eval)
      (typedParam.`type`, neutralClosure(typedParam, implicit env => body.infer, env))
    })

    case Projection(record, field) => record.infer match {
      case Value.RecordType(fields) => fields.getOrElse(field, {
        RecordMissingField.raise(s"Field $field not found in record $record")
      })
      case _ => TypeNotMatch.raise(s"Cannot project from non-record value: $record")
    }

  }

  def partialEval(implicit env: Environment.Typed[Value]): Value = this.eval(evalMode = EvalMode.Partial)

  def eval(implicit env: Environment.Typed[Value]): Value = this.eval(evalMode = EvalMode.Normal)

  def forceEval(implicit env: Environment.Typed[Value]): Value = this.eval(evalMode = EvalMode.Force)

  def eval(evalMode: EvalMode = EvalMode.Normal)(
    implicit env: Environment.Typed[Value]
  ): Value = this match {

    case Universe => Value.Universe

    case Primitive(value) => Value.Primitive(value)

    case PrimitiveType(ty) => Value.PrimitiveType(ty)
    
    case Variable(variable, ty) => env.getValue(variable) match {
      case Some(value) => value
      case None => ty.toOption match {
        // If the variable is associated with a type, then it is a neutral value
        case Some(ty) => Value.variable(variable, ty)
        case None => throw new NoSuchElementException(s"Variable $variable not found in the environment")
      }
    }

    case FunctionInvoke(fnRef, argTerms) => {

      lazy val argsValue: Seq[Value] = argTerms.map(_.eval(evalMode))

      if evalMode == EvalMode.Partial then {
        return Value.functionInvoke(fnRef, argsValue)
      }

      val function: Function[Term] = env.getSymbol(fnRef).get match {
        case function: Function[Term] => function
        case overloaded: Overloaded[Term] => fnRef.definition.toOption match {
          case Some(function) => function
          case None => Term.OverloadInvoke(overloaded.ident, argTerms).getOverload.asInstanceOf[Function[Term]]
        }
        case _: Declaration[Term, Function] @unchecked => {
          // If the function is a pre-declared function, keep it as a neutral value
          return Value.functionInvoke(fnRef, argsValue)
        }
        case _ => DefinitionNotMatch.raise {
          s"Expected function, but got: ${fnRef.name}"
        }
      }

      lazy val allArgumentsFinal = argsValue.forall(_.isFinal(Set.empty))
      lazy val argVarList: Seq[(Var.Local, Typed[Value])] = function.arguments(argsValue).map {
        (param, arg) => (param.ident, Typed[Value](arg, param.`type`.eval(evalMode)))
      }

      lazy val evaluatedFunctionBody: Value = function match {
        case fn: DefinedFunction[Term] => {
          env.invokeFunction(fn.ident) { implicit env =>
            env.withLocals(argVarList.toMap) {
              implicit env => fn.body.get.eval(evalMode)(env)
            }
          }
        }
        case fn: NativeFunction[Term] => {
          // Only evaluate the native function if all arguments are final
          if allArgumentsFinal then {
            // TODO: arguments apply mode
            fn.invoke(argVarList.map { (_, typed) => Argument(typed.value) })
          } else {
            Value.functionInvoke(fn.ident, argsValue)
          }
        }
      }

      env.currentDefinition match {
        case Some(current: Var.Defined[Term, Function] @unchecked) if evalMode == EvalMode.Force => {
          if !function.isRecursive || !function.dependencies.contains(current) || allArgumentsFinal then {
            evaluatedFunctionBody
          } else {
            Value.functionInvoke(function.ident, argsValue)
          }
        }
        case None | Some(_) => {
          if !function.isRecursive || allArgumentsFinal then {
            evaluatedFunctionBody
          } else {
            Value.functionInvoke(function.ident, argsValue)
          }
        }
      }
    }

    case invoke: OverloadInvoke => {
      Term.functionInvoke(invoke.getOverload.ident.asInstanceOf, invoke.args).eval(evalMode)
    }

    case InductiveType(indRef, argTerms) => {
      val argsValue: Seq[Value] = argTerms.map(_.eval(evalMode))
      Value.inductiveType(indRef, argsValue)
    }

    case InductiveVariant(inductiveTerm, constructor, args) => inductiveTerm.eval(evalMode) match {
      case inductiveType: Value.InductiveType => {
        val argValues = env.withLocals(inductiveType.argsMap) { implicit env => args.map(_.eval(evalMode)) }
        Value.inductiveVariant(inductiveType, constructor, argValues)
      }
      case ty => TypeNotMatch.raise(s"Expected inductive type, but got: ${ty.readBack}")
    }

    case Match(scrutinees, clauses) => {
      val scrutineesValue = scrutinees.map(_.eval(evalMode))
      // If all scrutinees are final, try to match the clauses
      if scrutineesValue.forall(_.isFinal(Set.empty)) then {
        // Try to match the scrutinees with the clauses
        clauses.tryMatch(scrutineesValue).getOrElse {
          // If all scrutinees are final and no match is found, raise an error
          MatchNotExhaustive.raise {
            s"Match is not exhaustive for scrutinee: ${scrutineesValue.map(_.readBack).mkString(", ")}"
          }
        }
      } else {
        // Otherwise (at least one scrutinee contains neutral value), keep the match as a neutral value
        val valueClauses = clauses.map { clause =>
          // Bind the pattern variables to the scrutinee values
          val bindings: Seq[(Var.Local, Typed[Value])] = scrutineesValue.zip(clause.patterns).flatMap {
            (scrutinee, pattern) => pattern.buildMatchBindings(scrutinee.infer)
          }.map {
            case (param, ty) => (param, Typed[Value](Value.variable(param, ty), ty))
          }
          val body = env.withLocals(bindings.toMap) { implicit env =>
            clause.body.infer match {
              case Value.PrimitiveType(LiteralType.NothingType) => clause.body.partialEval
              case _ => clause.body.eval(evalMode)
            }
          }
          Clause(clause.patterns.map(_.map(_.eval(evalMode))), body)
        }
        Value.Neutral(NeutralValue.Match(scrutineesValue, valueClauses))
      }
    }

    case piType: Pi => piType.eval(Value.Pi.apply, evalMode)

    case piType: OverloadedPi => piType.eval(Value.OverloadedPi.apply, evalMode)

    case sigmaType: Sigma => sigmaType.eval(Value.Sigma.apply, evalMode)

    case Record(fields) => Value.Record(fields.map((name, term) => (name, term.eval(evalMode))))

    case RecordType(fields) => Value.RecordType(fields.map((name, ty) => (name, ty.eval(evalMode))))

    case Apply(fn, arg) => fn.eval(evalMode) match {

      case Value.Lambda(paramType, bodyClosure) => {
        if !(paramType <:< arg.infer) then TypeNotMatch.raise {
          s"Expected argument type: ${paramType.readBack}, but got: ${arg.infer.readBack}"
        }
        bodyClosure(arg.eval(evalMode))
      }

      case overloaded: Value.OverloadedLambda => {
        overloaded.applyArgument(
          arg.eval(evalMode), arg.infer, Value.OverloadedLambda.apply,
          unwrapStates = {
            case Value.OverloadedLambda(states) => states
            case value => TypeNotMatch.raise {
              s"Expected an overloaded lambda, but got: ${value.readBack}"
            }
          }
        )
      }

      // If the evaluation of the function stuck, the whole application is stuck
      // Thus, no need for considering the situation that function is a global call
      // because it is tried to be evaluated before but failed
      case Value.Neutral(neutral) => Value.Neutral(NeutralValue.Apply(neutral, arg.eval(evalMode)))

      case _ => TypeNotMatch.raise(s"Cannot apply an argument to a non-function value: $fn")
    }

    case lambda: Lambda => lambda.eval(Value.Lambda.apply, evalMode)

    case lambda: OverloadedLambda => lambda.eval(Value.OverloadedLambda.apply, evalMode)

    case Projection(record, field) => record.eval match {
      case Value.Record(fields) => fields.getOrElse(field, {
        RecordMissingField.raise(s"Field $field not found in record")
      })
      case neutral: Value.Neutral => Value.Neutral(NeutralValue.Projection(neutral, field))
      case _ => TypeNotMatch.raise(s"Cannot project from non-record value: $record")
    }
  }

  /**
   * Substitute a term with another term based on the semantics, 
   * taking into account an implicit environment for type and value resolution.
   * @param from The term to replace.
   * @param to The term to substitute with.
   * @param env The environment containing type and value bindings.
   * @return The new term with substitutions made, considering semantics.
   */
  def substitute(from: Term, to: Term)(implicit env: Environment.Typed[Value]): Term = this match {
    case `from` if this unify from => to
    case Universe => Universe
    case Primitive(_) => this
    case PrimitiveType(_) => this
    case Variable(variable, _) => env.getTyped(variable) match {
      case Some(typed) if typed.value.readBack unify from => to
      case _ => this
    }
    case FunctionInvoke(fn, args) => FunctionInvoke(fn, args.map(_.substitute(from, to)))
    case OverloadInvoke(fn, args) => OverloadInvoke(fn, args.map(_.substitute(from, to)))
    case InductiveType(inductive, args) => InductiveType(inductive, args.map(_.substitute(from, to)))
    case InductiveVariant(inductive, constructor, args) => {
      InductiveVariant(inductive.substitute(from, to), constructor, args.map(_.substitute(from, to)))
    }
    case Pi(param, codomain) => {
      val newParam = param.map(_.substitute(from, to))
      val newCodomain = codomain.substitute(from, to)
      if (newParam.`type` unify from) Pi(newParam, codomain) else Pi(newParam, newCodomain)
    }
    case OverloadedPi(states) => OverloadedPi(states.view.mapValues(_.substitute(from, to)).toMap)
    case Sigma(param, codomain) => {
      val newParam = param.map(_.substitute(from, to))
      val newCodomain = codomain.substitute(from, to)
      if newParam.`type` unify from then Sigma(newParam, codomain)
      else Sigma(newParam, newCodomain)
    }
    case Record(fields) => Record(fields.view.mapValues(_.substitute(from, to)).toMap)
    case RecordType(fields) => RecordType(fields.view.mapValues(_.substitute(from, to)).toMap)
    case Apply(fn, arg) => Apply(fn.substitute(from, to), arg.substitute(from, to))
    case Lambda(param, body) => {
      val newParam = param.map(_.substitute(from, to))
      if (newParam.`type` unify from) Lambda(newParam, body) else Lambda(newParam, body.substitute(from, to))
    }
    case OverloadedLambda(states) => OverloadedLambda(states.view.mapValues(_.substitute(from, to)).toMap)
    case Projection(record, field) => Projection(record.substitute(from, to), field)
    case Match(scrutinees, clauses) => {
      val newScrutinees = scrutinees.map(_.substitute(from, to))
      val newClauses = clauses.map { clause =>
        val bindings: Seq[(Var.Local, Typed[Value])] = scrutinees.zip(clause.patterns).flatMap {
          (scrutinee, pattern) => pattern.buildMatchBindings(scrutinee.infer)
        }.map {
          case (param, ty) => (param, Typed[Value](Value.variable(param, ty), ty))
        }
        val body = env.withLocals(bindings.toMap) { implicit env =>
          clause.body.substitute(from, to)
        }
        Clause(clause.patterns.map(_.map(_.substitute(from, to))), body)
      }
      Match(newScrutinees, newClauses)
    }
  }

  infix def unify(that: Term)(implicit env: Environment.Typed[Value]): Boolean = this.eval unify that.eval

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

  override def variable(ident: Var.Local, ty: Term): Term = Variable(ident)

  override def inductiveType(
    inductive: Var.Defined[Term, Inductive], args: Seq[Term]
  ): Term = InductiveType(inductive, args)

  override def functionInvoke(
    function: Var.Defined[Term, Function], args: Seq[Term]
  ): Term = FunctionInvoke(function, args)

  override def inductiveVariant(
    inductive: Term, constructor: Constructor[Term], args: Seq[Term]
  ): Term = InductiveVariant(inductive, constructor, args)

  def overloaded[T <: Term & OverloadedTermExt[T]](
    constructor: Map[Param[Term], Term] => T,
    paths: Seq[(Seq[Param[Term]], Term)],
  ): T = paths.foldLeft(constructor(Map.empty)) {
    case (overloaded, (path, body)) => addOverloadedPath(overloaded, path, body)
  }

  private def addOverloadedPath[T <: Term & OverloadedTermExt[T]](
    overloaded: T, path: Seq[Param[Term]], body: Term,
  ): T = path match {
    case Nil => overloaded
    case head +: tail => {
      val updatedState: Term = overloaded.states.get(head) match {
        case Some(term: Term.OverloadedLambda) => addOverloadedPath(term, tail, body)
        case Some(_) => OverloadingAmbiguous.raise(s"Ambiguous overloading for ${overloaded}")
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
  ): (Type, Value => Value) = {
    val typedParam = param.map(_.eval(evalMode))
    (typedParam.`type`, neutralClosure(typedParam, implicit env => term.eval(evalMode), env))
  }

}

private sealed trait LambdaLikeTerm {
  def param: Param[Term]
  def body: Term

  def eval(constructor: (Type, Value  => Value) => Value, evalMode: EvalMode)(
    implicit env: Environment.Typed[Value]
  ): Value = {
    val (paramType, closure) = Term.evalParameterized(param, body, evalMode)
    constructor(paramType, closure)
  }

  def toOverloaded: OverloadedTermExt[?] & Term = this match {
    case lambda: Term.Lambda => Term.OverloadedLambda(Map(lambda.param -> lambda.body))
    case pi: Term.Pi => Term.OverloadedPi(Map(pi.param -> pi.codomain))
    case _ => unreachable
  }
}

private sealed trait PiLikeTerm extends LambdaLikeTerm {
  def codomain: Term
  override def body: Term = codomain
}

private sealed trait OverloadedTermExt[S <: Term & OverloadedTermExt[S]] {

  def states: Map[Param[Term], Term]

  def eval(constructor: Map[Type, Value => Value] => Value, evalMode: EvalMode)(
    implicit env: Environment.Typed[Value]
  ): Value = {
    // Normalize the states (unify the parameters with identical type but different names)
    val parameterizedTerms: Seq[(Param[Term], Term)] = states.toSeq.map {
      (param, term) => Term.evalParameterized(param, term, evalMode)
    }.map {
      (paramType, closure) => Value.readBackClosure(paramType, closure)
    }

    // Check whether there is any parameter type that is associated with multiple names
    val normalizedParams: Seq[(Var.Local, Type)] = parameterizedTerms.map {
      (param, _) => (param.ident, param.`type`.eval)
    }
    assert(!normalizedParams.exists { (ident, ty) =>
      normalizedParams.count((ident2, ty2) => (ty unify ty2) && ident != ident2) > 1
    })

    // Merge the states with identical parameter types
    //  1. Group the states by parameter type
    val grouped: Map[Param[Term], Seq[Term]] = parameterizedTerms.groupMap(_._1)(_._2)

    //  2. Merge the states with identical parameter types
    val merged: Map[Param[Term], Term] = grouped.map { (param, terms) =>
      assert(terms.nonEmpty)
      if terms.size == 1 then {
        param -> terms.head
      } else {
        // There are multiple states with the same parameter type, merge them
        val merged: Term & OverloadedTermExt[?] = terms.map {
          // case 1: The term is a lambda-like term, convert it to an overloaded lambda
          case lambdaLikeTerm: LambdaLikeTerm => lambdaLikeTerm.toOverloaded
          // case 2: The term is an overloaded lambda, keep it as is
          case overloaded: OverloadedTermExt[?] => overloaded
          // case 3: The term is not a lambda-like term, indicating an ambiguous overload
          case _ => OverloadingAmbiguous.raise {
            s"Ambiguous overloading for function: ${param.ident.name}"
          }
        }.reduce { (merged, overloaded) => merged.merge(overloaded) }
        param -> merged
      }
    }

    // TODO: This need to be optimized, because the states
    //  are already evaluated before
    constructor(merged.map((param, term) => Term.evalParameterized(param, term, evalMode)))
  }

  def copy(states: Map[Param[Term], Term]): S

  @SuppressWarnings(Array("unchecked"))
  private def merge(other: OverloadedTermExt[?]): S = {
    assert(this.getClass == other.getClass)
    val mergedStates = (this.states.keySet ++ other.states.keySet).map { param =>
      val term: Term = (this.states.get(param), other.states.get(param)) match {
        case (Some(term1: Term), Some(term2: Term)) => (term1, term2) match {
          // States are overloaded lambdas, merge them recursively
          case (overloaded1: OverloadedTermExt[?], overloaded2: OverloadedTermExt[?]) => {
            // Recursively merge the states
            try overloaded1.asInstanceOf[S].merge(overloaded2.asInstanceOf[S]) catch {
              // If the merge fails, the states are not compatible (e.g. trying to merge a Pi and a Lambda)
              case _: ClassCastException => TypeNotMatch.raise {
                s"Cannot merge states of different types: ${term1}, ${term2}"
              }
            }
          }
          // States are not overloaded lambdas, indicating a mismatch
          case _ => TypeNotMatch.raise(s"Cannot merge states of different types: ${term1}, ${term2}")
        }
        case (Some(term: Term), _) => term
        case (_, Some(term: Term)) => term
        case (None, None) => unreachable
      }
      param -> term
    }.toMap[Param[Term], Term]
    this.copy(mergedStates)
  }
}

private sealed trait OverloadInvokeExt {
  def fn: Var.Defined[Term, Overloaded]
  def args: Seq[Term]

  /**
   * Get the most suitable state of the overloaded function that matches the argument types.
   * @param env The environment
   * @return The most suitable eigenstate of the overloaded function body
   */
  def getOverload(implicit env: Environment.Typed[Value]): NaiveSymbol[Term] = {
    
    val fn = env.getSymbol(this.fn).get.asInstanceOf[OverloadedSymbol[Term, ?, ? <: NaiveSymbol[Term]]]

    // Filter out the candidate overloads that fit the argument types
    val candidateOverloads = fn.overloads.filter { overload =>
      val params = overload.params
      if params.size != this.args.size then false
      else params.zip(this.args).forall {
        (param, arg) => param.`type`.eval <:< arg.infer
      }
    }

    // If no candidate overload fits the argument types, throw an error
    if candidateOverloads.isEmpty then {
      NoSuchOverloading.raise {
        s"No overloading of function ${fn.ident.name} found for arguments of types: " +
        this.args.map(_.infer.readBack).mkString(", ")
      }
    }

    // Otherwise, find the most suitable one
    // The most suitable one is the one that has the most specific parameter types

    // Iterate through each parameter position, eliminating branches that are not
    // the most specific at that position
    var remainingOverloads = candidateOverloads
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
      OverloadingAmbiguous.raise {
        s"Ambiguous overloading for function ${fn.ident.name} with arguments of types: " +
        this.args.map(_.infer.readBack).mkString(", ")
      }
    }

    remainingOverloads.head
  }
}

enum EvalMode {
  case Partial
  case Normal
  case Force
}
