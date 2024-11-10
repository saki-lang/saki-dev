package saki.core.syntax

import saki.core.{RuntimeEntity, RuntimeEntityFactory}
import saki.core.context.{Environment, Typed}
import saki.core.domain.{NeutralValue, Type, Value}
import saki.error.CoreErrorKind.*
import saki.error.PanicError
import saki.util.unreachable

import scala.collection.Seq

enum Term extends RuntimeEntity[Type] {

  case Universe

  case Primitive(value: Literal)

  case PrimitiveType(`type`: LiteralType)

  case Variable(variable: Var.Local)

  case TypeBarrier(term: Term, `type`: Term)

  case Union(types: Set[Term])  // Sum type

  case Intersection(types: Set[Term])

  case FunctionInvoke(fn: Var.Defined[Term, Function], args: Seq[Term])

  case OverloadInvoke(fn: Var.Defined[Term, Overloaded], args: Seq[Term]) extends Term

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

  case Pair(first: Term, second: Term) // instances of Sigma type

  case Projection(record: Term, field: String)

  def normalize(implicit env: Environment.Typed[Value]): Term = this.eval.readBack

  /**
   * Infer the type of the term
   * @param env The environment
   * @return The inferred type
   */
  override def infer(implicit env: Environment.Typed[Value]): Value = this match {
    // Just a simple trick, avoid unnecessary evaluation, and should be removed if causing any problem
    case Variable(variable) => env.locals.get(variable) match {
      case Some(value) => value.`type`
      case None => throw new IllegalArgumentException(s"Variable not found: ${variable.name}")
    }
    case _ => this.eval.infer
  }

  // For non-inline functions
  def partialEval(implicit env: Environment.Typed[Value]): Value = this.eval(evalMode = EvalMode.Partial)

  def eval(implicit env: Environment.Typed[Value]): Value = this.eval(evalMode = EvalMode.Normal)

  def forceEval(implicit env: Environment.Typed[Value]): Value = this.eval(evalMode = EvalMode.Force)

  def eval(evalMode: EvalMode = EvalMode.Normal)(
    implicit env: Environment.Typed[Value]
  ): Value = this match {

    case Universe => Value.Universe

    case Primitive(value) => Value.Primitive(value)

    case PrimitiveType(ty) => Value.PrimitiveType(ty)
    
    case Variable(variable) => env.getValue(variable) match {
      case Some(value) => value
      case None => throw new IllegalArgumentException(s"Variable not found: ${variable.name}")
    }

    case TypeBarrier(term, ty) => {
      val value: Value = term.eval(evalMode)
      val termType: Type = value.infer
      val expectedType: Type = ty.eval(evalMode)
      if expectedType <:< termType then {
        term.eval(evalMode)
      } else {
        Value.typeBarrier(term.eval(evalMode), expectedType)
      }
    }

    case Union(types) => {
      // Return the least upper bound of the types
      val typeValues = types.map(_.eval(evalMode))
      typeValues.reduce((a, b) => a \/ b)
    }

    case Intersection(types) => {
      // Return the greatest lower bound of the types
      val typeValues = types.map(_.eval(evalMode))
      typeValues.reduce((a, b) => a /\ b)
    }

    case Pair(first, second) => Value.Pair(first.eval(evalMode), second.eval(evalMode))

    case FunctionInvoke(fnRef, argTerms) => {

      lazy val argsValue: Seq[Value] = argTerms.map(_.eval(evalMode))

      if evalMode == EvalMode.Partial then {
        return Value.functionInvoke(fnRef, argsValue)
      }

      val function: Function[Term] = env.getSymbol(fnRef).get match {
        case function: Function[Term] => function
        case overloaded: Overloaded[Term] => fnRef.definition.toOption match {
          case Some(function) => function
          case None => overloaded.getOverload(argTerms).asInstanceOf[Function[Term]]
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
      lazy val (bodyEnv, argVarList) = function.mapArgs(argsValue).foldLeft((env, Seq.empty[(Var.Local, Typed[Value])])) {
        case ((env, argList), (param, arg)) => {
          given Environment.Typed[Value] = env
          val paramType = param.`type`.eval(evalMode)
          val updatedEnv = env.add(param.ident, arg, paramType)
          val argPair: (Var.Local, Typed[Value]) = (param.ident, Typed[Value](arg, paramType))
          (updatedEnv, argList :+ argPair)
        }
      }

      lazy val evaluatedFunctionBody: Value = function match {

        case fn: DefinedFunction[Term] => {
          bodyEnv.invokeFunction(fn.ident) {
            implicit env => fn.body.get.eval(evalMode)(env)
          }
        }

        case fn: NativeFunction[Term] => {
          // Only evaluate the native function if all arguments are final
          if allArgumentsFinal then {
            // TODO: arguments apply mode
            try fn.invoke(argVarList.map { (_, typed) => Argument(typed.value) }) catch {
              case e: PanicError => {
                if evalMode == EvalMode.Force then throw e
                else Value.functionInvoke(fn.ident, argsValue)
              }
            }
          } else {
            Value.functionInvoke(fn.ident, argsValue)
          }
        }
      }

      env.currentDefinition match {
        case Some(current: Var.Defined[Term, Function] @unchecked) => {
          if !function.isRecursive || !function.dependencies.contains(current) || allArgumentsFinal then {
            if evaluatedFunctionBody.containsMatching
            then Value.functionInvoke(fnRef, argsValue)
            else evaluatedFunctionBody
          } else {
            Value.functionInvoke(function.ident, argsValue)
          }
        }
        case None | Some(_) => {
          if evaluatedFunctionBody.containsMatching
          then Value.functionInvoke(fnRef, argsValue)
          else evaluatedFunctionBody
        }
      }
    }

    case OverloadInvoke(fn, args) => {
      Term.functionInvoke(fn.definition.get.getOverload(args).asInstanceOf, args).eval(evalMode)
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
      // Try to match the scrutinees with the clauses
      clauses.tryMatch(scrutineesValue, evalMode).getOrElse {
        // If all scrutinees are final and no match is found, raise an error
        MatchNotExhaustive.raise {
          s"Match is not exhaustive for scrutinee: ${scrutineesValue.map(_.readBack).mkString(", ")}"
        }
      }
    }

    case piType: Pi => piType.eval(Value.Pi.apply, evalMode)

    case piType: OverloadedPi => piType.eval(Value.OverloadedPi.apply, evalMode)

    case sigmaType: Sigma => sigmaType.eval(Value.Sigma.apply, evalMode)

    case Record(fields) => Value.Record(fields.map((name, term) => (name, term.eval(evalMode))))

    case RecordType(fields) => Value.RecordType(fields.map((name, ty) => (name, ty.eval(evalMode))))

    case Apply(fn, arg) => fn.eval(evalMode) match {

      case Value.Lambda(paramType, closure) => {
        if !(paramType <:< arg.infer) then TypeNotMatch.raise {
          s"Expected argument type: ${paramType.readBack}, but got: ${arg.infer.readBack}"
        }
        closure(arg.eval(evalMode))
      }

      case overloaded: Value.OverloadedLambda => {
        val argValue: Value = arg.eval(evalMode)
        overloaded.applyArgument(
          argValue, arg.infer, Value.OverloadedLambda.apply,
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
    case Variable(variable) => env.getTyped(variable) match {
      case Some(typed) if typed.value.readBack unify from => to
      case _ => this
    }
    case Union(types) => Union(types.map(_.substitute(from, to)))
    case Intersection(types) => Intersection(types.map(_.substitute(from, to)))
    case Pair(first, second) => Pair(first.substitute(from, to), second.substitute(from, to))
    case TypeBarrier(term, ty) => TypeBarrier(term.substitute(from, to), ty.substitute(from, to))
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
          (scrutinee, pattern) => pattern.buildTypeMapping(scrutinee.infer)
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

  def contains(ident: Var.Local, shadowed: Set[Var.Local] = Set.empty)(
    implicit env: Environment.Typed[Value]
  ): Boolean = this match {
    case Universe => false
    case Primitive(_) => false
    case PrimitiveType(_) => false
    case Variable(variable) => variable == ident && !shadowed.contains(variable)
    case Union(types) => types.exists(_.contains(ident, shadowed))
    case Intersection(types) => types.exists(_.contains(ident, shadowed))
    case TypeBarrier(term, _) => term.contains(ident, shadowed)
    case FunctionInvoke(_, args) => args.exists(_.contains(ident, shadowed))
    case OverloadInvoke(_, args) => args.exists(_.contains(ident, shadowed))
    case InductiveType(_, args) => args.exists(_.contains(ident, shadowed))
    case InductiveVariant(inductive, _, args) => {
      inductive.contains(ident, shadowed) || args.exists(_.contains(ident, shadowed))
    }
    case Pi(param, codomain) => codomain.contains(ident, shadowed + param.ident)
    case OverloadedPi(states) => states.exists((param, codomain) => codomain.contains(ident, shadowed + param.ident))
    case Sigma(param, codomain) => codomain.contains(ident, shadowed + param.ident)
    case Record(fields) => fields.values.exists(_.contains(ident, shadowed))
    case RecordType(fields) => fields.values.exists(_.contains(ident, shadowed))
    case Apply(fn, arg) => fn.contains(ident, shadowed) || arg.contains(ident, shadowed)
    case Lambda(param, body) => body.contains(ident, shadowed + param.ident)
    case OverloadedLambda(states) => states.exists((param, body) => body.contains(ident, shadowed + param.ident))
    case Pair(first, second) => first.contains(ident, shadowed) || second.contains(ident, shadowed)
    case Projection(record, _) => record.contains(ident, shadowed)
    case Match(scrutinees, clauses) => {
      scrutinees.exists(_.contains(ident, shadowed)) || clauses.exists { clause =>
        val bindings = clause.patterns.zip(scrutinees).flatMap {
          (pattern, scrutinee) => pattern.buildTypeMapping(scrutinee.infer)
        }.map { (param, _) => (param) }
        clause.body.contains(ident, shadowed ++ bindings)
      }
    }
  }

  override def toString: String = this.toString(_.toString)

  def toString(fmt: Term => String): String = this match {
    case Universe => s"'Type"
    case Primitive(value) => value.toString
    case PrimitiveType(ty) => ty.toString
    case Variable(variable) => variable.name
    case TypeBarrier(term, ty) => s"($term : $ty)"
    case Union(types) => s"(${types.map(fmt).mkString(" | ")})"
    case Intersection(types) => s"(${types.map(fmt).mkString(" & ")})"
    case FunctionInvoke(fn, args) => s"${fn.name}(${args.map(fmt).mkString(", ")})"
    case OverloadInvoke(fn, args) => s"${fn.name}(${args.map(fmt).mkString(", ")})"
    case InductiveType(inductive, args) => {
      val argsStr = if args.nonEmpty then s"(${args.map(fmt).mkString(", ")})" else ""
      s"${inductive.name}$argsStr"
    }
    case InductiveVariant(inductive, constructor, args) => {
      val argsStr = if args.nonEmpty then s"(${args.map(fmt).mkString(", ")})" else ""
      s"$inductive::${constructor.ident}$argsStr"
    }
    case Match(scrutinees, clauses) => {
      val scrutineesStr = if scrutinees.size > 1 then {
        s"(${scrutinees.map(fmt).mkString(", ")})"
      } else scrutinees.head.toString
      s"match ($scrutineesStr) { ${clauses.mkString(" | ")} }"
    }
    case Pi(param, codomain) => s"Π(${param.name} : ${fmt(param.`type`)}) -> $codomain"
    case OverloadedPi(states) => {
      states.map {
        (param, body) => s"(Π(${param.name} : ${fmt(param.`type`)}) -> $body)"
      }.mkString(" ⊕ ")
    }
    case Sigma(param, codomain) => s"Σ(${param.name} : ${fmt(param.`type`)}) -> $codomain"
    case Record(fields) => s"{ ${fields.map { case (k, v) => s"$k = ${fmt(v)}" }.mkString(", ")} }"
    case RecordType(fields) => s"record { ${fields.map { case (k, v) => s"$k: ${fmt(v)}" }.mkString(", ")} }"
    case Apply(fn, arg) => s"(${fmt(fn)} ${fmt(arg)})"
    case Lambda(param, body) => s"λ(${param.name} : ${fmt(param.`type`)}) => $body"
    case OverloadedLambda(states) => {
      states.map {
        (param, body) => s"(λ(${param.name} : ${fmt(param.`type`)}) => $body)"
      }.mkString(" ⊕ ")
    }
    case Pair(first, second) => s"(${fmt(first)}, ${fmt(second)})"
    case Projection(record, field) => s"${fmt(record)}.$field"
  }

  def evalString(implicit env: Environment.Typed[Value]): String = this match {
    case Pi(param, codomain) => {
      if !codomain.contains(param.ident) then {
        s"${param.`type`} -> ${codomain.evalString}"
      } else this.toString
    }
    case _ => this.toString(_.evalString)
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
        case Some(term: Term.OverloadedPi) => addOverloadedPath(term, tail, body)
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

private sealed trait LambdaLikeTerm {
  def param: Param[Term]
  def body: Term

  def eval(constructor: (Type, CodomainClosure) => Value, evalMode: EvalMode)(
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

  def eval(constructor: Map[Type, CodomainClosure] => Value, evalMode: EvalMode)(
    implicit env: Environment.Typed[Value]
  ): Value = {
    // Normalize the states (unify the parameters with identical type but different names)
    val closureStates: Seq[(Type, CodomainClosure)] = states.toSeq.map {
      (param, term) => Term.evalParameterized(param, term, evalMode)
    }

    // Merge the states with identical parameter types
    //  1. Group the states by parameter type
    val grouped: Map[Type, Iterable[CodomainClosure]] = closureStates.groupBy(_._1.readBack).flatMap {
      (_, states) => states.map(states.head._1 -> _._2)
    }.groupMap(_._1)(_._2)

    //  2. Merge the states with identical parameter types
    val merged: Map[Type, CodomainClosure] = grouped.map { (paramType, closures) =>
      assert(closures.nonEmpty)
      if closures.size == 1 then {
        paramType -> closures.head
      } else {
        val paramIdent = env.uniqueVariable
        val variable = Value.variable(paramIdent, paramType)
        env.withLocal(paramIdent, variable, paramType) { implicit env =>
          val terms = closures.map(_(variable).readBack)
          val merged = terms.map {
            // case 1: The term is a lambda-like term, convert it to an overloaded lambda
            case lambdaLikeTerm: LambdaLikeTerm => lambdaLikeTerm.toOverloaded
            // case 2: The term is an overloaded lambda, keep it as is
            case overloaded: OverloadedTermExt[?] => overloaded
            // case 3: The term is not a lambda-like term, indicating an ambiguous overload
            case _ => OverloadingAmbiguous.raise {
              s"Ambiguous overloading for function: ${paramType}"
            }
          }.reduce {
            (merged, overloaded) => merged.merge(overloaded)
          }
          Term.evalParameterized(paramType, merged, evalMode)
        }
      }
    }
    constructor(merged)
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

enum EvalMode {
  case Partial
  case Normal
  case Force
}
