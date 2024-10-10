package saki.core.elaborate

import saki.core.{Param, TypeError}
import saki.core.context.{Environment, Typed}
import saki.core.domain.{Type, Value}
import saki.core.syntax.{*, given}
import saki.util.unreachable

import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

private[core] object Synthesis {

  def elaborate(expr: Expr, expectedType: Term)(
    implicit env: Environment.Typed[Value]
  ): Term = expr match {
    case Expr.Lambda(lambdaParam, body, returnType) => {
      // Check that expectedType is a Pi type
      expectedType.normalize match {
        case Term.Pi(piParam, codomain) => Term.Lambda(
          param = lambdaParam.map(_.elaborate(piParam.`type`)),
          body = env.withLocal(lambdaParam.ident, piParam.`type`.eval, piParam.`type`.infer) {
            // Check that the body type is the same as the codomain of the Pi type
            returnType.map(_.synth.term.eval unify codomain.eval) match {
              case Some(false) => TypeError.mismatch(codomain.toString, returnType.get.toString, expr.span)
              case _ => body.elaborate(codomain)
            }
          }
        )
        case ty => TypeError.mismatch("Π (x : A) -> B", ty.toString, expr.span)
      }
    }
    case Expr.Hole(_) =>  TypeError.error("Holes are not allowed in this context", expr.span)
    case _ => {
      val synthResult = expr.synth
      if !(synthResult.`type` unify expectedType.eval) then {
        TypeError.mismatch(expectedType.toString, synthResult.`type`.toString, expr.span)
      }
      synthResult.term
    }
  }

  case class Synth(term: Term, `type`: Type) {

    def unpack: (Term, Type) = (term, `type`)

  }

  def synth(expr: Expr)(implicit env: Environment.Typed[Value]): Synth = expr match {

    case Expr.Unresolved(name) => {
      try {
        env.getTyped(name) match {
          case Some(Typed(value, ty)) => Synth(value.readBack, ty)
          // If the variable is not found in the environment,
          // indicating that it is a primitive variable or an unknown variable,
          // try to synthesize it as a primitive type
          case None => synthPrimitiveType(name)
        }
      } catch { // TODO: refactor error handling
        case TypeError(message, _) => TypeError.error(message, expr.span)
      }
    }

    case Expr.Universe() => Synth(Term.Universe, Value.Universe)

    case Expr.Primitive(value) => Synth(Term.Primitive(value), Value.PrimitiveType(value.ty))

    case Expr.PrimitiveType(ty) => Synth(Term.PrimitiveType(ty), Value.Universe)

    case Expr.Variable(ref) => ref match {
      // Converting a definition reference to a lambda, enabling curry-style function application
      case definitionVar: Var.Defined[Term, ?] => definitionVar.definition.toOption match {
        case None => env.getDefinition(definitionVar) match {
          case Some(definition) => {
            definition.ident.definition :=! definition
            synthDefinitionRef(definition)
          }
          case None => TypeError.error(s"Unbound definition: ${definitionVar.name}", expr.span)
        }
        case Some(definition: Definition[Term]) => synthDefinitionRef(definition)
      }
      case variable: Var.Local => env.locals.get(variable) match {
        case Some(ty) => Synth(ty.value.readBack, ty.`type`)
        case None => TypeError.error(s"Unbound variable: ${variable.name}", expr.span)
      }
    }

    case Expr.Elimination(obj, member) => obj.synth(env).unpack match {
      // This is a project operation
      // `obj.field`
      // TODO: maybe we don't need to normalize here?
      case (term, recordType: Value.RecordType) => term match {
        case Term.Record(fields) => fields.get(member) match {
          case Some(value) => Synth(value, recordType.fields(member))
          case None => TypeError.error(s"Field not found: $member", expr.span)
        }
        case _ => Synth(Term.Projection(term, member), recordType.fields(member))
      }
      // This is a method call
      // `obj.method`
      case (term, _) => {
        val method: Function[Term] = env.getDefinitionByName(member) match {
          case Some(definition: Function[Term]) => definition
          case _ => TypeError.error(s"Method not found: $member", expr.span)
        }
        Synth(Term.FunctionInvoke(method.ident, Seq(term)), method.resultType.eval)
      }
    }

    case Expr.Apply(fnExpr, argExpr) => {
      val (fn, fnType) = fnExpr.synth.unpack
      fnType match {
        case Value.Pi(paramType, codomain) => {
          val paramIdent = env.uniqueVariable
          val param = Value.variable(paramIdent)
          env.withLocal(paramIdent, param, paramType) { implicit env =>
            val (arg, argType) = argExpr.value.synth(env).unpack
            if !(argType unify paramType) then {
              TypeError.mismatch(paramType.toString, argType.toString, argExpr.value.span)
            }
            Synth(fn.apply(arg), codomain(arg.eval))
          }
        }
        case _ => TypeError.mismatch("Π (x : A) -> B", fnType.toString, fnExpr.span)
      }
    }

    case Expr.Match(scrutinees, clauses) => {
      val scrutineesSynth: Seq[Synth] = scrutinees.map(_.synth)
      val clausesSynth: Seq[(Clause[Term], Term)] = clauses.map { clause =>
        synthClause(clause, scrutineesSynth)
      }
      val clauseBodyTypes: Seq[Term] = clausesSynth.map(_._2)
      if !clauseBodyTypes.tail.forall(_.eval unify clauseBodyTypes.head.eval) then {
        TypeError.error("Clauses have different types", expr.span)
      }
      Synth(
        term = Term.Match(scrutineesSynth.map(_.term), clausesSynth.map(_._1)),
        `type` = clauseBodyTypes.head.eval
      )
    }

    case Expr.Pi(param, result) => synthDependentType(param, result, Term.Pi.apply)

    case Expr.Sigma(param, result) => synthDependentType(param, result, Term.Sigma.apply)

    case Expr.Lambda(param, body, returnType) => {
      val paramIdent = param.ident
      val (paramType, _) = param.`type`.synth.unpack
      val paramTypeValue = paramType.eval
      val paramVariable = Value.variable(paramIdent)
      val (bodyTerm: Term, bodyType: Type) = env.withLocal(paramIdent, paramVariable, paramTypeValue) {
        implicit env => body.synth(env).unpack
      }
      val returnTypeValue: Value = returnType match {
        case Some(returnTypeExpr) => {
          val (returnType, _) = returnTypeExpr.synth.unpack
          val returnTypeValue = returnType.eval
          if !(returnTypeValue <:< bodyType) then {
            TypeError.mismatch(returnType.toString, bodyType.toString, returnTypeExpr.span)
          }
          returnTypeValue
        }
        case None => bodyType
      }

      // Closure for the Pi type
      def piTypeClosure(arg: Value): Value = {
        val argVar: Typed[Value] = Typed[Value](arg, paramTypeValue)
        env.withLocal(param.ident, argVar) {
          implicit env => returnTypeValue.readBack(env).eval(env)
        }
      }

      Synth(
        term = Term.Lambda(Param(paramIdent, paramType), bodyTerm),
        `type` = Value.Pi(paramTypeValue, piTypeClosure),
      )
    }

    case _ => TypeError.error("Failed to synthesis expression", expr.span)
    
  }

  def synthPrimitiveType(name: String): Synth = name match {
    case "'Type" => Synth(Term.Universe, Value.Universe)
    case "Nothing" => Synth(Term.PrimitiveType(LiteralType.NothingType), Value.Universe)
    case "Int" | "ℤ" => Synth(Term.PrimitiveType(LiteralType.IntType), Value.Universe)
    case "Float" | "ℝ" => Synth(Term.PrimitiveType(LiteralType.FloatType), Value.Universe)
    case "Bool" | "\uD835\uDD39" => Synth(Term.PrimitiveType(LiteralType.BoolType), Value.Universe)
    case "Char" => Synth(Term.PrimitiveType(LiteralType.CharType), Value.Universe)
    case "String" => Synth(Term.PrimitiveType(LiteralType.StringType), Value.Universe)
    case _ => TypeError.error(s"Unknown primitive type: $name")
  }

  private def synthDependentType(param: Param[Expr], result: Expr, constructor: (Param[Term], Term) => Term)(
    implicit env: Environment.Typed[Value]
  ): Synth = {
    val (paramType, paramTypeType) = param.`type`.synth.unpack
    val (codomain, _) = env.withLocal(param.ident, paramType.eval, paramTypeType) {
      result.synth(_).unpack
    }
    Synth(
      term = constructor(Param(param.ident, paramType), codomain),
      `type` = Value.Universe,
    )
  }

  def synthClause(clause: Clause[Expr], scrutinees: Seq[Synth])(
    implicit env: Environment.Typed[Value]
  ): (Clause[Term], Term) = {
    val patterns = clause.patterns.map(pattern => pattern.map(_.synth.term))
    val map = patterns.zip(scrutinees).foldLeft(Map.empty: Map[Var.Local, Term]) {
      case (subst, (pattern, param)) => subst ++ pattern.buildMatch(param.`type`.readBack)
    }.map { case (k, v) => k -> Typed[Value](Value.variable(k), v.eval) }
    val (body, ty) = env.withLocals[Synth](map) { clause.body.synth }.unpack
    (Clause(patterns, body), ty.readBack)
  }

  extension (definition: Definition[Expr]) {
    def synth(implicit env: Environment.Typed[Value]): Definition[Term] = synthDefinition(definition)
  }
  
  def synthDefinition(definition: Definition[Expr])(
    implicit env: Environment.Typed[Value]
  ): Definition[Term] = definition match {
    case Function(ident, paramExprs, resultTypeExpr, isNeutral, pristineBody) => {
      // TODO: Compute `isNeutral` using a reference graph
      val resultType = resultTypeExpr.elaborate(Term.Universe)
      val params = synthParams(paramExprs)
      val paramsType = params.map { param =>
        param.ident -> Typed[Value](Value.variable(param.ident), param.`type`.eval)
      }
      env.withLocals(paramsType.toMap) { implicit env =>
        val function = Function[Term](Var.Defined(ident.name), params, resultType, isNeutral)
        function.ident.definition := function
        function.body := env.withCurrentDefinition(function.ident) {
          pristineBody.get.elaborate(resultType)
        }
        function
      }
    }

    case inductiveExpr: Inductive[Expr] => {
      val params = synthParams(inductiveExpr.params)(env)
      val constructors = ArrayBuffer.empty[Constructor[Term]]
      val inductiveDefinition: Inductive[Term] = Inductive(Var.Defined(inductiveExpr.ident.name), params, constructors)
      inductiveDefinition.ident.definition := inductiveDefinition
      // To support recursive inductive types, we need to add the inductive type to the context
      // before synthesizing the constructors
      env.withCurrentDefinition[Inductive[Term]](inductiveDefinition.ident) { implicit env =>
        constructors ++= inductiveExpr.constructors.map { constructor =>
          // TODO: Check whether `Var.Local(inductiveDefinition.ident.name)` works as expected
          val constructorParams: ArrayBuffer[Param[Term]] = ArrayBuffer.empty
          val signature = Signature(constructorParams, Term.Variable(Var.Local(inductiveDefinition.ident.name)))
          val constructorDefinition: Constructor[Term] = {
            val consIdent: Var.Defined[Term, Constructor] = Var.Defined(constructor.ident.name)
            val indIdent: Var.Defined[Term, Inductive] = inductiveDefinition.ident
            Constructor(consIdent, indIdent, constructorParams)
          }
          constructorDefinition.ident.definition := constructorDefinition
          constructorParams ++= synthParams(constructor.params)
          constructorDefinition
        }
        inductiveDefinition
      }
    }

    case Constructor(_, _, _) => unreachable
    
    case OverloadedFunction(ident, body) => {
      // recursively traverse the states of the overloaded function, synthesis each state
      // and merge branches with the same type arguments
      // TODO: finish this
      ???
    }
  }
  
  def synthParams(paramExprs: Seq[Param[Expr]])(
    implicit env: Environment.Typed[Value]
  ): Seq[Param[Term]] = {
    paramExprs.map { param =>
      Param(param.ident, param.`type`.elaborate(Term.Universe))
    }
  }

  def synthDefinitionRef(definition: Definition[Term])(
    implicit env: Environment.Typed[Value]
  ): Synth = definition match {
    case definition: PureDefinition[Term] => Synth(
      term = definition.params.buildLambda(definition.ident.buildInvoke(Term)).normalize,
      `type` = definition.params.buildPiType(definition.resultType).eval,
    )
    case definition: OverloadedFunction[Term] => ???
  }
}
