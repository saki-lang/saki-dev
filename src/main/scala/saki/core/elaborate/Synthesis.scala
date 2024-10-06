package saki.core.elaborate

import saki.core.syntax.given
import saki.core.syntax.buildInvoke
import saki.core.{Entity, TypeError}
import saki.core.syntax.*
import saki.core.Param
import saki.core.context.{CurrentDefinitionContext, Environment}
import saki.util.unreachable

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

private[core] object Synthesis {

  def elaborate(expr: Expr, expectedType: Term)(
    implicit env: Environment.Untyped[Term]
  ): Term = expr match {
    case Expr.Lambda(lambdaParam, body, returnType) => {
      // Check that expectedType is a Pi type
      expectedType.normalize(Environment.Typed(env)) match {
        case piType: Term.Pi => Term.Lambda(
          param = lambdaParam.map(_.elaborate(piType.param.`type`)),
          body = env.withLocal(lambdaParam.ident, piType.param.`type`) {
            // Check that the body type is the same as the codomain of the Pi type
            returnType.map(_.synth.term unify piType.codomain) match {
              case Some(false) => TypeError.mismatch(piType.codomain.toString, returnType.get.toString, expr.span)
              case _ => body.elaborate(piType.codomain)
            }
          }
        )
        case ty => TypeError.mismatch("Π (x : A) -> B", ty.toString, expr.span)
      }
    }
    case Expr.Hole(_) =>  TypeError.error("Holes are not allowed in this context", expr.span)
    case _ => {
      val synthResult = expr.synth
      if !(synthResult.`type` unify expectedType.normalize(Environment.Typed(env))) then {
        TypeError.mismatch(expectedType.toString, synthResult.`type`.toString, expr.span)
      }
      synthResult.term
    }
  }

  case class Synth(term: Term, `type`: Term) {

    def unpack: (Term, Term) = (term, `type`)

  }

  def synth(expr: Expr)(implicit env: Environment.Untyped[Term]): Synth = expr match {

    case Expr.Universe() => Synth(Term.Universe, Term.Universe)

    case Expr.Primitive(value) => Synth(Term.Primitive(value), Term.PrimitiveType(value.ty))

    case Expr.PrimitiveType(ty) => Synth(Term.PrimitiveType(ty), Term.Universe)

    case Expr.Variable(ref) => ref match {
      case definitionVar: Var.Defined[Term, ?] => definitionVar.definition.toOption match {
        case None => env.getDefinition[Term](definitionVar) match {
          case Some(definition) => {
            definition.ident.definition :=! definition
            Synth(
              term = definition.params.buildLambda(definition.ident.buildInvoke(Term)).normalize(Environment.Typed(env)),
              `type` = definition.params.buildPiType(definition.resultType),
            )
          }
          case None => TypeError.error(s"Unbound definition: ${definitionVar.name}", expr.span)
        }
        case Some(definition: Definition[Term]) => Synth(
          // TODO: should we remove `rename` here? (does it make sense?)
          term = definition.params.buildLambda(definition.ident.buildInvoke(Term)).normalize(Environment.Typed(env)),
          `type` = definition.params.buildPiType(definition.resultType),
        )
      }
      case variable: Var.Local => env.locals.get(variable) match {
        case Some(ty) => Synth(Term.Variable(variable), ty)
        case None => TypeError.error(s"Unbound variable: ${variable.name}", expr.span)
      }
    }

    case Expr.Elimination(obj, member) => obj.synth.unpack match {
      // This is a project operation
      // `obj.field`
      // TODO: maybe we don't need to normalize here?
      case (term, recordType: Term.RecordType) => term match {
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
        Synth(Term.FunctionInvoke(method.ident, Seq(term)), method.resultType)
      }
    }

    case Expr.Apply(fnExpr, argExpr) => {
      val (fn, fnType) = fnExpr.synth.unpack
      fnType match {
        case Term.Pi(param, codomain) => {
          env.withLocal(param.ident, param.`type`) { env =>
            val (arg, argType) = argExpr.value.synth(env).unpack
            if !(argType unify param.`type`) then {
              TypeError.mismatch(param.`type`.toString, argType.toString, argExpr.value.span)
            }
            Synth(fn.apply(arg), codomain.subst(param.ident, arg))
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
      if !clauseBodyTypes.tail.forall(_ unify clauseBodyTypes.head) then {
        TypeError.error("Clauses have different types", expr.span)
      }
      Synth(
        term = Term.Match(scrutineesSynth.map(_.term), clausesSynth.map(_._1)),
        `type` = clauseBodyTypes.head
      )
    }

    case Expr.Pi(param, result) => synthDependentType(param, result)

    case Expr.Sigma(param, result) => synthDependentType(param, result)

    case _ => TypeError.error("Failed to synthesis expression", expr.span)
    
  }

  private def synthDependentType(param: Param[Expr], result: Expr)(
    implicit env: Environment.Untyped[Term]
  ): Synth = {
    val (paramType, _) = param.`type`.synth.unpack
    val (codomain, _) = env.withLocal(param.ident, paramType) { result.synth(_).unpack }
    Synth(
      term = Term.Sigma(Param(param.ident, paramType), codomain),
      `type` = Term.Universe
    )
  }

  def synthClause(clause: Clause[Expr], scrutinees: Seq[Synth])(
    implicit env: Environment.Untyped[Term]
  ): (Clause[Term], Term) = {
    val patterns = clause.patterns.map(pattern => pattern.map(_.synth.term))
    val map = patterns.zip(scrutinees).foldLeft(Map.empty: Map[Var.Local, Term]) {
      case (subst, (pattern, param)) => subst ++ pattern.buildMatch(param.`type`)
    }
    val (body, ty) = env.withLocals[Synth](map) { clause.body.synth }.unpack
    (Clause(patterns, body), ty)
  }

  extension (definition: Definition[Expr]) {
    def synth(implicit env: Environment.Untyped[Term]): Definition[Term] = synthDefinition(definition)
  }
  
  def synthDefinition(definition: Definition[Expr])(
    implicit env: Environment.Untyped[Term]
  ): Definition[Term] = env.withCurrentDefinition(definition.ident) { env =>
    
    definition match {

      case Function(ident, paramExprs, resultTypeExpr, isNeutral, pristineBody) => {
        // TODO: Compute `isNeutral` using a reference graph
        val resultType = resultTypeExpr.elaborate(Term.Universe)
        val params = synthParams(paramExprs)
        env.withLocals(params.map(param => param.ident -> param.`type`).toMap) {
          implicit env => {
            val function: Function[Term] = Function[Term](Var.Defined(ident.name), params, resultType, isNeutral)
            function.ident.definition := function
            given Environment.Untyped[Term] = env.copy(definitions = env.definitions.updated(ident, function))
            function.body := pristineBody.get.elaborate(resultType)
            function
          }
        }
      }

      case inductiveExpr: Inductive[Expr] => {
        val params = synthParams(inductiveExpr.params)(env)
        val constructors = ArrayBuffer.empty[Constructor[Term]]
        val inductiveDefinition: Inductive[Term] = Inductive(Var.Defined(inductiveExpr.ident.name), params, constructors)
        inductiveDefinition.ident.definition := inductiveDefinition
        // To support recursive inductive types, we need to add the inductive type to the context
        // before synthesizing the constructors
        given Environment.Untyped[Term] = {
          env.copy(definitions = env.definitions.updated(inductiveDefinition.ident, inductiveDefinition))
        }
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

      case Constructor(_, _, _) => unreachable
    }
  }

  def synthParams(paramExprs: Seq[Param[Expr]])(
    implicit env: Environment.Untyped[Term]
  ): Seq[Param[Term]] = {
    paramExprs.map { param => Param(param.ident, param.`type`.elaborate(Term.Universe)) }
  }
}
