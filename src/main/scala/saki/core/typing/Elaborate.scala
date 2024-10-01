package saki.core.typing

import scala.collection.Seq
import saki.core.TypeError
import saki.core.syntax.*
import saki.core.syntax.matchWith
import saki.util.unreachable

private[core] object Elaborate {

  case class Context(
    definitions: Map[Var.Defined[? <: Definition], Definition],
    locals: Map[Var.Local, Type],
  ) {
    private[Elaborate] def withLocal[R](local: Var.Local, `type`: Type)(action: Context => R): R = {
      action(copy(locals = locals.updated(local, `type`)))
    }

    private[Elaborate] def withLocals[R](locals: Map[Var.Local, Type])(action: Context => R): R = {
      action(copy(locals = this.locals ++ locals))
    }

    def getDefinition(name: String): Option[Definition] = definitions.collectFirst {
      case (varDef, definition) if varDef.name == name => definition
    }
  }
  
  object Context {
    def empty: Context = Context(Map.empty, Map.empty)
  }

  def elaborate(expr: Expr, expectedType: Type)(implicit ctx: Context): Term = expr match {
    case Expr.Lambda(lambdaParam, body, returnType) => {
      // Check that expectedType is a Pi type
      expectedType.normalize(Map.empty) match {
        case piType: Term.Pi => Term.Lambda(
          param = lambdaParam.ident,
          body = ctx.withLocal(lambdaParam.ident, piType.param.`type`) {
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
      if !(synthResult.`type` unify expectedType) then {
        TypeError.mismatch(expectedType.toString, synthResult.`type`.toString, expr.span)
      }
      synthResult.term
    }
  }

  case class Synth(term: Term, `type`: Type) {
    def unpack: (Term, Type) = (term, `type`)
    def normalize: Synth = copy(term = term.normalize(Map.empty), `type` = `type`.normalize(Map.empty))
  }

  def synth(expr: Expr)(implicit ctx: Context): Synth = (expr match {

    case Expr.Universe() => Synth(Term.Universe, Term.Universe)

    case Expr.Primitive(value) => Synth(Term.Primitive(value), Term.PrimitiveType(value.ty))

    case Expr.PrimitiveType(ty) => Synth(Term.PrimitiveType(ty), Term.Universe)

    case Expr.Resolved(ref) => ref match {
      case definitionVar: Var.Defined[? <: Definition] => definitionVar.definition match {
        case None => Synth(
          term = definitionVar.signature.params.buildLambda(definitionVar.call),
          `type` = definitionVar.signature.params.buildPiType(definitionVar.signature.resultType)
        )
        case Some(definition: Definition) => Synth(
          // TODO: should we remove `rename` here? (does it make sense?)
          term = definition.params.buildLambda(definition.ident.call).rename(Map.empty),
          `type` = definition.params.buildPiType(definition.resultType),
        )
      }
      case variable: Var.Local => ctx.locals.get(variable) match {
        case Some(ty) => Synth(Term.Ref(variable), ty)
        case None => TypeError.error(s"Unbound variable: ${variable.name}", expr.span)
      }
    }

    case Expr.Elimination(obj, member) => obj.synth.normalize.unpack match {
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
        val method: Definition.Function = ctx.getDefinition(member) match {
          case Some(definition: Definition.Function) => definition
          case _ => TypeError.error(s"Method not found: $member", expr.span)
        }
        Synth(Term.FunctionCall(method.ident, Seq(term)), method.resultType)
      }
    }

    case Expr.Apply(fnExpr, argExpr) => {
      val (fn, fnType) = fnExpr.synth.normalize.unpack
      fnType match {
        case Term.Pi(param, codomain) => {
          ctx.withLocal(param.ident, param.`type`) { ctx =>
            val (arg, argType) = argExpr.value.synth(ctx).normalize.unpack
            if !(argType unify param.`type`) then {
              TypeError.mismatch(param.`type`.toString, argType.toString, argExpr.value.span)
            }
            Synth(fn.apply(arg), codomain.subst(param.ident, arg))
          }
        }
        case _ => TypeError.error("Expected a function type", fnExpr.span)
      }
    }

    case Expr.Match(scrutinee, clauses) => {
      val (scrutineeTerm, scrutineeType) = scrutinee.synth.normalize.unpack
      val clausesSynth = clauses.map { clause =>
        val patterns = clause.patterns.map(pattern => pattern.map(_.synth.term))
        (patterns, clause.body.synth)
      }
      if !clausesSynth.map(_._2.`type`).forall(_ unify scrutineeType) then {
        TypeError.error("Clauses have different types", expr.span)
      }
      Synth(
        term = Term.Match(
          scrutinee = scrutineeTerm,
          clauses = clausesSynth.map {
            case (patterns, bodySynth) => Clause(patterns, bodySynth.term)
          }
        ),
        `type` = scrutineeType
      )
    }

    case Expr.Pi(param, result) => synthDependentType(param, result)

    case Expr.Sigma(param, result) => synthDependentType(param, result)

    case _ => TypeError.error("Failed to synthesis expression", expr.span)
    
  }).normalize

  private def synthDependentType(param: Param[Expr], result: Expr)(implicit ctx: Context): Synth = {
    val (paramType, _) = param.`type`.synth.unpack
    val (codomain, _) = ctx.withLocal(param.ident, paramType) { result.synth(_).unpack }
    Synth(
      term = Term.Sigma(Param(param.ident, paramType), codomain),
      `type` = Term.Universe
    )
  }

  def synthClause(
    clause: Clause[Expr], params: Seq[Param[Term]], resultType: Type
  )(implicit ctx: Context): Clause[Term] = {
    val patterns = clause.patterns.map(pattern => pattern.map(_.synth.term))
    val map = patterns.zip(params).foldLeft(Map.empty: Map[Var.Local, Type]) {
      case (subst, (pattern, param)) => subst ++ pattern.matchWith(param.`type`)
    }
    val body = ctx.withLocals(map) {
      elaborate(clause.body, resultType)
    }
    Clause(patterns, body)
  }

  def synthDefinition(definition: PristineDefinition)(implicit ctx: Context): Definition = definition match {

    case PristineDefinition.Function(ident, paramExprs, resultTypeExpr, pristineBody) => {
      val resultType = resultTypeExpr.elaborate(Term.Universe)
      val params = synthParams(paramExprs)
      ctx.withLocals(params.map(param => param.ident -> param.`type`).toMap) {
        implicit ctx => {
          import PristineDefinition.FunctionBody
          val signature = Signature(params, resultType)
          val body: Either[Term, Seq[Clause[Term]]] = pristineBody match {
            case FunctionBody.Expr(expr) => Left(expr.elaborate(resultType))
            case FunctionBody.Clauses(clauses) => {
              Right(clauses.map(clause => synthClause(clause, params, resultType)))
              // TODO: classify clauses
            }
            case FunctionBody.UnresolvedClauses(_) => unreachable
          }
          Definition.Function(ident, signature, params, resultType, body)
        }
      }
    }

    case inductive: PristineDefinition.Inductive => {
      val params = synthParams(inductive.params)
      val constructors: Seq[Definition.Constructor] = inductive.constructors.map { constructor =>
        // FIXME: the return type of a constructor should be the inductive type
        val signature = Signature(params, Term.Universe)
        Definition.Constructor(constructor.ident, signature, inductive.ident, params)
      }
      Definition.Inductive(inductive.ident, Signature(params, Term.Universe), params, constructors)
    }

    case PristineDefinition.Constructor(_, _, _) => unreachable
  }

  def synthParams(paramExprs: Seq[Param[Expr]])(implicit ctx: Context): Seq[Param[Term]] = {
    paramExprs.map { param => Param(param.ident, param.`type`.elaborate(Term.Universe)) }
  }
}
