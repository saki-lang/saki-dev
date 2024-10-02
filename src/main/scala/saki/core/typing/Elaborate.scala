package saki.core.typing

import saki.core.TypeError
import saki.core.syntax.*
import saki.util.unreachable

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

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
      case definitionVar: Var.Defined[? <: Definition] => definitionVar.definition.toOption match {
        case None => ctx.definitions.get(definitionVar) match {
          case Some(definition) => {
            definition.ident.definition :=! definition
            Synth(
              term = definition.params.buildLambda(definition.ident.call).rename(Map.empty),
              `type` = definition.params.buildPiType(definition.resultType),
            )
          }
          case None => TypeError.error(s"Unbound definition: ${definitionVar.name}", expr.span)
        }
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
        case _ => TypeError.mismatch("Π (x : A) -> B", fnType.toString, fnExpr.span)
      }
    }

    case Expr.Match(scrutinees, clauses) => {
      val scrutineesSynth: Seq[Synth] = scrutinees.map(_.synth.normalize)
      val clausesSynth: Seq[(Clause[Term], Type)] = clauses.map { clause =>
        synthClause(clause, scrutineesSynth)
      }
      val clauseBodyTypes: Seq[Type] = clausesSynth.map(_._2)
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
    clause: Clause[Expr], scrutinees: Seq[Synth]
  )(implicit ctx: Context): (Clause[Term], Type) = {
    val patterns = clause.patterns.map(pattern => pattern.map(_.synth.term))
    val map = patterns.zip(scrutinees).foldLeft(Map.empty: Map[Var.Local, Type]) {
      case (subst, (pattern, param)) => subst ++ pattern.matchWith(param.`type`)
    }
    val (body, ty) = ctx.withLocals[Synth](map) { clause.body.synth }.unpack
    (Clause(patterns, body), ty)
  }

  def synthDefinition(definition: PristineDefinition)(implicit ctx: Context): Definition = definition match {

    case PristineDefinition.Function(ident, paramExprs, resultTypeExpr, pristineBody) => {
      val resultType = resultTypeExpr.elaborate(Term.Universe)
      val params = synthParams(paramExprs)
      ctx.withLocals(params.map(param => param.ident -> param.`type`).toMap) {
        implicit ctx => {
          val function: Definition.Function = {
            Definition.Function(ident, Signature(params, resultType), params, resultType)
          }
          function.ident.definition := function
          given Context = ctx.copy(definitions = ctx.definitions.updated(ident, function))
          function.body := pristineBody.elaborate(resultType)
          function
        }
      }
    }

    case inductive: PristineDefinition.Inductive => {
      val params = synthParams(inductive.params)(ctx)
      val constructors = ArrayBuffer.empty[Definition.Constructor]
      val inductiveDefinition: Definition.Inductive = {
        Definition.Inductive(inductive.ident, Signature(params, Term.Universe), params, constructors)
      }
      inductiveDefinition.ident.definition := inductiveDefinition
      // To support recursive inductive types, we need to add the inductive type to the context
      // before synthesizing the constructors
      given Context = ctx.copy(definitions = ctx.definitions.updated(inductive.ident, inductiveDefinition))
      constructors ++= inductive.constructors.map { constructor =>
        // TODO: Check whether `Var.Local(inductiveDefinition.ident.name)` works as expected
        val constructorParams: ArrayBuffer[Param[Term]] = ArrayBuffer.empty
        val signature = Signature(constructorParams, Term.Ref(Var.Local(inductiveDefinition.ident.name)))
        val constructorDefinition: Definition.Constructor = {
          Definition.Constructor(constructor.ident, signature, inductive.ident, constructorParams)
        }
        constructorDefinition.ident.definition := constructorDefinition
        constructorParams ++= synthParams(constructor.params)
        constructorDefinition
      }
      inductiveDefinition
    }

    case PristineDefinition.Constructor(_, _, _) => unreachable
  }

  def synthParams(paramExprs: Seq[Param[Expr]])(implicit ctx: Context): Seq[Param[Term]] = {
    paramExprs.map { param => Param(param.ident, param.`type`.elaborate(Term.Universe)) }
  }
}
