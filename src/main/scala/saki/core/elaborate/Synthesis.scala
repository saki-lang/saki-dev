package saki.core.elaborate

import saki.core.syntax.given
import saki.core.{Entity, TypeError}
import saki.core.syntax.*
import saki.core.Param
import saki.util.unreachable

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

private[core] object Synthesis {

  case class Context(
    definitions: Map[Var.Defined[?, ?], Definition[Term]],
    locals: Map[Var.Local, Term],
  ) {
    private[Synthesis] def withLocal[R](local: Var.Local, `type`: Term)(action: Context => R): R = {
      action(copy(locals = locals.updated(local, `type`)))
    }

    private[Synthesis] def withLocals[R](locals: Map[Var.Local, Term])(action: Context => R): R = {
      action(copy(locals = this.locals ++ locals))
    }

    def getDefinition(name: String): Option[Definition[Term]] = definitions.collectFirst {
      case (varDef, definition) if varDef.name == name => definition
    }
  }
  
  object Context {
    def empty: Context = Context(Map.empty, Map.empty)
  }

  def elaborate(expr: Expr, expectedType: Term)(implicit ctx: Context): Term = expr match {
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
      if !(synthResult.`type` unify expectedType.normalize(Map.empty)) then {
        TypeError.mismatch(expectedType.toString, synthResult.`type`.toString, expr.span)
      }
      synthResult.term
    }
  }

  case class Synth(term: Term, `type`: Term) {
    def unpack: (Term, Term) = (term, `type`)
    def normalize: Synth = copy(term = term.normalize(Map.empty), `type` = `type`.normalize(Map.empty))
    def normalizeType: Synth = copy(term = term, `type` = `type`.normalize(Map.empty))
  }

  def synth(expr: Expr)(implicit ctx: Context): Synth = (expr match {

    case Expr.Universe() => Synth(Term.Universe, Term.Universe)

    case Expr.Primitive(value) => Synth(Term.Primitive(value), Term.PrimitiveType(value.ty))

    case Expr.PrimitiveType(ty) => Synth(Term.PrimitiveType(ty), Term.Universe)

    case Expr.Variable(ref) => ref match {
      case definitionVar: Var.Defined[Term, ?] => definitionVar.definition.toOption match {
        case None => ctx.definitions.get(definitionVar) match {
          case Some(definition) => {
            definition.ident.definition :=! definition
            Synth(
              term = definition.params.buildLambda(definition.ident.call(Term)).rename(Map.empty),
              `type` = definition.params.buildPiType(definition.resultType),
            )
          }
          case None => TypeError.error(s"Unbound definition: ${definitionVar.name}", expr.span)
        }
        case Some(definition: Definition[Term]) => Synth(
          // TODO: should we remove `rename` here? (does it make sense?)
          term = definition.params.buildLambda(definition.ident.call(Term)).rename(Map.empty),
          `type` = definition.params.buildPiType(definition.resultType),
        )
      }
      case variable: Var.Local => ctx.locals.get(variable) match {
        case Some(ty) => Synth(Term.Variable(variable), ty)
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
        val method: Function[Term] = ctx.getDefinition(member) match {
          case Some(definition: Function[Term]) => definition
          case _ => TypeError.error(s"Method not found: $member", expr.span)
        }
        Synth(Term.FunctionInvoke(method.ident, Seq(term)), method.resultType)
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
    
  }).normalizeType

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
  )(implicit ctx: Context): (Clause[Term], Term) = {
    val patterns = clause.patterns.map(pattern => pattern.map(_.synth.term))
    val map = patterns.zip(scrutinees).foldLeft(Map.empty: Map[Var.Local, Term]) {
      case (subst, (pattern, param)) => subst ++ pattern.matchWith(param.`type`)
    }
    val (body, ty) = ctx.withLocals[Synth](map) { clause.body.synth }.unpack
    (Clause(patterns, body), ty)
  }

  extension (definition: Definition[Expr]) {
    def synth(implicit ctx: Context): Definition[Term] = synthDefinition(definition)
  }
  
  def synthDefinition(definition: Definition[Expr])(implicit ctx: Context): Definition[Term] = definition match {

    case Function(ident, paramExprs, resultTypeExpr, isNeutral, pristineBody) => {
      // TODO: Compute `isNeutral` using a reference graph
      val resultType = resultTypeExpr.elaborate(Term.Universe)
      val params = synthParams(paramExprs)
      ctx.withLocals(params.map(param => param.ident -> param.`type`).toMap) {
        implicit ctx => {
          val function: Function[Term] = Function[Term](Var.Defined(ident.name), params, resultType, isNeutral)
          function.ident.definition := function
          given Context = ctx.copy(definitions = ctx.definitions.updated(ident, function))
          function.body := pristineBody.get.elaborate(resultType)
          function
        }
      }
    }

    case inductiveExpr: Inductive[Expr] => {
      val params = synthParams(inductiveExpr.params)(ctx)
      val constructors = ArrayBuffer.empty[Constructor[Term]]
      val inductiveDefinition: Inductive[Term] = Inductive(Var.Defined(inductiveExpr.ident.name), params, constructors)
      inductiveDefinition.ident.definition := inductiveDefinition
      // To support recursive inductive types, we need to add the inductive type to the context
      // before synthesizing the constructors
      given Context = ctx.copy(definitions = ctx.definitions.updated(inductiveDefinition.ident, inductiveDefinition))
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

  def synthParams(paramExprs: Seq[Param[Expr]])(implicit ctx: Context): Seq[Param[Term]] = {
    paramExprs.map { param => Param(param.ident, param.`type`.elaborate(Term.Universe)) }
  }
}
