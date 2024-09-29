package saki.core.pattern

import saki.core.{Expr, Param, ParamList, PristineDefinition, TypeError, Var}
import util.SourceSpan

private[pattern] type ResolvingContext = Map[String, Var]

extension (ctx: ResolvingContext) {

  def add(variable: Var): ResolvingContext = ctx + (variable.name -> variable)

  def withVariable[R](variable: Var)(action: ResolvingContext => R): R = {
    action(ctx.add(variable))
  }
}

object ResolvingContext {
  def apply(): ResolvingContext = Map.empty
  def apply(env: Map[String, Var]): ResolvingContext = env
  def empty: ResolvingContext = Map.empty
}

extension (self: Expr) {
  def resolve(implicit ctx: ResolvingContext): Expr = {
    given span: SourceSpan = self.span
    self match {

      case Expr.Universe() => Expr.Universe()
      case Expr.Primitive(value) => Expr.Primitive(value)
      case Expr.PrimitiveType(ty) => Expr.PrimitiveType(ty)

      case Expr.Resolved(ref) => Expr.Resolved(ref)
      case Expr.Unresolved(name) => ctx.get(name) match {
        case Some(variable) => Expr.Resolved(variable)
        case None => TypeError.error(s"Unresolved variable: $name", span)
      }

      case Expr.Hole(_) => Expr.Hole(ctx.values.flatMap {
        case local: Var.Local => Some(local)
        case _ => None
      }.toSeq)

      case Expr.Pi(param, codomain) => {
        val resolvedParam = Param(param.ident, param.`type`.resolve(ctx))
        Expr.Pi(resolvedParam, ctx.withVariable(resolvedParam.ident) { codomain.resolve })
      }
      case Expr.Sigma(param, codomain) => {
        val resolvedParam = Param(param.ident, param.`type`.resolve(ctx))
        Expr.Sigma(resolvedParam, ctx.withVariable(resolvedParam.ident) { codomain.resolve })
      }

      case Expr.Apply(fn, arg) => Expr.Apply(fn.resolve, arg.resolve)
      case Expr.Elimination(obj, member) => Expr.Elimination(obj.resolve, member)
      case Expr.Lambda(param, body) => Expr.Lambda(param, ctx.withVariable(param) { body.resolve })

      case Expr.Record(fields) => Expr.Record(fields.view.mapValues(_.resolve).toMap)
      case Expr.RecordType(fields) => Expr.RecordType(fields.view.mapValues(_.resolve).toMap)
    }
  }
}

extension (params: ParamList[Expr]) {
  def resolve(ctx: ResolvingContext): (ParamList[Expr], ResolvingContext) = {
    params.foldLeft((Seq.empty: ParamList[Expr], ctx)) {
      case ((params, ctx), param) => {
        val resolvedParam = Param(param.ident, param.`type`.resolve(ctx))
        (params :+ resolvedParam, ctx.add(resolvedParam.ident))
      }
    }
  }
}

extension (self: PristineDefinition) {

  private def resolve: PristineDefinition = {
    var global: ResolvingContext = ResolvingContext.empty

    import PristineDefinition.FunctionBody
    val resolvedDefinition: PristineDefinition = self match {

      case PristineDefinition.Function(ident, params, resultType, body) => {
        val (resolvedParams, ctxWithParam) = params.resolve(global)
        // register the function name to global context
        global += (ident.name -> ident)
        // add the function name to the context for recursive calls
        val bodyCtx = ctxWithParam + (ident.name -> ident)
        val resolvedBody = body match {
          case FunctionBody.Expr(expr) => FunctionBody.Expr(expr.resolve(bodyCtx))
          case FunctionBody.Clauses(clauses) => FunctionBody.Clauses(clauses)
          case FunctionBody.UnresolvedClauses(clauses) => FunctionBody.Clauses(clauses.map(_.resolve(bodyCtx)))
        }
        PristineDefinition.Function(ident, resolvedParams, resultType.resolve(bodyCtx), resolvedBody)
      }

      // TODO: constructors should be resolved only in the context of the inductive type
      // TODO: Remove this match arm (?)
      case PristineDefinition.Constructor(ident, owner, params) => {
        val (resolvedParams, _) = params.resolve(global)
        global += (ident.name -> ident)
        PristineDefinition.Constructor(ident, owner, resolvedParams)
      }

      case PristineDefinition.Inductive(ident, params, constructors) => {
        val (resolvedParams, _) = params.resolve(global)
        global += (ident.name -> ident)
        val resolvedConstructors: Seq[PristineDefinition.Constructor] = constructors.map { constructor =>
          val (resolvedConsParams, _) = constructor.params.resolve(global)
          PristineDefinition.Constructor(constructor.ident, constructor.owner, resolvedConsParams)
        }
        PristineDefinition.Inductive(ident, resolvedParams, resolvedConstructors)
      }
    }
    resolvedDefinition
  }

}
