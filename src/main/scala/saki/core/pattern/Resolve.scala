package saki.core.pattern

import saki.core.{Expr, Param, ParamList, PristineDefinition, TypeError, Var}
import util.{ScopedMap, SourceSpan}

private[pattern] type ResolvingContext = ScopedMap[String, Var]

extension (ctx: ResolvingContext) {

  def add(variable: Var): ResolvingContext = ctx :+ (variable.name, variable)

  def withVariable[R](variable: Var)(action: ResolvingContext => R): R = {
    action(ctx.add(variable))
  }
}

object ResolvingContext {
  def apply(): ResolvingContext = ScopedMap.empty
  def apply(env: Map[String, Var]): ResolvingContext = ScopedMap(env)
}

extension (self: Expr) {
  def resolve(implicit ctx: ResolvingContext): Expr = {
    given span: SourceSpan = self.span
    self match {

      case Expr.Primitive(value) => Expr.Primitive(value)
      case Expr.PrimitiveType(ty) => Expr.PrimitiveType(ty)
      case Expr.Universe(level) => Expr.Universe(level)

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

extension (self: PristineDefinition) {

  private def resolveParam: (ParamList[Expr], ResolvingContext) = {
    self.params.foldLeft((Seq.empty: ParamList[Expr], ResolvingContext())) {
      case ((params, ctx), param) => {
        val resolvedParam = Param(param.ident, param.`type`.resolve(ctx))
        (params :+ resolvedParam, ctx.add(param.ident))
      }
    }
  }

  private def resolve(implicit ctx: ResolvingContext): PristineDefinition = ???
}
