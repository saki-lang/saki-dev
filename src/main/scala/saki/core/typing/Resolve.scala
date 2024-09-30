package saki.core.typing

import saki.core.TypeError
import saki.core.syntax.*
import saki.util.SourceSpan

object Resolve {

  type Context = Map[String, Var]

  object Context {
    def apply(): Resolve.Context = Map.empty
    def apply(env: Map[String, Var]): Resolve.Context = env
    def empty: Resolve.Context = Map.empty
  }

  extension (ctx: Context) {
    def add(variable: Var): Resolve.Context = ctx + (variable.name -> variable)

    def withVariable[R](variable: Var)(action: Resolve.Context => R): R = {
      action(ctx.add(variable))
    }
  }

  def resolveExpr(expr: Expr)(implicit ctx: Resolve.Context): Expr = {
    given span: SourceSpan = expr.span
    expr match {

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

      case Expr.Apply(fn, arg) => Expr.Apply(fn.resolve, Argument(arg.value.resolve, arg.applyMode))
      case Expr.Elimination(obj, member) => Expr.Elimination(obj.resolve, member)
      case Expr.Lambda(param, body, returnType) => Expr.Lambda(
        param = Param(param.ident, param.`type`.map(_.resolve)),
        body = ctx.withVariable(param.ident) { body.resolve },
        returnType = returnType,
      )

      case Expr.Match(scrutinee, clauses) => { // TODO: double check
        Expr.Match(scrutinee.resolve, clauses.map(clause => clause.map(_.resolve)))
      }

      case Expr.Record(fields) => Expr.Record(fields.view.mapValues(_.resolve).toMap)
      case Expr.RecordType(fields) => Expr.RecordType(fields.view.mapValues(_.resolve).toMap)

    }
  }

  extension (params: ParamList[Expr]) {
    def resolve(ctx: Resolve.Context): (ParamList[Expr], Resolve.Context) = {
      params.foldLeft((Seq.empty: ParamList[Expr], ctx)) {
        case ((params, ctx), param) => {
          val resolvedParam = Param(param.ident, param.`type`.resolve(ctx))
          (params :+ resolvedParam, ctx.add(resolvedParam.ident))
        }
      }
    }
  }

  def resolvePristineDefinition(pristineDefinition: PristineDefinition): PristineDefinition = {
    var global: Resolve.Context = Resolve.Context.empty

    import PristineDefinition.FunctionBody
    val resolvedDefinition: PristineDefinition = pristineDefinition match {

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

  def resolveUnresolvedPattern(
    unresolvedPattern: UnresolvedPattern, span: SourceSpan
  )(implicit ctx: Resolve.Context): (Pattern[Term], Resolve.Context) = {
    given SourceSpan = span
    ctx.get(unresolvedPattern.name) match {
      // If the variable is already defined, then it should be a constructor.
      // TODO: literals and records
      case Some(variable: Var.Defined[?]) if unresolvedPattern.patterns.nonEmpty => {
        val (resolvedPatterns, updatedContext) = unresolvedPattern.patterns.foldLeft((List.empty[Pattern[Term]], ctx)) {
          case ((resolvedPatterns, context), pattern) => {
            val (resolved, newCtx) = pattern.resolve(context)
            (resolvedPatterns :+ resolved, newCtx)
          }
        }
        val pattern = Pattern.Cons(
          cons = variable.asInstanceOf[Var.Defined[Definition.Constructor]],
          patterns = resolvedPatterns
        )
        (pattern, updatedContext)
      }

      // Otherwise, it should be a new introduced variable.
      case _ => {
        val variable: Var.Local = Var.Local(unresolvedPattern.name)
        (Pattern.Bind(variable), ctx.updated(unresolvedPattern.name, variable))
      }
    }
  }
}
