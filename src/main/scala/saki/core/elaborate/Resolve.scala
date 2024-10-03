package saki.core.elaborate

import scala.collection.Seq
import saki.core.TypeError
import saki.core.syntax.*
import saki.core.syntax.Pattern.resolve
import saki.core.SourceSpan
import saki.util.LateInit

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

      case Expr.Variable(ref) => Expr.Variable(ref)

      case Expr.Unresolved(name) => ctx.get(name) match {
        case Some(variable) => Expr.Variable(variable)
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

      case Expr.Match(scrutinees, clauses) => { // TODO: double check
        Expr.Match(scrutinees.map(_.resolve), clauses.map { clause =>
          val (resolvedClause, _) = resolveClause(clause)
          resolvedClause
        })
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
  
  extension (definition: Definition[Expr]) {
    def resolve(implicit ctx: Resolve.Context): (Definition[Expr], Resolve.Context) = {
      resolveDefinitionExpr(definition)
    }
  }

  def resolveDefinitionExpr(
    pristineDefinition: Definition[Expr]
  )(implicit ctx: Resolve.Context): (Definition[Expr], Resolve.Context) = {

    var global: Resolve.Context = ctx

    val resolvedDefinition: Definition[Expr] = pristineDefinition match {

      case Function(ident, params, resultType, body) => {
        val (resolvedParams, ctxWithParam) = params.resolve(global)
        // register the function name to global context
        global += (ident.name -> ident)
        // add the function name to the context for recursive calls
        val bodyCtx = ctxWithParam + (ident.name -> ident)
        val resolvedBody = body.get.resolve(bodyCtx)
        val resolvedResultType = resultType.resolve(bodyCtx)
        Function[Expr](ident, resolvedParams, resolvedResultType, LateInit(resolvedBody))
      }

      // TODO: constructors should be resolved only in the context of the inductive type
      // TODO: Remove this match arm (?)
      case Constructor(ident, owner, params) => {
        val (resolvedParams, _) = params.resolve(global)
        global += (ident.name -> ident)
        Constructor(ident, owner, resolvedParams)
      }

      case Inductive(ident, params, constructors) => {
        val (resolvedParams, _) = params.resolve(global)
        global += (ident.name -> ident)
        val resolvedConstructors: Seq[Constructor[Expr]] = constructors.map { constructor =>
          val (resolvedConsParams, _) = constructor.params.resolve(global)
          global += (constructor.ident.name -> constructor.ident)
          Constructor[Expr](constructor.ident, constructor.owner, resolvedConsParams)
        }
        Inductive[Expr](ident, resolvedParams, resolvedConstructors)
      }
    }
    (resolvedDefinition, global)
  }

  def resolvePattern(pattern: Pattern[Expr])(implicit ctx: Context): (Pattern[Expr], Context) = {
    given span: SourceSpan = pattern.span
    pattern match {

      case Pattern.Bind(binding) => {
        (Pattern.Bind(binding), ctx.updated(binding.name, binding))
      }

      case Pattern.Cons(cons, patterns) => {
        val (resolvedPatterns, updatedContext) = patterns.foldLeft((List.empty[Pattern[Expr]], ctx)) {
          case ((resolvedPatterns, context), pattern) => {
            val (resolved, newCtx) = pattern.resolve(context)
            (resolvedPatterns :+ resolved, newCtx)
          }
        }
        (Pattern.Cons(cons, resolvedPatterns), updatedContext)
      }

      case Pattern.Primitive(value) => (Pattern.Primitive(value), ctx)

      case Pattern.Record(fields) => {
        val (resolvedFields, updatedContext) = fields.foldLeft((Map.empty[String, Pattern[Expr]], ctx)) {
          case ((resolvedFields, context), (label, pattern)) => {
            val (resolved, newCtx) = pattern.resolve(context)
            (resolvedFields + (label -> resolved), newCtx)
          }
        }
        (Pattern.Record(resolvedFields.toSeq), updatedContext)
      }

      case Pattern.Typed(pattern, ty) => {
        val (resolvedPattern, updatedContext) = pattern.resolve(ctx)
        (Pattern.Typed(resolvedPattern, ty.resolve(updatedContext)), updatedContext)
      }
    }
  }

  def resolveClause(clause: Clause[Expr])(implicit ctx: Resolve.Context): (Clause[Expr], Context) = {
    val (resolvedPatterns, newCtx) = clause.patterns.foldLeft((List.empty[Pattern[Expr]], ctx)) {
      case ((resolvedPatterns, context), pattern) => {
        val (resolved, newCtx) = pattern.resolve(context)
        (resolvedPatterns :+ resolved, newCtx)
      }
    }
    val body = clause.body.resolve(newCtx)
    (Clause(resolvedPatterns, body), newCtx)
  }
}
