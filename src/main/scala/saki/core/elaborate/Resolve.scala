package saki.core.elaborate

import scala.collection.Seq
import saki.core.TypeError
import saki.core.syntax.*
import saki.core.syntax.Pattern.resolve
import saki.core.SourceSpan
import saki.util.{Graph, LateInit}

import scala.annotation.targetName

object Resolve {

  case class Context(
    variableMap: Map[String, Var] = Map.empty,
    dependencyGraph: Graph[Var.Defined[?, ?]] = Graph.directed,
    currentDefinition: Option[Var.Defined[Expr, ?]] = None,
  ) {
    def variables: Seq[Var] = variableMap.values.toSeq


    @targetName("add")
    def +(variable: Var): Context = copy(variableMap = variableMap + (variable.name -> variable))

    def ref(variable: Var.Defined[?, ?]): Context = {
      val updatedDependencies = currentDefinition match {
        case Some(definition) => dependencyGraph.addEdge(definition, variable)
        case None => dependencyGraph
      }
      copy(variableMap = variableMap + (variable.name -> variable), dependencyGraph = updatedDependencies)
    }

    def withVariable[R](variable: Var)(action: Context => R): R = {
      action(this + variable)
    }

    def withDefinition[R](definition: Var.Defined[Expr, ?])(action: Context => R): R = {
      if currentDefinition.isDefined then {
        throw new IllegalStateException("Cannot nest definitions")
      } else {
        action(copy(
          currentDefinition = Some(definition),
          variableMap = variableMap + (definition.name -> definition),
          dependencyGraph = dependencyGraph.addVertex(definition),
        ))
      }
    }

    def get(name: String): Option[Var] = variableMap.get(name)
  }

  object Context {
    // def apply(): Resolve.Context = empty
    def apply(env: Map[String, Var]): Resolve.Context = new Context(env)
    def empty: Resolve.Context = Context()
  }

  def resolveExpr(expr: Expr)(implicit ctx: Resolve.Context): (Expr, Resolve.Context) = {
    given span: SourceSpan = expr.span
    given Resolve.Context = ctx
    expr match {

      case Expr.Universe() => (Expr.Universe(), ctx)
      case Expr.Primitive(value) => (Expr.Primitive(value), ctx)
      case Expr.PrimitiveType(ty) => (Expr.PrimitiveType(ty), ctx)

      case Expr.Variable(variable) => variable match {
        case defined: Var.Defined[?, ?] => (Expr.Variable(defined), ctx.ref(defined))
        case local: Var.Local => (Expr.Variable(local), ctx)
      }

      case Expr.Unresolved(name) => ctx.get(name) match {
        case Some(variable) => (Expr.Variable(variable), ctx)
        case None => TypeError.error(s"Unresolved variable: $name", span)
      }

      case Expr.Hole(_) => {
        val resolved = Expr.Hole(ctx.variables.flatMap {
          case local: Var.Local => Some(local)
          case _ => None
        }.toSeq)
        (resolved, ctx)
      }

      case Expr.Pi(param, codomain) => {
        val (resolvedParamType, ctxParam) = param.`type`.resolve(ctx)
        val resolvedParam = Param(param.ident, resolvedParamType)
        val (resolvedResult, ctxResult) = ctxParam.withVariable(resolvedParam.ident) { codomain.resolve }
        (Expr.Pi(resolvedParam, resolvedResult), ctxResult)
      }

      case Expr.Sigma(param, codomain) => {
        val (resolvedParamType, ctxParam) = param.`type`.resolve(ctx)
        val resolvedParam = Param(param.ident, resolvedParamType)
        val (resolvedResult, ctxResult) = ctxParam.withVariable(resolvedParam.ident) { codomain.resolve }
        (Expr.Sigma(resolvedParam, resolvedResult), ctxResult)
      }

      case Expr.Apply(fn, arg) => {
        val (resolvedFn, fnCtx) = fn.resolve
        val (resolvedArg, argCtx) = arg.value.resolve(fnCtx)
        val resolved = Expr.Apply(resolvedFn, Argument(resolvedArg, arg.applyMode))
        (resolved, argCtx)
      }

      case Expr.Elimination(obj, member) => {
        val (resolvedObj, ctx) = obj.resolve
        val resolved = Expr.Elimination(resolvedObj, member)
        (resolved, ctx)
      }

      case Expr.Lambda(param, body, returnType) => {
        val (resolvedParamType, ctxParam) = param.`type` match {
          case Some(ty) =>
            val (resolved, ctxParam) = ty.resolve(ctx)
            (Some(resolved), ctxParam)
          case None => (None, ctx)
        }
        val (resolvedBody, ctxBody) = ctxParam.withVariable(param.ident) { body.resolve }
        val resolved = Expr.Lambda(
          param = Param(param.ident, resolvedParamType),
          body = resolvedBody,
          returnType = returnType,
        )
        (resolved, ctxBody)
      }

      case Expr.Match(scrutinees, clauses) => { // TODO: double check
        val (resolvedScrutinees, scrutineesCtx) = scrutinees.foldLeft((List.empty[Expr], ctx)) {
          case ((resolvedScrutinees, ctx), scrutinee) => {
            val (resolved, newCtx) = scrutinee.resolve(ctx)
            (resolvedScrutinees :+ resolved, newCtx)
          }
        }
        val (resolvedClauses, clausesCtx) = clauses.foldLeft((List.empty[Clause[Expr]], scrutineesCtx)) {
          case ((resolvedClauses, ctx), clause) => {
            val (resolved, newCtx) = resolveClause(clause)(ctx)
            (resolvedClauses :+ resolved, newCtx)
          }
        }
        (Expr.Match(resolvedScrutinees, resolvedClauses), clausesCtx)
      }

      case Expr.Record(fields) => {
        val (resolvedFields, fieldsCtx) = fields.foldLeft((Map.empty[String, Expr], ctx)) {
          case ((resolvedFields, ctx), (label, expr)) => {
            val (resolved, newCtx) = expr.resolve(ctx)
            (resolvedFields + (label -> resolved), newCtx)
          }
        }
        (Expr.Record(resolvedFields), fieldsCtx)
      }

      case Expr.RecordType(fields) => {
        val (resolvedFields, fieldsCtx) = fields.foldLeft((Map.empty[String, Expr], ctx)) {
          case ((resolvedFields, ctx), (label, expr)) => {
            val (resolved, newCtx) = expr.resolve(ctx)
            (resolvedFields + (label -> resolved), newCtx)
          }
        }
        (Expr.RecordType(resolvedFields), fieldsCtx)
      }

    }
  }

  extension (params: ParamList[Expr]) {
    def resolve(ctx: Resolve.Context): (ParamList[Expr], Resolve.Context) = {
      params.foldLeft((Seq.empty: ParamList[Expr], ctx)) {
        case ((params, ctx), param) => {
          val (resolvedParamType, paramCtx) = param.`type`.resolve(ctx)
          val resolvedParam = Param(param.ident, resolvedParamType)
          (params :+ resolvedParam, paramCtx + resolvedParam.ident)
        }
      }
    }
  }
  
  extension (definition: Definition[Expr]) {
    def resolve(implicit ctx: Resolve.Context): (Definition[Expr], Resolve.Context) = {
      resolveDefinition(definition)
    }
  }

  def resolveDefinition(
    pristineDefinition: Definition[Expr]
  )(implicit ctx: Resolve.Context): (Definition[Expr], Resolve.Context) = {

    // TODO: support mutual recursion (need another pass to pre-build declarations)

    var global: Resolve.Context = ctx

    val resolvedDefinition: Definition[Expr] = pristineDefinition match {

      case Function(ident, params, resultType, isNeutral, body) => {
        val (resolvedParams, ctxWithParam) = params.resolve(global)
        // register the function name to global context
        global += ident
        // add the function name to the context for recursive calls
        ctxWithParam.withDefinition(ident) { implicit ctx =>
          val (resolvedBody, bodyCtx) = body.get.resolve(ctx)
          val (resolvedResultType, resultTypeCtx) = resultType.resolve(bodyCtx)
          Function[Expr](ident, resolvedParams, resolvedResultType, isNeutral, LateInit(resolvedBody))
        }
      }

      // TODO: constructors should be resolved only in the context of inductive types
      // TODO: Remove this match arm (?)
      case Constructor(ident, owner, params) => {
        val (resolvedParams, _) = params.resolve(global)
        global += ident
        Constructor(ident, owner, resolvedParams)
      }

      case Inductive(ident, params, constructors) => {
        val (resolvedParams, _) = params.resolve(global)
        global += ident
        val resolvedConstructors: Seq[Constructor[Expr]] = constructors.map { constructor =>
          val (resolvedConsParams, _) = constructor.params.resolve(global)
          global += constructor.ident
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

      case Pattern.Bind(binding) => (Pattern.Bind(binding), ctx + binding)

      case Pattern.Cons(cons, patterns) => {
        val (resolvedPatterns, updatedContext) = patterns.foldLeft((List.empty[Pattern[Expr]], ctx)) {
          case ((resolvedPatterns, context), pattern) => {
            val (resolved, newCtx) = pattern.resolve(context)
            (resolvedPatterns :+ resolved, newCtx)
          }
        }
        (Pattern.Cons(cons, resolvedPatterns), updatedContext.ref(cons))
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
        val (resolvedType, tyCtx) = ty.resolve(updatedContext)
        (Pattern.Typed(resolvedPattern, resolvedType), tyCtx)
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
    val (resolvedPattern, bodyCtx) = clause.body.resolve(newCtx)
    (Clause(resolvedPatterns, resolvedPattern), bodyCtx)
  }
}
