package saki.core.elaborate

import saki.core.context.DefinitionContext
import saki.core.syntax.*
import saki.util.{unreachable, Graph, LateInit, SourceSpan}
import saki.error.CoreErrorKind.*

import scala.annotation.targetName
import scala.collection.Seq

object Resolve {

  case class Context(
    variableMap: Map[String, Var] = Map.empty,
    dependencyGraph: Graph[String] = Graph.directed,
    currentDefinition: Option[Var.Defined[Expr, ?]] = None,
  ) {
    
    def variables: Seq[Var] = variableMap.values.toSeq
    
    @targetName("add")
    def +(variable: Var): Context = copy(variableMap = variableMap + (variable.name -> variable))

    @targetName("remove")
    def -(variable: Var): Context = copy(variableMap = variableMap - variable.name)

    def ref(variable: Var.Defined[?, ?]): Context = {
      val updatedDependencies = currentDefinition match {
        case Some(definition) => dependencyGraph.addEdge(definition.name, variable.name)
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
          dependencyGraph = dependencyGraph.addVertex(definition.name),
        ))
      }
    }

    def get(name: String): Option[Var] = variableMap.get(name)
  }

  object Context {

    def apply(implicit env: DefinitionContext): Resolve.Context = {
      val variables = env.definitions.keys
      Context(variableMap = variables.map(variable => variable.name -> variable).toMap)
    }

    def apply(symbols: Seq[Var]): Resolve.Context = {
      new Context(symbols.map(symbol => symbol.name -> symbol).toMap)
    }

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

      case Expr.TypeOf(value) => {
        val (resolved, ctx) = value.resolve
        (Expr.TypeOf(resolved), ctx)
      }

      case Expr.Unresolved(name) => ctx.get(name) match {
        case Some(variable) => Expr.Variable(variable).resolve
        case None => UnresolvedReference.raise(s"Unresolved variable: $name")
      }

      case Expr.Union(types) => {
        val (resolvedTypes, typesCtx) = types.foldLeft((List.empty[Expr], ctx)) {
          case ((resolvedTypes, ctx), ty) => {
            val (resolved, newCtx) = ty.resolve(ctx)
            (resolvedTypes :+ resolved, newCtx)
          }
        }
        (Expr.Union(resolvedTypes), typesCtx)
      }

      case Expr.Intersection(types) => {
        val (resolvedTypes, typesCtx) = types.foldLeft((List.empty[Expr], ctx)) {
          case ((resolvedTypes, ctx), ty) => {
            val (resolved, newCtx) = ty.resolve(ctx)
            (resolvedTypes :+ resolved, newCtx)
          }
        }
        (Expr.Intersection(resolvedTypes), typesCtx)
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
        val (resolvedObj, context) = obj.resolve
        // If the member is a definition, indicating that this is a method call.
        // Thus, we need to update the dependency graph.
        val updatedContext = context.variableMap.get(member) match {
          case Some(definition: Var.Defined[Term@unchecked, ?]) => context.ref(definition)
          case _ => context
        }
        val resolved = Expr.Elimination(resolvedObj, member)
        (resolved, updatedContext)
      }

      case Expr.Constructor(inductive, constructor) => {
        val (resolvedInductive, inductiveCtx) = inductive.resolve
        (Expr.Constructor(resolvedInductive, constructor), inductiveCtx)
      }

      case Expr.Lambda(param, body, returnType) => {
        val (resolvedParamType, ctxParam) = param.`type`.resolve
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

      case Expr.Record(fields, expectedType) => {
        val (resolvedFields, fieldsCtx) = fields.foldLeft((Map.empty[String, Expr], ctx)) {
          case ((resolvedFields, ctx), (label, expr)) => {
            val (resolved, newCtx) = expr.resolve(ctx)
            (resolvedFields + (label -> resolved), newCtx)
          }
        }
        (Expr.Record(resolvedFields, expectedType), fieldsCtx)
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
  
  extension (definition: Definition[Expr]) {

    def resolve(implicit ctx: Resolve.Context): (Definition[Expr], Resolve.Context) = {
      resolveDefinition(definition)
    }
  }

  def resolveDefinition(
    definition: Definition[Expr]
  )(implicit ctx: Resolve.Context): (Definition[Expr], Resolve.Context) = {

    var global: Resolve.Context = ctx

    def resolveParams(
      params: ParamList[Expr], ctx: Resolve.Context = global
    ): (ParamList[Expr], Resolve.Context) = {
      params.foldLeft((Seq.empty: ParamList[Expr], ctx)) {
        case ((resolvedParams, ctx), param) => {
          val (resolvedParamType, paramCtx) = param.`type`.resolve(ctx)
          val resolvedParam = Param(param.ident, resolvedParamType)
          global = global.copy(dependencyGraph = global.dependencyGraph merge paramCtx.dependencyGraph)
          (resolvedParams :+ resolvedParam, paramCtx + resolvedParam.ident)
        }
      }
    }

    val resolvedDefinition: Definition[Expr] = definition match {

      case DefinedFunction(ident, params, resultType, _, body) => {
        // register the function name to global context
        global += ident
        // add the function name to the context for recursive calls
        global.withDefinition(ident) { implicit ctx =>
          // Return type or param type cannot call itself,
          //  thus we remove it from the context and add it back after
          //  finished resolving the params and return type
          val (resolvedParams, ctxWithParam) = resolveParams(params, ctx - ident)
          val (resolvedResultType, signatureCtx) = resultType.resolve(ctxWithParam)
          // Resolve the body of the function, add the self identifier back to the context
          val (resolvedBody, bodyCtx) = body.get.resolve(signatureCtx + ident)
          val dependencies: Set[Var.Defined[Expr, Function]] = bodyCtx.dependencyGraph.reachableSet(ident.name).map(Var.Defined(_))
          // update the global context with the dependency graph
          global = global.copy(dependencyGraph = bodyCtx.dependencyGraph)
          DefinedFunction[Expr](Var.Defined(ident.name), resolvedParams, resolvedResultType, dependencies, LateInit(resolvedBody))
        }
      }

      case Inductive(ident, params, constructors) => {
        val (resolvedParams, ctx) = resolveParams(params)
        global += ident
        ctx.withDefinition(ident) { implicit ctx =>
          val resolvedConstructors = constructors.map { constructor =>
            val (resolvedConsParams, _) = resolveParams(constructor.params, ctx)
            Constructor[Expr](constructor.ident, constructor.owner, resolvedConsParams)
          }
          Inductive[Expr](Var.Defined(ident.name), resolvedParams, resolvedConstructors)
        }
      }

      case Overloaded(ident, body) => {
        global += ident
        val (resolvedBody, _) = body.foldLeft((Seq.empty[Function[Expr]], global)) {
          case ((resolvedBody, ctx), function) => {
            // For each function overloading, we need to resolve it twice.
            // This is to avoid the previous dependency graph affect the
            // determination of the current function's dependencies
            //  1. Resolve the function body using global context
            val (resolved, _) = function.resolve(global)
            //  2. Update the dependency graph using updated context
            val (_, newCtx) = function.resolve(ctx)
            (resolvedBody :+ resolved.asInstanceOf[Function[Expr]], newCtx)
          }
        }
        Overloaded[Expr](Var.Defined(ident.name), resolvedBody)
      }

      case _ => unreachable
    }

    (resolvedDefinition, global)
  }

  def resolvePattern(pattern: Pattern[Expr])(implicit ctx: Context): (Pattern[Expr], Context) = {
    given span: SourceSpan = pattern.span
    pattern match {

      case Pattern.Bind(binding) => (Pattern.Bind(binding), ctx + binding)

      case Pattern.Variant(inductive, constructor, patterns) => {
        val (resolvedInductive, inductiveContext) = inductive.resolve
        val (resolvedPatterns, updatedContext) = patterns.foldLeft((Seq.empty[Pattern[Expr]], inductiveContext)) {
          case ((resolvedPatterns, context), pattern) => {
            val (resolved, newCtx) = pattern.resolve(context)
            (resolvedPatterns :+ resolved, newCtx)
          }
        }
        (Pattern.Variant(resolvedInductive, constructor, resolvedPatterns), updatedContext)
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
