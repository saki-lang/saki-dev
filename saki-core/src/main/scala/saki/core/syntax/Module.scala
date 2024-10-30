package saki.core.syntax

import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.elaborate.Resolve
import saki.core.elaborate.Resolve.resolve
import saki.core.elaborate.Synthesis.{synth, synthDeclaration}
import saki.core.syntax.Module.EvalResult
import saki.core.syntax.OverloadedDeclaration.merge
import saki.prelude.Prelude
import saki.util.Graph

import scala.collection.Seq

case class Module(definitions: Set[Definition[Term]]) {

  override def toString: String = {
    definitions.map(_.toString).mkString("\n\n")
  }

  lazy val env: Environment.Typed[Value] = Environment.Typed.global[Value](definitions.toSeq)

  def evaluate(expr: Expr): EvalResult = {
    val (term, ty) = expr.resolve(Resolve.Context(env))._1.synth(env).unpack
    EvalResult(term.forceEval(env).readBack(env), ty.readBack(env))(env)
  }

}

object Module {

  def empty: Module = Module(Set.empty)

  def from(definitions: Seq[Definition[Expr]]): Module = {

    // Pre-resolve phase: add all definitions to the context
    val preResolveContext = definitions.foldLeft(Resolve.Context(Prelude.symbols)) {
      (ctx, definition) => ctx + definition.ident
    }

    val resolvedContext = definitions.foldLeft(preResolveContext) {
      (ctx, definition) => definition.resolve(ctx)._2
    }

    val stronglyConnectedSeq: Seq[Set[Definition[Expr]]] = {
      resolvedContext.dependencyGraph.stronglyConnectedComponents.reverse.map(
        // Extract the names of definitions in the strongly connected component
        // Filter out definitions that are not in the current module
        // Overloaded functions in the *SAME MODULE* are gathered in the same
        // strongly connected component since they are identified by the same name
        _.flatMap(name => definitions.filter(name == _.ident.name))
      )
    }.filter(_.nonEmpty)

    val (_, finalEnv) = stronglyConnectedSeq.foldLeft((resolvedContext, Prelude.environment)) {
      case ((resolvingContext, env), definitions) => {
        if definitions.size == 1 then {
          // If the strongly connected component contains only one definition,
          // resolve and synthesis it immediately
          val (resolved, newCtx) = definitions.head.resolve(resolvingContext)
          val definitionSynth = resolved.synth(env)
          (newCtx, env.add(definitionSynth))
        } else {
          // Otherwise, the definitions in the strongly connected component
          // are mutually recursive, so we need to resolve them together
          synthStronglyConnectedDefinitions(definitions, resolvingContext.dependencyGraph)(env)
        }
      }
    }

    Module(finalEnv.definitions.values.toSet)
  }

  private def synthStronglyConnectedDefinitions(
    definitions: Set[Definition[Expr]],
    dependencyGraph: Graph[String],
  )(
    implicit env: Environment.Typed[Value]
  ): (Resolve.Context, Environment.Typed[Value]) = {
    // TODO: In a same strongly connected component, the dependency order of the
    //  params and the return type of the functions should be preserved:
    //  sort the definitions in the strongly connected component based on the
    //  topological order of the dependency relation between types
    //  e.g. def A: C; def B: A. In this case, B should be resolved before A

    // Pre-build declarations for definitions of the strongly connected component
    // Step 1. Merge overloaded functions
    val declarations = definitions.map(synthDeclaration).flatMap { declaration =>
      env.getDefinitionByName(declaration.ident.name) match {
        // If the definition already exists in the environment, merge it with the new declaration
        case Some(definition) => Seq(definition.toDeclaration(Term)) ++ Seq(declaration)
        case _ => Seq(declaration)
      }
    }.groupBy(_.ident).map {
      case (_, decls) => if decls.size == 1 then decls.head else {
        decls.reduce { (decl1, decl2) => merge(decl1.asInstanceOf, decl2.asInstanceOf) }
      }
    }
    // Step 2. Add declarations to the environment
    val declEnv: Environment.Typed[Value] = declarations.foldLeft(env) {
      (env, declaration) => env.addDeclaration(declaration)
    }
    // Step 3. Resolve and synthesis definitions with the pre-built declarations
    val variables = declEnv.definitions.keys ++ declEnv.declarations.keys
    val resolveCtx = Resolve.Context(
      variableMap = variables.map(variable => variable.name -> variable).toMap,
      dependencyGraph = dependencyGraph,
    )
    definitions.foldLeft((resolveCtx, env)) {
      case ((resolvingContext, env), definition) => {
        val (resolved, newCtx) = definition.resolve(resolvingContext)
        // We only use the environment with declarations for synthesis with in the same strongly connected component
        val definitionSynth = resolved.synth(declEnv)
        (newCtx, env.add(definitionSynth))
      }
    }
  }

  case class EvalResult(term: Term, `type`: Term)(
    implicit env: Environment.Typed[Value]
  ) {
    def unapply: (Term, Term) = (term, `type`)
    override def toString: String = s"${term.evalString} : ${`type`.evalString}"
    override def equals(obj: Any): Boolean = obj match {
      case EvalResult(term2, type2) => term == term2 && `type` == type2
      case _ => false
    }
  }
}
