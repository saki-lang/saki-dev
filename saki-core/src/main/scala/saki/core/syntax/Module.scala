package saki.core.syntax

import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.elaborate.Resolve
import saki.core.elaborate.Resolve.resolve
import saki.core.elaborate.Synthesis.{synth, synthPreDeclaration}
import saki.core.syntax.Module.EvalResult
import saki.core.syntax.OverloadedPreDeclaration.merge
import saki.prelude.Prelude

import scala.collection.Seq

case class Module(definitions: Set[Definition[Term]]) {

  override def toString: String = {
    definitions.map(_.toString).mkString("\n\n")
  }

  lazy val env: Environment.Typed[Value] = Environment.Typed.global[Value](definitions.toSeq)

  def evaluate(expr: Expr): EvalResult = {
    val (term, ty) = expr.resolve(Resolve.Context(env))._1.synth(env).unpack
    EvalResult(term.normalize(env), ty.readBack(env))
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
        // Overloaded functions are gathered in the same strongly connected component
        // since they have the same name
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
          synthStronglyConnectedDefinitions(definitions)(env)
        }
      }
    }

    Module(finalEnv.definitions.values.toSet)
  }

  private def synthStronglyConnectedDefinitions(definitions: Set[Definition[Expr]])(
    implicit env: Environment.Typed[Value]
  ): (Resolve.Context, Environment.Typed[Value]) = {
    // Pre-build declarations for definitions of the strongly connected component
    // Step 1. Merge overloaded functions
    val declarations = definitions.map(synthPreDeclaration).groupBy(_.ident).map {
      case (_, decls) => if decls.size == 1 then decls.head else {
        decls.reduce { (decl1, decl2) => merge(decl1.asInstanceOf, decl2.asInstanceOf) }
      }
    }
    val declEnv: Environment.Typed[Value] = declarations.foldLeft(env) {
      (env, declaration) => env.addDeclaration(declaration)
    }
    definitions.foldLeft((Resolve.Context(env), env)) {
      case ((resolvingContext, env), definition) => {
        val (resolved, newCtx) = definition.resolve(resolvingContext)
        val definitionSynth = resolved.synth(declEnv)
        (newCtx, env.add(definitionSynth))
      }
    }
  }

  case class EvalResult(term: Term, `type`: Term) {
    def unapply: (Term, Term) = (term, `type`)
    override def toString: String = s"${term} : ${`type`}"
  }
}
