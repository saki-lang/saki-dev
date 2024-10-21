package saki.core.syntax

import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.elaborate.Resolve
import saki.core.elaborate.Resolve.resolve
import saki.core.elaborate.Synthesis.synth
import saki.core.syntax.Module.EvalResult
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
  ): (Resolve.Context, Environment.Typed[Value]) = ???

  case class EvalResult(term: Term, `type`: Term) {
    def unapply: (Term, Term) = (term, `type`)
    override def toString: String = s"${term} : ${`type`}"
  }
}
