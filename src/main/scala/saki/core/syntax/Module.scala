package saki.core.syntax

import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.elaborate.Resolve
import saki.core.elaborate.Resolve.resolve
import saki.core.elaborate.Synthesis.synth
import saki.core.syntax.Module.EvalResult

import scala.collection.Seq

case class Module(definitions: Set[Definition[Term]]) {
  override def toString: String = {
    definitions.map(_.toString).mkString("\n\n")
  }

  lazy val env: Environment.Typed[Value] = Environment.Typed.global[Value](definitions.toSeq)

  def eval(expr: Expr): EvalResult = {
    val (term, ty) = expr.resolve(Resolve.Context(env))._1.synth(env).unpack
    EvalResult(term.normalize(env), ty.readBack(env))
  }

}

object Module {
  
  def empty: Module = Module(Set.empty)

  def from(pristineDefinitions: Seq[Definition[Expr]]): Module = {
    val (_, finalEnv) = pristineDefinitions.foldLeft((Resolve.Context.empty, Environment.Typed[Value]())) {
      case ((resolvingContext, env), definition) => {
        val (resolved, newCtx) = definition.resolve(resolvingContext)
        val definitionSynth = resolved.synth(env)
        val updatedEnv = definitionSynth match {
          case inductive: Inductive[Term] =>
            val constructors = inductive.constructors.map(cons => cons.ident -> cons).toMap
            env.copy(definitions = env.definitions ++ constructors)
          case _ => env
        }
        (newCtx, updatedEnv.copy(definitions = updatedEnv.definitions.updated(definitionSynth.ident, definitionSynth)))
      }
    }
    Module(finalEnv.definitions.values.toSet)
  }

  case class EvalResult(term: Term, `type`: Term) {
    def unapply: (Term, Term) = (term, `type`)
    override def toString: String = s"${term} : ${`type`}"
  }
}
