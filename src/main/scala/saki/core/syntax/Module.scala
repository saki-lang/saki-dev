package saki.core.syntax

import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.elaborate.Resolve
import saki.core.elaborate.Resolve.resolve
import saki.core.elaborate.Synthesis.synth

import scala.collection.Seq

case class Module(definitions: Seq[Definition[Term]]) {
  override def toString: String = {
    definitions.map(_.toString).mkString("\n\n")
  }
}

object Module {
  
  def empty: Module = Module(Seq.empty)

  def from(pristineDefinitions: Seq[Definition[Expr]]): Module = {
    var resolvingContext = Resolve.Context.empty
    var env = Environment.Typed[Value]()
    val definitions = pristineDefinitions.map { definition =>
      val (resolved, ctx) = definition.resolve(resolvingContext)
      resolvingContext = ctx
      resolved
    }.map { definition =>
      val definitionSynth = definition.synth(env)
      definitionSynth match {
        case inductive: Inductive[Term] => inductive.constructors.foreach { constructor =>
          env = env.copy(
            definitions = env.definitions.updated(constructor.ident, constructor)
          )
        }
        case _ => ()
      }
      env = env.copy(
        definitions = env.definitions.updated(definitionSynth.ident, definitionSynth)
      )
      println(definitionSynth)
      definitionSynth
    }
    Module(definitions)
  }
}
