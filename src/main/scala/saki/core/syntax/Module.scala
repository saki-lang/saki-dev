package saki.core.syntax

import saki.core.typing.{Synthesis, Resolve}
import saki.core.typing.Synthesis.synth
import saki.core.typing.Resolve.resolve

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
    var elaboratingContext = Synthesis.Context.empty
    val definitions = pristineDefinitions.map { definition =>
      val (resolved, ctx) = definition.resolve(resolvingContext)
      resolvingContext = ctx
      resolved
    }.map { definition =>
      val definitionSynth = definition.synth(elaboratingContext)
      definitionSynth match {
        case inductive: Inductive[Term] => inductive.constructors.foreach { constructor =>
          elaboratingContext = elaboratingContext.copy(
            definitions = elaboratingContext.definitions.updated(constructor.ident, constructor)
          )
        }
        case _ => ()
      }
      elaboratingContext = elaboratingContext.copy(
        definitions = elaboratingContext.definitions.updated(definitionSynth.ident, definitionSynth)
      )
      definitionSynth
    }
    Module(definitions)
  }
}
