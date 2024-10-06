package saki.core.syntax

import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.elaborate.{Resolve, Synthesis}
import saki.core.elaborate.Synthesis.synth
import saki.core.elaborate.Resolve.resolve

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
    var elaboratingContext = Environment.Typed[Value]()
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
