package saki.prelude

import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.syntax.{Definition, NativeFunction, Param, Term, Var}

import scala.annotation.targetName

extension (name: String) {
  @targetName("parameter")
  private[prelude] infix def @:(`type`: Term): Param[Term] = {
    Param(Var.Local(name), `type`)
  }
}

trait PreludeDefinitionSet {
  def definitions: Seq[NativeFunction[Term]]
}

object Prelude {
  lazy val definitions: Seq[NativeFunction[Term]] = Seq(
    PrimitiveBool.definitions,
    PrimitiveInt.definitions,
    PrimitiveString.definitions,
  ).flatten

  lazy val environment: Environment.Typed[Value] = Environment.Typed[Value](
    definitions = Prelude.definitions.map {
      definition => definition.ident -> definition
    }.toMap[Var.Defined[Term, ?], Definition[Term]]
  )
}
