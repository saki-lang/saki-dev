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
    PrimitiveType.definitions,
    PrimitiveBool.definitions,
    PrimitiveInt.definitions,
    PrimitiveFloat.definitions,
    PrimitiveString.definitions,
  ).flatten

  lazy val symbols: Seq[Var] = definitions.map(_.ident)

  lazy val environment: Environment.Typed[Value] = Environment.Typed.global(Prelude.definitions)
}
