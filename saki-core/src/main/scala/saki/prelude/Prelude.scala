package saki.prelude

import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.syntax.*
import saki.error.CoreErrorKind

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
    Seq(panic),
  ).flatten

  lazy val symbols: Seq[Var] = definitions.map(_.ident)

  lazy val environment: Environment.Typed[Value] = Environment.Typed.global(Prelude.definitions)

  val panic: NativeFunction[Term] = NativeFunction(
    ident = Var.Defined("panic"),
    params = Seq("message" @: Term.PrimitiveType(LiteralType.StringType)),
    resultType = Term.PrimitiveType(LiteralType.NothingType),
    isRecursive = true, // We mark this function as recursive to avoid early evaluation
    nativeImpl = (args: ArgList[Value]) => {
      val message = args.head.value match {
        case Value.Primitive(Literal.StringValue(value)) => value
        case _ => "Unknown error"
      }
      CoreErrorKind.PanicFailure.raise(message)
    },
  )
}
