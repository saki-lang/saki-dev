package saki.prelude

import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.syntax.*
import saki.error.PanicError

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
    Seq(equals, notEquals, panic),
  ).flatten

  lazy val symbols: Seq[Var] = definitions.map(_.ident)

  lazy val environment: Environment.Typed[Value] = Environment.Typed.global(Prelude.definitions)

  private val equals: NativeFunction[Term] = NativeFunction(
    ident = Var.Defined("=="),
    params = Seq("a" @: Term.PrimitiveType(LiteralType.AnyType), "b" @: Term.PrimitiveType(LiteralType.AnyType)),
    resultType = Term.PrimitiveType(LiteralType.BoolType),
    nativeImpl = (args: ArgList[Value], env: Environment.Typed[Value]) => {
      assert(args.size == 2)
      val a = args.head.value
      val b = args.tail.head.value
      Value.Primitive(Literal.BoolValue(a.readBack(env) == b.readBack(env)))
    },
  )

  private val notEquals: NativeFunction[Term] = NativeFunction(
    ident = Var.Defined("!="),
    params = Seq("a" @: Term.PrimitiveType(LiteralType.AnyType), "b" @: Term.PrimitiveType(LiteralType.AnyType)),
    resultType = Term.PrimitiveType(LiteralType.BoolType),
    nativeImpl = (args: ArgList[Value], env: Environment.Typed[Value]) => {
      assert(args.size == 2)
      val a = args.head.value
      val b = args.tail.head.value
      Value.Primitive(Literal.BoolValue(a.readBack(env) != b.readBack(env)))
    },
  )

  private val panic: NativeFunction[Term] = NativeFunction(
    ident = Var.Defined("panic"),
    params = Seq("message" @: Term.PrimitiveType(LiteralType.StringType)),
    resultType = Term.PrimitiveType(LiteralType.NothingType),
    isRecursive = true, // We mark this function as recursive to avoid early evaluation
    nativeImpl = (args: ArgList[Value], _) => {
      val message = args.head.value match {
        case Value.Primitive(Literal.StringValue(value)) => value
        case _ => "Unknown error"
      }
      throw PanicError(message)
    },
  )
}
