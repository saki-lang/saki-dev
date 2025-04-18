package saki.prelude

import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.syntax.*
import saki.core.term
import saki.core.term
import saki.core.term.Term
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
    PrimitiveRune.definitions,
    PrimitiveString.definitions,
    Seq(equals, notEquals, panic),
  ).flatten

  lazy val symbols: Seq[Var] = definitions.map(_.ident)

  lazy val environment: Environment.Typed[Value] = Environment.Typed.global(Prelude.definitions)

  private val equals: NativeFunction[Term] = NativeFunction(
    ident = Var.Defined("=="),
    params = Seq("a" @: term.PrimitiveType(LiteralType.AnyType), "b" @: term.PrimitiveType(LiteralType.AnyType)),
    resultType = term.PrimitiveType(LiteralType.BoolType),
    nativeImpl = (args: ArgList[Value], env: Environment.Typed[Value]) => {
      assert(args.size == 2)
      val a = args.head.value
      val b = args.tail.head.value
      Value.Primitive(Literal.BoolValue(a.reflect(env) == b.reflect(env)))
    },
  )

  private val notEquals: NativeFunction[Term] = NativeFunction(
    ident = Var.Defined("!="),
    params = Seq("a" @: term.PrimitiveType(LiteralType.AnyType), "b" @: term.PrimitiveType(LiteralType.AnyType)),
    resultType = term.PrimitiveType(LiteralType.BoolType),
    nativeImpl = (args: ArgList[Value], env: Environment.Typed[Value]) => {
      assert(args.size == 2)
      val a = args.head.value
      val b = args.tail.head.value
      Value.Primitive(Literal.BoolValue(a.reflect(env) != b.reflect(env)))
    },
  )

  private val panic: NativeFunction[Term] = NativeFunction(
    ident = Var.Defined("panic"),
    params = Seq("message" @: term.PrimitiveType(LiteralType.StringType)),
    resultType = term.PrimitiveType(LiteralType.NothingType),
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
