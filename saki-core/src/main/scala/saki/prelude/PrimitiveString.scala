package saki.prelude

import saki.core.domain.Value
import saki.core.syntax.{ArgList, NativeFunction, Term, Var}

// noinspection ZeroIndexToHead
object PrimitiveString extends PreludeDefinitionSet {

  import saki.core.syntax.Literal.*
  import saki.core.syntax.LiteralType.*

  private def binaryFunction(ident: String, fn: (String, String) => String): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: StringType.toTerm, "b" @: StringType.toTerm),
    resultType = StringType.toTerm,
    nativeImpl = (args: ArgList[Value]) => {
      val a: Value = args(0).value
      val b: Value = args(1).value
      (a, b) match {
        case (Value.Primitive(StringValue(a)), Value.Primitive(StringValue(b))) => Value.Primitive(StringValue(fn(a, b)))
        case _ => throw new IllegalArgumentException(s"Invalid arguments: $a, $b")
      }
    }
  )

  private def unaryFunction(ident: String, fn: String => String): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: StringType.toTerm),
    resultType = StringType.toTerm,
    nativeImpl = (args: ArgList[Value]) => {
      val a: Value = args(0).value
      a match {
        case Value.Primitive(StringValue(a)) => Value.Primitive(StringValue(fn(a)))
        case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
      }
    }
  )

  private def unaryBoolFunction(ident: String, fn: String => Boolean): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: StringType.toTerm),
    resultType = BoolType.toTerm,
    nativeImpl = (args: ArgList[Value]) => {
      val a: Value = args(0).value
      a match {
        case Value.Primitive(StringValue(a)) => Value.Primitive(BoolValue(fn(a)))
        case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
      }
    }
  )

  val length: NativeFunction[Term] = NativeFunction(
    ident = Var.Defined("length"),
    params = Seq("str" @: StringType.toTerm),
    resultType = IntType.toTerm,
    nativeImpl = (args: ArgList[Value]) => {
      val a: Value = args(0).value
      a match {
        case Value.Primitive(StringValue(a)) => Value.Primitive(IntValue(a.length))
        case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
      }
    }
  )

  val parseInt: NativeFunction[Term] = NativeFunction(
    ident = Var.Defined("parseInt"),
    params = Seq("intString" @: StringType.toTerm),
    resultType = IntType.toTerm,
    nativeImpl = (args: ArgList[Value]) => {
      val a: Value = args(0).value
      a match {
        case Value.Primitive(StringValue(a)) => Value.Primitive(IntValue(a.toLong))
        case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
      }
    }
  )
  
  val parseFloat: NativeFunction[Term] = NativeFunction(
    ident = Var.Defined("parseFloat"),
    params = Seq("floatString" @: StringType.toTerm),
    resultType = FloatType.toTerm,
    nativeImpl = (args: ArgList[Value]) => {
      val a: Value = args(0).value
      a match {
        case Value.Primitive(StringValue(a)) => Value.Primitive(FloatValue(a.toDouble))
        case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
      }
    }
  )

  override lazy val definitions: Seq[NativeFunction[Term]] = Seq(
    unaryFunction("toLowerCase", _.toLowerCase),
    unaryFunction("toUpperCase", _.toUpperCase),
    unaryFunction("trim", _.trim),
    unaryFunction("reverse", _.reverse),
    unaryBoolFunction("isEmpty", _.isEmpty),
    unaryBoolFunction("nonEmpty", _.nonEmpty),
    binaryFunction("++", _ + _),
    length,
    parseInt,
  )
  
}
