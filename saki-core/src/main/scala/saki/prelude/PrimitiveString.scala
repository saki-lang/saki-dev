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
    nativeImpl = (args: ArgList[Value], _) => {
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
    nativeImpl = (args: ArgList[Value], _) => {
      val a: Value = args(0).value
      a match {
        case Value.Primitive(StringValue(a)) => Value.Primitive(StringValue(fn(a)))
        case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
      }
    }
  )

  private def binaryBoolFunction(ident: String, fn: (String, String) => Boolean): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: StringType.toTerm, "b" @: StringType.toTerm),
    resultType = BoolType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
      val a: Value = args(0).value
      val b: Value = args(1).value
      (a, b) match {
        case (Value.Primitive(StringValue(a)), Value.Primitive(StringValue(b))) => Value.Primitive(BoolValue(fn(a, b)))
        case _ => throw new IllegalArgumentException(s"Invalid arguments: $a, $b")
      }
    }
  )

  private def unaryBoolFunction(ident: String, fn: String => Boolean): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: StringType.toTerm),
    resultType = BoolType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
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
    nativeImpl = (args: ArgList[Value], _) => {
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
    nativeImpl = (args: ArgList[Value], _) => {
      val a: Value = args(0).value
      a match {
        case Value.Primitive(StringValue(a)) => Value.Primitive(IntValue(BigInt(a)))
        case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
      }
    }
  )
  
  val parseFloat: NativeFunction[Term] = NativeFunction(
    ident = Var.Defined("parseFloat"),
    params = Seq("floatString" @: StringType.toTerm),
    resultType = FloatType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
      val a: Value = args(0).value
      a match {
        case Value.Primitive(StringValue(a)) => Value.Primitive(FloatValue(a.toDouble))
        case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
      }
    }
  )

  val repeat: NativeFunction[Term] = NativeFunction(
    ident = Var.Defined("repeat"),
    params = Seq("str" @: StringType.toTerm, "n" @: IntType.toTerm),
    resultType = StringType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
      val str: Value = args(0).value
      val n: Value = args(1).value
      (str, n) match {
        case (Value.Primitive(StringValue(str)), Value.Primitive(IntValue(n))) => Value.Primitive(StringValue(str * n.toInt))
        case _ => throw new IllegalArgumentException(s"Invalid arguments: $str, $n")
      }
    }
  )

  val appendRune: NativeFunction[Term] = NativeFunction(
    ident = Var.Defined("++"),
    params = Seq("str" @: StringType.toTerm, "c" @: RuneType.toTerm),
    resultType = StringType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
      val str: Value = args(0).value
      val c: Value = args(1).value
      (str, c) match {
        case (Value.Primitive(StringValue(str)), Value.Primitive(RuneValue(c))) => Value.Primitive(StringValue(str + c))
        case _ => throw new IllegalArgumentException(s"Invalid arguments: $str, $c")
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
    parseFloat,
    repeat,
    appendRune,
  )
  
}
