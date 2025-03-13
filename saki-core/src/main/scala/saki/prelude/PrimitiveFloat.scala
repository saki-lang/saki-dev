package saki.prelude

import saki.core.domain.Value
import saki.core.syntax.*
import saki.core.term
import saki.core.term.Term
import saki.prelude.@:

// noinspection ZeroIndexToHead
object PrimitiveFloat extends PreludeDefinitionSet {

  import saki.core.syntax.Literal.*
  import saki.core.syntax.LiteralType.*

  private def binaryFunction(ident: String, fn: (Double, Double) => Double): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: FloatType.toTerm, "b" @: FloatType.toTerm),
    resultType = FloatType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
      val a: Value = args(0).value
      val b: Value = args(1).value
      (a, b) match {
        case (Value.Primitive(FloatValue(a)), Value.Primitive(FloatValue(b))) => Value.Primitive(FloatValue(fn(a, b)))
        case _ => throw new IllegalArgumentException(s"Invalid arguments: $a, $b")
      }
    }
  )

  private def unaryFunction(ident: String, fn: Double => Double): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: FloatType.toTerm),
    resultType = FloatType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
      val a: Value = args(0).value
      a match {
        case Value.Primitive(FloatValue(a)) => Value.Primitive(FloatValue(fn(a)))
        case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
      }
    }
  )

  private def unaryIntFunction(ident: String, fn: Double => BigInt): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: FloatType.toTerm),
    resultType = IntType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
      val a: Value = args(0).value
      a match {
        case Value.Primitive(FloatValue(a)) => Value.Primitive(IntValue(fn(a)))
        case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
      }
    }
  )

  private def binaryBoolFunction(ident: String, fn: (Double, Double) => Boolean): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: FloatType.toTerm, "b" @: FloatType.toTerm),
    resultType = BoolType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
      val a: Value = args(0).value
      val b: Value = args(1).value
      (a, b) match {
        case (Value.Primitive(FloatValue(a)), Value.Primitive(FloatValue(b))) => Value.Primitive(BoolValue(fn(a, b)))
        case _ => throw new IllegalArgumentException(s"Invalid arguments: $a, $b")
      }
    }
  )

  override lazy val definitions: Seq[NativeFunction[Term]] = Seq(

    binaryFunction("+", _ + _),
    binaryFunction("-", _ - _),
    binaryFunction("*", _ * _),
    binaryFunction("/", _ / _),
    binaryFunction("%", _ % _),

    binaryFunction("**", Math.pow),

    binaryFunction("max", _.max(_)),
    binaryFunction("min", _.min(_)),

    binaryBoolFunction("<", _ < _),
    binaryBoolFunction(">", _ > _),
    binaryBoolFunction("<=", _ <= _),
    binaryBoolFunction(">=", _ >= _),

    unaryFunction("abs", _.abs),
    unaryFunction("inc", _ + 1),
    unaryFunction("dec", _ - 1),
    unaryFunction("--", -_),

    unaryFunction("sin", Math.sin),
    unaryFunction("cos", Math.cos),
    unaryFunction("tan", Math.tan),
    unaryFunction("asin", Math.asin),
    unaryFunction("acos", Math.acos),
    unaryFunction("atan", Math.atan),
    unaryFunction("exp", Math.exp),
    unaryFunction("log", Math.log),
    unaryFunction("sqrt", Math.sqrt),

    unaryIntFunction("ceil", value => BigInt(Math.ceil(value).toInt)),
    unaryIntFunction("floor", value => BigInt(Math.floor(value).toInt)),

    // round
    NativeFunction(
      ident = Var.Defined("round"),
      params = Seq("a" @: FloatType.toTerm),
      resultType = FloatType.toTerm,
      nativeImpl = (args: ArgList[Value], _) => {
        val a: Value = args(0).value
        a match {
          case Value.Primitive(FloatValue(a)) => Value.Primitive(IntValue(Math.round(a)))
          case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
        }
      }
    ),

    // toString
    NativeFunction(
      ident = Var.Defined("toString"),
      params = Seq("a" @: FloatType.toTerm),
      resultType = StringType.toTerm,
      nativeImpl = (args: ArgList[Value], _) => {
        val str: Value = args(0).value
        str match {
          case Value.Primitive(FloatValue(str)) => Value.Primitive(StringValue(str.toString))
          case _ => throw new IllegalArgumentException(s"Invalid argument: $str")
        }
      }
    )
  )

}
