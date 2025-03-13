package saki.prelude

import saki.core.domain.Value
import saki.core.syntax.*
import saki.core.term
import saki.core.term.Term
import saki.prelude.@:

import scala.annotation.tailrec

// noinspection ZeroIndexToHead
object PrimitiveInt extends PreludeDefinitionSet {

  import saki.core.syntax.Literal.*
  import saki.core.syntax.LiteralType.*

  private def binaryFunction(ident: String, fn: (BigInt, BigInt) => BigInt): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: IntType.toTerm, "b" @: IntType.toTerm),
    resultType = IntType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
      val a: Value = args(0).value
      val b: Value = args(1).value
      (a, b) match {
        case (Value.Primitive(IntValue(a)), Value.Primitive(IntValue(b))) => Value.Primitive(IntValue(fn(a, b)))
        case _ => throw new IllegalArgumentException(s"Invalid arguments: $a, $b")
      }
    }
  )

  private def unaryFunction(ident: String, fn: BigInt => BigInt): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: IntType.toTerm),
    resultType = IntType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
      val a: Value = args(0).value
      a match {
        case Value.Primitive(IntValue(a)) => Value.Primitive(IntValue(fn(a)))
        case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
      }
    }
  )

  private def unaryFloatFunction(ident: String, fn: BigInt => Double): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: IntType.toTerm),
    resultType = FloatType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
      val a: Value = args(0).value
      a match {
        case Value.Primitive(IntValue(a)) => Value.Primitive(FloatValue(fn(a)))
        case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
      }
    }
  )

  private def binaryBoolFunction(ident: String, fn: (BigInt, BigInt) => Boolean): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: IntType.toTerm, "b" @: IntType.toTerm),
    resultType = BoolType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
      val a: Value = args(0).value
      val b: Value = args(1).value
      (a, b) match {
        case (Value.Primitive(IntValue(a)), Value.Primitive(IntValue(b))) => Value.Primitive(BoolValue(fn(a, b)))
        case _ => throw new IllegalArgumentException(s"Invalid arguments: $a, $b")
      }
    }
  )

  private def fastPow(base: BigInt, exp: BigInt): BigInt = {
    @tailrec
    def loop(base: BigInt, exponent: BigInt, acc: BigInt): BigInt = {
      if (exponent == 0) acc
      else if (exponent % 2 == 0) loop(base * base, exponent / 2, acc)
      else loop(base, exponent - 1, acc * base)
    }
    loop(base, exp, 1)
  }
  
  override lazy val definitions: Seq[NativeFunction[Term]] = Seq(

    binaryFunction("+", _ + _),
    binaryFunction("-", _ - _),
    binaryFunction("*", _ * _),
    binaryFunction("/", _ / _),
    binaryFunction("%", _ % _),

    binaryFunction("**", fastPow),

    binaryFunction("xor", _ ^ _),
    binaryFunction("and", _ & _),
    binaryFunction("or", _ | _),
    
    // binaryFunction("shl", _ << _),
    // binaryFunction("shr", _ >> _),
    // binaryFunction("ushr", _ >>> _),

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
    unaryFunction("inv", ~_),

    // toFloat
    unaryFloatFunction("toFloat", _.toDouble),
    unaryFloatFunction("sqrt", value => Math.sqrt(value.toDouble)),
    unaryFloatFunction("sin", value => Math.sin(value.toDouble)),
    unaryFloatFunction("cos", value => Math.cos(value.toDouble)),
    unaryFloatFunction("tan", value => Math.tan(value.toDouble)),
    unaryFloatFunction("asin", value => Math.asin(value.toDouble)),
    unaryFloatFunction("acos", value => Math.acos(value.toDouble)),
    unaryFloatFunction("atan", value => Math.atan(value.toDouble)),
    unaryFloatFunction("exp", value => Math.exp(value.toDouble)),
    unaryFloatFunction("log", value => Math.log(value.toDouble)),

    // toString
    NativeFunction(
      ident = Var.Defined("toString"),
      params = Seq("str" @: IntType.toTerm),
      resultType = StringType.toTerm,
      nativeImpl = (args: ArgList[Value], _) => {
        val str: Value = args(0).value
        str match {
          case Value.Primitive(IntValue(str)) => Value.Primitive(StringValue(str.toString))
          case _ => throw new IllegalArgumentException(s"Invalid argument: $str")
        }
      }
    )
  )

}
