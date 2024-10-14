package saki.prelude

import saki.core.domain.Value
import saki.core.syntax.*
import saki.prelude.@:

// noinspection ZeroIndexToHead
object PrimitiveInt extends PreludeDefinitionSet {

  import saki.core.syntax.Literal.*
  import saki.core.syntax.LiteralType.*

  private def binaryFunction(ident: String, fn: (Int, Int) => Int): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: IntType.toTerm, "b" @: IntType.toTerm),
    resultType = IntType.toTerm,
    nativeImpl = (args: ArgList[Value]) => {
      val a: Value = args(0).value
      val b: Value = args(1).value
      (a, b) match {
        case (Value.Primitive(IntValue(a)), Value.Primitive(IntValue(b))) => Value.Primitive(IntValue(fn(a, b)))
        case _ => throw new IllegalArgumentException(s"Invalid arguments: $a, $b")
      }
    }
  )

  private def unaryFunction(ident: String, fn: Int => Int): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: IntType.toTerm),
    resultType = IntType.toTerm,
    nativeImpl = (args: ArgList[Value]) => {
      val a: Value = args(0).value
      a match {
        case Value.Primitive(IntValue(a)) => Value.Primitive(IntValue(fn(a)))
        case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
      }
    }
  )

  private def binaryBoolFunction(ident: String, fn: (Int, Int) => Boolean): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: IntType.toTerm, "b" @: IntType.toTerm),
    resultType = BoolType.toTerm,
    nativeImpl = (args: ArgList[Value]) => {
      val a: Value = args(0).value
      val b: Value = args(1).value
      (a, b) match {
        case (Value.Primitive(IntValue(a)), Value.Primitive(IntValue(b))) => Value.Primitive(BoolValue(fn(a, b)))
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
    binaryFunction("**", Math.pow(_, _).toInt),

    binaryFunction("xor", _ ^ _),
    binaryFunction("and", _ & _),
    binaryFunction("or", _ | _),
    binaryFunction("shl", _ << _),
    binaryFunction("shr", _ >> _),
    binaryFunction("ushr", _ >>> _),

    binaryBoolFunction("==", _ == _),
    binaryBoolFunction("!=", _ != _),
    binaryBoolFunction("<", _ < _),
    binaryBoolFunction(">", _ > _),
    binaryBoolFunction("<=", _ <= _),
    binaryBoolFunction(">=", _ >= _),

    unaryFunction("abs", Math.abs),
    unaryFunction("inc", _ + 1),
    unaryFunction("dec", _ - 1),
    unaryFunction("--", -_),
    unaryFunction("inv", ~_),
  )

}
