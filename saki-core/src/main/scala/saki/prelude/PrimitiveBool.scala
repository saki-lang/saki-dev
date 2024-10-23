package saki.prelude

import saki.core.domain.Value
import saki.core.syntax.{ArgList, NativeFunction, Term, Var}

// noinspection ZeroIndexToHead
object PrimitiveBool extends PreludeDefinitionSet {

  import saki.core.syntax.Literal.*
  import saki.core.syntax.LiteralType.*

  private def binaryFunction(ident: String, fn: (Boolean, Boolean) => Boolean): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: BoolType.toTerm, "b" @: BoolType.toTerm),
    resultType = BoolType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
      val a: Value = args(0).value
      val b: Value = args(1).value
      (a, b) match {
        case (Value.Primitive(BoolValue(a)), Value.Primitive(BoolValue(b))) => Value.Primitive(BoolValue(fn(a, b)))
        case _ => throw new IllegalArgumentException(s"Invalid arguments: $a, $b")
      }
    }
  )

  private def unaryFunction(ident: String, fn: Boolean => Boolean): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq("a" @: BoolType.toTerm),
    resultType = BoolType.toTerm,
    nativeImpl = (args: ArgList[Value], _) => {
      val a: Value = args(0).value
      a match {
        case Value.Primitive(BoolValue(a)) => Value.Primitive(BoolValue(fn(a)))
        case _ => throw new IllegalArgumentException(s"Invalid argument: $a")
      }
    }
  )
  
  override lazy val definitions: Seq[NativeFunction[Term]] = Seq(
    binaryFunction("&&", _ && _),
    binaryFunction("||", _ || _),
    binaryFunction("^", _ ^ _),
    unaryFunction("!", !_),
  )


}
