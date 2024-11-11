package saki.prelude
import saki.core.domain.Value
import saki.core.syntax.{NativeFunction, Term, Var}

object PrimitiveRune extends PreludeDefinitionSet {

  import saki.core.syntax.Literal.*
  import saki.core.syntax.LiteralType.*

  override lazy val definitions: Seq[NativeFunction[Term]] = Seq(
    // Ascii to rune
    NativeFunction(
      ident = Var.Defined("rune"),
      params = Seq("codePoint" @: IntType.toTerm),
      resultType = RuneType.toTerm,
      nativeImpl = (args, _) => {
        val codePoint = args.head.value
        codePoint match {
          case Value.Primitive(IntValue(codePoint)) => Value.Primitive(RuneValue(codePoint.toChar))
          case _ => throw new IllegalArgumentException(s"Invalid argument: $codePoint")
        }
      }
    ),

    // Add two runes (Rune -> Rune -> String)
    NativeFunction(
      ident = Var.Defined("++"),
      params = Seq("rune1" @: RuneType.toTerm, "rune2" @: RuneType.toTerm),
      resultType = StringType.toTerm,
      nativeImpl = (args, _) => {
        val rune1 = args.head.value
        val rune2 = args(1).value
        (rune1, rune2) match {
          case (Value.Primitive(RuneValue(rune1)), Value.Primitive(RuneValue(rune2))) => Value.Primitive(StringValue(rune1.toString + rune2.toString))
          case _ => throw new IllegalArgumentException(s"Invalid arguments: $rune1, $rune2")
        }
      }
    ),
  )
}
