package saki.prelude
import saki.core.domain.Value
import saki.core.syntax.*

object PrimitiveType extends PreludeDefinitionSet {

  private def primitiveType(ident: String, value: Value): NativeFunction[Term] = NativeFunction(
    ident = Var.Defined(ident),
    params = Seq.empty,
    resultType = Term.Universe,
    nativeImpl = (_: ArgList[Value]) => value
  )

  override lazy val definitions: Seq[NativeFunction[Term]] = Seq(
    primitiveType("'Type", Value.Universe),
    primitiveType("Nothing", LiteralType.NothingType.toValue),
    primitiveType("Unit", LiteralType.UnitType.toValue),
    primitiveType("Bool", LiteralType.BoolType.toValue),
    primitiveType("Int", LiteralType.IntType.toValue),
    primitiveType("ℤ", LiteralType.IntType.toValue),
    primitiveType("Float", LiteralType.FloatType.toValue),
    primitiveType("ℝ", LiteralType.FloatType.toValue),
    primitiveType("Rune", LiteralType.RuneType.toValue),
    primitiveType("String", LiteralType.StringType.toValue),
  )
}
