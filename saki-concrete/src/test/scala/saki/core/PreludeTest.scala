package saki.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PreludeTest extends AnyFlatSpec with should.Matchers with SakiTestExt {
  it should "primitive int" in {
    val (term, _) = synthExpr("114 + 514")
    term should be (Term.Primitive(Literal.IntValue(628)))
  }
}
