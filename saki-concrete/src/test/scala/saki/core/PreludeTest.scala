package saki.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.syntax.Literal.IntValue

class PreludeTest extends AnyFlatSpec with should.Matchers with SakiTestExt {

  extension (code: String) {
    def synth(implicit env: Environment.Typed[Value]): Term = synthExpr(code)._1
  }

  it should "primitive int" in {
    "114 + 514".synth should be (IntValue(628).term)
    "114 - 514".synth should be (IntValue(-400).term)
    "114 * 514".synth should be (IntValue(58596).term)
    "527 / 98".synth should be (IntValue(5).term)
  }

}
