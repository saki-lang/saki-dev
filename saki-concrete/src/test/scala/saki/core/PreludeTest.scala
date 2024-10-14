package saki.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import saki.core.context.Environment
import saki.core.domain.Value

class PreludeTest extends AnyFlatSpec with should.Matchers with SakiTestExt {

  import Literal.*
  import LiteralType.*

  extension (code: String) {
    def synth(implicit env: Environment.Typed[Value]): Term = synthExpr(code)._1.normalize
  }

  it should "primitive int" in {
    "114 + 514".synth should be (IntValue(628).term)
    "114 - 514".synth should be (IntValue(-400).term)
    "114 * 514".synth should be (IntValue(58596).term)
    "527 / 98".synth should be (IntValue(5).term)
    "527 % 98".synth should be (IntValue(37).term)
  }

  it should "int operator precedence" in {
    "527 ** 2 / 98 + 412 * 65".synth should be (IntValue(29613).term)
    "(300 + 876) % 153 * 47 - 29".synth should be (IntValue(4906).term)
    "789 + 32 ** 3 * 15 - 450 / 9".synth should be (IntValue(492259).term)
    "(245 + 678) * (120 - 45) ** 2 / 25".synth should be (IntValue(207675).term)
    "912 - (250 + 37 * (18 ** 2)) / 14".synth should be (IntValue(38).term)
    "645 / 15 + 88 * 22 - 999 % 75".synth should be (IntValue(1955).term)
    "(421 + 256 * 18) ** 2 / (300 - 120) + 59 % 13".synth should be (IntValue(140511).term)
    "(703 + (58 - 19) ** 2) * 3 - (720 / 60 + 30)".synth should be (IntValue(6630).term)
    "480 * (134 + 217) ** 2 - (900 / 45 + 35) * 8".synth should be (IntValue(59136040).term)
  }

  it should "int value cmp" in {
    "114 < 514".synth should be (BoolValue(true).term)
    "114 <= 514".synth should be (BoolValue(true).term)
    "114 > 514".synth should be (BoolValue(false).term)
    "114 >= 514".synth should be (BoolValue(false).term)
    "114 == 514".synth should be (BoolValue(false).term)
    "114 != 514".synth should be (BoolValue(true).term)
  }

  it should "int value logic" in {
    "true && false".synth should be (BoolValue(false).term)
    "true || false".synth should be (BoolValue(true).term)
    "false || true".synth should be (BoolValue(true).term)
    "!true".synth should be (BoolValue(false).term)
    "!false".synth should be (BoolValue(true).term)
  }

  it should "int cmp logic" in {
    "527 + 98 < 1122 && 1919 - 810 > 1926".synth should be (BoolValue(false).term)
    "1122 % 98 == 26 || 1919 / 47 >= 40".synth should be (BoolValue(true).term)
    "817 ** 2 < 222 * 626 && !(114 + 47 >= 61)".synth should be (BoolValue(false).term)
    "89 + 214 >= 19260817 || 626 / 47 < 15".synth should be (BoolValue(true).term)
    "1926 - 1919 < 114 && 222 * (89 + 61) > 810 + 214".synth should be (BoolValue(true).term)
    "!(527 < 89) && 626 % 47 == 14 || 1122 + 1919 < 19260817".synth should be (BoolValue(true).term)
    "(114 <= 626 || 817 / 89 > 10) && (1926 % 89 != 0)".synth should be (BoolValue(true).term)
    "!(1919 == 1919 && 98 ** 2 < 10000) || 19260817 % 47 == 0".synth should be (BoolValue(false).term)
    "((527 * 2 > 98 + 450 || 1122 / 89 < 15) && 1919 + 114 == 2033) || 114514 / 514 > 200".synth should be (BoolValue(true).term)
    "!(114514 % 1919 == 0 && (19260817 / 114 > 100000 || 1926 - 222 <= 1700))".synth should be (BoolValue(true).term)
    "((114 + 514) * 89 == 56186 && 817 % 47 < 10) || !(1919810 > 1926 ** 2 && 222 / 89 < 5)".synth should be (BoolValue(true).term)
    "!(114514 / 626 > 180 && (1926 ** 2 <= 1919810 || 817 * 61 > 50000)) && 222 % 47 == 34".synth should be (BoolValue(true).term)
    "((1919 - 114) ** 2 > 1919810 || (626 / 47 < 15 && 114514 % 89 == 14)) && !(19260817 / 1919 < 10000)".synth should be (BoolValue(true).term)
    "((817 ** 2 < 114514 * 222 || 1919 + 114 == 2033) && !(1926 % 514 == 0 || 98 ** 2 >= 9605)) || 626 + 114514 < 19260817".synth should be (BoolValue(true).term)
    "((114514 / 98 < 1200 && 1919810 % 61 == 35) || !(222 + 514 >= 800)) && 19260817 % 626 == 9".synth should be (BoolValue(false).term)
  }

  it should "string ops" in {
    "\"It's\" ++ \" \" ++ \"mygo\" ++ \"!!!!!\"".synth should be (StringValue("It's mygo!!!!!").term)
    "19260817.toString".synth should be (StringValue("19260817").term)
  }
}
