package saki.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RecordTest extends AnyFlatSpec with should.Matchers with SakiTestExt {

  import Literal.*
  import LiteralType.*

  it should "record" in {
    val code = {
      """
        type Person = record {
          name: String
          age: Int
        }
      """
    }
    val module = compileModule(code)

  }
}