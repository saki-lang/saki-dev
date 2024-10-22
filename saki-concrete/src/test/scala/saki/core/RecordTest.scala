package saki.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class RecordTest extends AnyFunSuite with should.Matchers with SakiTestExt {

  import Literal.*
  import LiteralType.*

  test("record definition") {
    val code = {
      """
        type Date = record {
            year: Int
            month: Int
            day: Int
        }

        type Person = record {
            name: String
            birth: Date
        }

        def bandMember: Person = Person ^{
            name = "Soyo Nagasaki"
            birth = Date ^{
                year = 2004
                month = 5
                day = 27
            }
        }
      """
    }
    val module = compileModule(code)
    module.eval("bandMember.name") should be (module.eval("\"Soyo Nagasaki\""))
    module.eval("bandMember.birth.year") should be (module.eval("2004"))
    module.eval("bandMember.birth.month") should be (module.eval("5"))
    module.eval("bandMember.birth.day") should be (module.eval("27"))
  }

  test("record subtyping") {
    val code = {
      """
        type A = record {
            a: Int
        }

        type B = record {
            a: Int
            b: Int
        }

        def f(x: A): Int = x.a
        def g(x: B): Int = f(x)
      """
    }
    compileModule(code)
  }
}