package saki.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.*
import saki.cli.compileModule

class DefinitionTest extends AnyFlatSpec with should.Matchers {
  it should "" in {
    val code = {
      """
      |  type Bool = inductive {
      |      True
      |      False
      |  }
      |""".stripMargin
    }
    val module = compileModule(code)
    module.definitions.foreach(println)
  }

}
