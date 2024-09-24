package saki.core

import scala.annotation.targetName

case class Context(
  values: Map[String, Value] = Map.empty,
  types: Map[String, Value] = Map.empty,
) {
  def names: Set[String] = this.values.keySet

  def typeOf(name: String): Option[Value] = this.types.get(name)

  def valueOf(name: String): Option[Value] = this.values.get(name)

  def addVariable(name: String, `type`: Value): Context = {
    val value = Value.Neutral(NeutralValue.Variable(name))
    this.copy(values = this.values + (name -> value), types = this.types + (name -> `type`))
  }

  @targetName("addVariable")
  def +(entry: (String, Value)): Context = this.addVariable(entry._1, entry._2)
}

object Context {
  def empty: Context = Context()

  def fromValues(values: (String, Value)*): Context = {
    values.foldLeft(Context.empty) {
      case (ctx, (name, value)) => ctx.addVariable(name, value)
    }
  }
}
