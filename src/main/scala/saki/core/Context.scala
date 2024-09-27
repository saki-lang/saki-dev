package saki.core

import scala.annotation.targetName
import saki.core.unify as staticUnify

case class Context(
  values: List[Value] = List.empty,
  types: List[Element] = List.empty,
  metas: Map[Level, MetaInfo] = Map.empty,
) {
  def level: Level = Level(values.length)

  def addBound(name: String, `type`: Value): Context = copy(
    values = Value.stuckLocal(level) :: values,
    types = Element.Bound(name, `type`) :: types,
  )

  def addDefined(name: String, `type`: Value, value: Value): Context = copy(
    values = value :: values,
    types = Element.Defined(name, `type`, value) :: types,
  )

  def freshMeta(ty: Value): (Context, Level) = {
    val metaLevel = Level(metas.size)
    val metaInfo = MetaInfo.Unresolved(ty)
    (copy(metas = metas.updated(metaLevel, metaInfo)), metaLevel)
  }

  @targetName("add")
  def +(entry: (String, Value)): Context = addBound(entry._1, entry._2)

  @targetName("add")
  def +(entry: (String, Value, Value)): Context = addDefined(entry._1, entry._2, entry._3)

  def lookup(name: String): Option[ContextVariable] = types.zipWithIndex.collectFirst {
    case (Element.Bound(`name`, typ), index) => ContextVariable(Index(index), typ)
    case (Element.Defined(`name`, typ, value), index) => ContextVariable(Index(index), typ)
  }

  def apply(name: String): ContextVariable = lookup(name).getOrElse {
    throw new NoSuchElementException(s"Name not found: $name")
  }

  def unify(value1: Value, value2: Value): Boolean = {
    staticUnify(this, value1, value2)
  }
}

case class ContextVariable(index: Index, `type`: Value)

object Context {
  def empty: Context = Context()
}
