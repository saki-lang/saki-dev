package saki.util

import scala.language.implicitConversions

case class OptionCache[+T](private val value: Option[T]) {
  override def equals(obj: Any): Boolean = obj.isInstanceOf[OptionCache[_]]
  override def hashCode(): Int = classOf[OptionCache[T]].hashCode()
  implicit def toOption: Option[T] = this.value
}

object OptionCache {
  implicit def fromOption[T](value: Option[T]): OptionCache[T] = OptionCache(value)
}
