package util

import scala.annotation.targetName

class ScopedMap[K, +V] private(
  private val stack: List[Map[K, V]] = List(Map.empty)
) extends Map[K, V] {

  def enter: ScopedMap[K, V] = new ScopedMap(Map.empty :: stack)

  def exit: ScopedMap[K, V] = if (stack.tail.isEmpty) this else new ScopedMap(stack.tail)

  @targetName("plus")
  def :+[V1 >: V](kv: (K, V1)): ScopedMap[K, V1] = updated(kv._1, kv._2)

  override def get(key: K): Option[V] = {
    stack.collectFirst { case map if map.contains(key) => map(key) }
  }

  override def contains(key: K): Boolean = stack.exists(_.contains(key))

  override def removed(key: K): Map[K, V] = new ScopedMap(stack.map(_.removed(key)))

  override def updated[V1 >: V](key: K, value: V1): ScopedMap[K, V1] = {
    new ScopedMap((stack.head + (key -> value)) :: stack.tail)
  }

  override def iterator: Iterator[(K, V)] = stack.iterator.flatMap(_.iterator)
}

object ScopedMap {
  def empty[K, V]: ScopedMap[K, V] = ScopedMap()
  def apply[K, V](elems: (K, V)*): ScopedMap[K, V] = new ScopedMap(List(elems.toMap))
  def apply[K, V](elems: Map[K, V]): ScopedMap[K, V] = new ScopedMap(List(elems))
}
