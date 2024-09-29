package util

import scala.collection.{AbstractMap, MapFactory, mutable}

/**
 * A map that supports scoped operations, allowing for nested contexts.
 *
 * @param stack The stack of maps representing different scopes.
 * @tparam K The type of keys maintained by this map.
 * @tparam V The type of mapped values.
 */
class ScopedMap[K, +V] private(
  private val stack: List[Map[K, V]] = List(Map.empty)
) extends AbstractMap[K, V] with Map[K, V] {

  /**
   * Enters a new scope by pushing an empty map onto the stack.
   *
   * @return A new `ScopedMap` with an additional scope.
   */
  def enter: ScopedMap[K, V] = new ScopedMap(Map.empty :: stack)

  /**
   * Exits the current scope by popping the top map from the stack.
   *
   * @return A new `ScopedMap` with the top scope removed.
   */
  def exit: ScopedMap[K, V] = if (stack.tail.isEmpty) this else new ScopedMap(stack.tail)

  /**
   * Returns an empty `ScopedMap`.
   *
   * @return An empty `ScopedMap`.
   */
  override def empty: ScopedMap[K, V] = ScopedMap.empty

  /**
   * Returns the factory for creating instances of `ScopedMap`.
   *
   * @return The `MapFactory` for `ScopedMap`.
   */
  override def mapFactory: MapFactory[ScopedMap] = ScopedMap

  /**
   * Adds a key-value pair to the map.
   *
   * @param kv The key-value pair to add.
   * @tparam V1 The type of the value.
   * @return A new `ScopedMap` with the added key-value pair.
   */
  override def +[V1 >: V](kv: (K, V1)): ScopedMap[K, V1] = updated(kv._1, kv._2)

  /**
   * Adds multiple key-value pairs to the map.
   *
   * @param xs The iterable of key-value pairs to add.
   * @tparam V1 The type of the values.
   * @return A new `ScopedMap` with the added key-value pairs.
   */
  override def ++[V1 >: V](xs: IterableOnce[(K, V1)]): ScopedMap[K, V1] = {
    new ScopedMap((stack.head ++ xs) :: stack.tail)
  }

  /**
   * Retrieves the value associated with the specified key.
   *
   * @param key The key whose associated value is to be returned.
   * @return An `Option` containing the value associated with the key, or `None` if the key is not found.
   */
  override def get(key: K): Option[V] = {
    stack.collectFirst { case map if map.contains(key) => map(key) }
  }

  /**
   * Checks if the map contains a mapping for the specified key.
   *
   * @param key The key whose presence in the map is to be tested.
   * @return `true` if the map contains a mapping for the key, `false` otherwise.
   */
  override def contains(key: K): Boolean = stack.exists(_.contains(key))

  /**
   * Removes the mapping for a key from the current scope.
   *
   * @param key The key whose mapping is to be removed.
   * @return A new `ScopedMap` with the mapping removed.
   */
  def removed(key: K): ScopedMap[K, V] = {
    new ScopedMap(stack.head.removed(key) :: stack.tail)
  }

  /**
   * Updates the mapping for a key in the current scope.
   *
   * @param key The key with which the specified value is to be associated.
   * @param value The value to be associated with the specified key.
   * @tparam V1 The type of the value.
   * @return A new `ScopedMap` with the updated mapping.
   */
  def updated[V1 >: V](key: K, value: V1): ScopedMap[K, V1] = {
    new ScopedMap((stack.head + (key -> value)) :: stack.tail)
  }

  /**
   * Returns an iterator over the elements in the map.
   *
   * @return An iterator over the key-value pairs in the map.
   */
  override def iterator: Iterator[(K, V)] = stack.iterator.flatMap(_.iterator)

  /**
   * Executes an action within a new scope.
   *
   * @param action The action to execute within the new scope.
   * @tparam R The return type of the action.
   * @return The result of the action.
   */
  def withScope[R](action: ScopedMap[K, V] => R): R = action(enter)

}

object ScopedMap extends MapFactory[ScopedMap] {
  /**
   * Creates an empty `ScopedMap`.
   *
   * @tparam K The type of keys maintained by this map.
   * @tparam V The type of mapped values.
   * @return An empty `ScopedMap`.
   */
  override def empty[K, V]: ScopedMap[K, V] = new ScopedMap(List.empty)

  /**
   * Creates a `ScopedMap` from an iterable of key-value pairs.
   *
   * @param it The iterable of key-value pairs.
   * @tparam K The type of keys maintained by this map.
   * @tparam V The type of mapped values.
   * @return A `ScopedMap` containing the specified key-value pairs.
   */
  override def from[K, V](it: IterableOnce[(K, V)]): ScopedMap[K, V] = ScopedMap(it.iterator.toMap)

  /**
   * Creates a `ScopedMap` with the specified map.
   *
   * @param elems The map to be included in the `ScopedMap`.
   * @tparam K The type of keys maintained by this map.
   * @tparam V The type of mapped values.
   * @return A `ScopedMap` containing the specified map.
   */
  def apply[K, V](elems: Map[K, V]): ScopedMap[K, V] = new ScopedMap(List(elems))

  /**
   * Returns a new builder for creating instances of `ScopedMap`.
   *
   * @tparam K The type of keys maintained by this map.
   * @tparam V The type of mapped values.
   * @return A new builder for `ScopedMap`.
   */
  override def newBuilder[K, V]: mutable.Builder[(K, V), ScopedMap[K, V]] = {
    new mutable.Builder[(K, V), ScopedMap[K, V]] {
      private val elems = mutable.ListBuffer.empty[(K, V)]
      override def clear(): Unit = elems.clear()
      override def result(): ScopedMap[K, V] = ScopedMap(elems.toMap)
      override def addOne(elem: (K, V)): this.type = {
        elems += elem; this
      }
    }
  }
}
