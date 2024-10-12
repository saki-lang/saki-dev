package saki.util

import scala.annotation.targetName

case class LateInit[A]() extends Product with IterableOnce[A] {

  private var value: Option[A] = None

  def toOption: Option[A] = value

  def get: A = value match {
    case Some(value) => value
    case None => throw new NoSuchElementException("LateInit has not been initialized")
  }

  def getOrElse(default: A): A = value.getOrElse(default)

  def getOrNull[A1 >: A](implicit ev: Null <:< A1): A1 = value.orNull

  def map[B](f: A => B): LateInit[B] = {
    val initValue = LateInit[B]()
    value.foreach(v => initValue := f(v))
    return initValue
  }

  def isDefined: Boolean = value.isDefined

  @targetName("init")
  def :=[A1 <: A](value: A1): Unit = {
    if (this.value.isDefined) {
      throw new IllegalStateException("LateInit has already been initialized")
    }
    this.value = Some(value)
  }

  /**
   * Deal with type erasure.
   */
  @targetName("forceInit")
  def :=![B](value: B): Unit = {
    this.value = Some(value.asInstanceOf[A])
  }

  override def iterator: Iterator[A] = value.iterator

  override def toString: String = value match {
    case Some(value) => value.toString
    case None => "uninitialized"
  }
}

object LateInit {

  def apply[A](value: A): LateInit[A] = {
    val initValue = LateInit[A]()
    initValue := value
    return initValue
  }

  def unapply[A](lateInit: LateInit[A]): Option[A] = lateInit.toOption

  def apply[A](): LateInit[A] = new LateInit[A]()
}

given [A]: Conversion[A, LateInit[A]] = LateInit.apply(_)
