package saki.core

def fail(message: String): Nothing = throw TypeError(message)

case class TypeError(message: String) extends Exception {
  override def toString: String = s"TypeError: $message"
}
