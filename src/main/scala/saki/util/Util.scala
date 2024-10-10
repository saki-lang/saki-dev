package saki.util

case class UnreachableException(message: String) extends Exception(message)

case class UnimplementedException(message: String) extends Exception(message)

def unreachable(message: String): Nothing = throw UnreachableException("Unreachable code: " + message)

def unreachable: Nothing = throw UnreachableException("Unreachable code")

def unimplemented(message: String): Nothing = throw UnimplementedException("Unimplemented code: " + message)
