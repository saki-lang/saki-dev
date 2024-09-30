package saki.util

def unreachable(message: String): Nothing = throw new Exception("Unreachable code: " + message)
def unreachable: Nothing = throw new Exception("Unreachable code")
