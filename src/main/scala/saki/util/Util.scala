package saki.util

def unreachable(message: String): Nothing = throw new Exception("Unreachable code: " + message)
def unreachable: Nothing = throw new Exception("Unreachable code")

def unimplemented(message: String): Nothing = throw new Exception("Unimplemented code: " + message)
