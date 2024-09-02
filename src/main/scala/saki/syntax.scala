package saki.syntax

import saki._

enum PrimitiveType {
  case UnitType
  case BoolType
  case IntType
  case FloatType
  case CharType
  case StringType
}

enum PrimitiveValue {
  case UnitValue
  case BoolValue(value: Boolean)
  case IntValue(value: Int)
  case FloatValue(value: Float)
  case CharValue(value: Char)
  case StringValue(value: String)
}

case class BoundVariable(name: String, `type`: Term)

enum ApplyMode {
  case Explicit
  case Implicit
  case Instance
}

enum Term {
  case Variable(name: String)
  case PrimitiveType(`type`: syntax.PrimitiveType)
  case PrimitiveValue(value: syntax.PrimitiveValue)
  case Application(function: Term, argument: Term)
  case Function(param: BoundVariable, applyMode: ApplyMode, body: Term, returnType: Option[Term])
  case FunctionType(paramType: Term, applyMode: ApplyMode, returnType: Term)
  case CodeBlock(statements: List[Term])
  case Let(variable: BoundVariable, value: Term)
  case If(condition: Term, thenBranch: Term, elseBranch: Option[Term])
  case Match(scrutinee: Term, cases: List[(Term, Term)])
  case SumType(cases: List[Term])
  case RecordType(fields: List[(String, Term)])
  case RecordValue(fields: List[(String, Term)])
  case Universe(level: Int)
}
