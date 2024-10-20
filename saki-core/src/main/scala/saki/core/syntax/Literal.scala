package saki.core.syntax

import saki.core.domain.Value

enum Literal {

  case UnitValue
  case BoolValue(value: Boolean)
  case IntValue(value: BigInt)
  case FloatValue(value: Double)
  case RuneValue(value: Char)
  case StringValue(value: String)

  override def toString: String = this match {
    case UnitValue => "unit"
    case BoolValue(value) => value.toString
    case IntValue(value) => value.toString
    case FloatValue(value) => value.toString + 'f'
    case RuneValue(value) => s"'${value.toString}'"
    case StringValue(value) => s""""$value""""
  }

  def ty: LiteralType = this match {
    case UnitValue => LiteralType.UnitType
    case BoolValue(_) => LiteralType.BoolType
    case IntValue(_) => LiteralType.IntType
    case FloatValue(_) => LiteralType.FloatType
    case RuneValue(_) => LiteralType.RuneType
    case StringValue(_) => LiteralType.StringType
  }

  def toTerm: Term = Term.Primitive(this)
  def toValue: Value = Value.Primitive(this)
}

object Literal {
  def unit: Literal = UnitValue
  def trueValue: Literal = BoolValue(true)
  def falseValue: Literal = BoolValue(false)
}

enum LiteralType {

  case NothingType
  case UnitType
  case BoolType
  case IntType
  case FloatType
  case RuneType
  case StringType

  override def toString: String = this match {
    case NothingType => "Nothing"
    case UnitType => "Unit"
    case BoolType => "Bool"
    case IntType => "Int"
    case FloatType => "Float"
    case RuneType => "Char"
    case StringType => "String"
  }

  def toTerm: Term = Term.PrimitiveType(this)
  def toValue: Value = Value.PrimitiveType(this)
}
