package saki.core

enum Literal {
  case UnitValue
  case BoolValue(value: Boolean)
  case IntValue(value: Int)
  case FloatValue(value: Float)
  case CharValue(value: Char)
  case StringValue(value: String)

  override def toString: String = this match {
    case UnitValue => "unit"
    case BoolValue(value) => value.toString
    case IntValue(value) => value.toString + 'i'
    case FloatValue(value) => value.toString + 'f'
    case CharValue(value) => s"'${value.toString}'"
    case StringValue(value) => s""""$value""""
  }

  def ty: LiteralType = this match {
    case UnitValue => LiteralType.UnitType
    case BoolValue(_) => LiteralType.BoolType
    case IntValue(_) => LiteralType.IntType
    case FloatValue(_) => LiteralType.FloatType
    case CharValue(_) => LiteralType.CharType
    case StringValue(_) => LiteralType.StringType
  }
}

enum LiteralType {
  case NothingType
  case UnitType
  case BoolType
  case IntType
  case FloatType
  case CharType
  case StringType
}
