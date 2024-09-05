package saki.syntax

import saki._

enum PrimitiveValue {
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
}

case class BoundVariable(name: String, `type`: Term)

enum ApplyMode {
  case Explicit
  case Implicit
  case Instance
  override def toString: String = this match {
    case Explicit => "explicit"
    case Implicit => "implicit"
    case Instance => "instance"
  }
}

enum Term {
  case Variable(name: String)
  case PrimitiveValue(value: syntax.PrimitiveValue)
  case Application(function: Term, argument: Term)
  case FieldProjection(record: Term, fieldName: String)
  case Function(param: BoundVariable, applyMode: ApplyMode, body: Term, returnType: Option[Term])
  case FunctionType(paramType: Term, applyMode: ApplyMode, returnType: Term)
  case CodeBlock(statements: List[Term])
  case Let(name: String, `type`: Option[Term], value: Term)
  case If(condition: Term, thenBranch: Term, elseBranch: Option[Term])
  case Match(scrutinee: Term, cases: List[MatchCase])
  case SumType(cases: List[Term])
  case RecordType(fields: List[(String, Term)])
  case RecordValue(fields: List[(String, Term)])
  case Universe(level: Int)

  // To s-expression (lisp-like language)
  override def toString: String = this match {
    case Variable(name) => name
    case Term.PrimitiveValue(value) => value.toString
    case Application(function, argument) => s"(apply $function $argument)"
    case FieldProjection(record, fieldName) => s"(proj $record $fieldName)"
    case Function(param, applyMode, body, returnType) => {
      val paramString = s"(bind ${param.name} ${param.`type`})"
      val returnTypeStr = returnType.map(_.toString).getOrElse("undef")
      s"(val-fun $paramString $applyMode $body $returnTypeStr)"
    }
    case FunctionType(paramType, applyMode, returnType) => {
      s"(type-fun $paramType $applyMode $returnType)"
    }
    case CodeBlock(statements) => s"(block ${statements.mkString(" ")})"
    case Let(name, ty, value) => ty match {
      case Some(ty) => s"(let $name $ty $value)"
      case None => s"(let $name undef $value)"
    }
    case If(condition, thenBranch, elseBranch) => elseBranch match {
      case Some(elseBranch) => s"(if $condition $thenBranch $elseBranch)"
      case None => s"(if $condition $thenBranch)"
    }
    case Match(scrutinee, cases) => s"(match $scrutinee ${cases.mkString(" ")})"
    case SumType(cases) => s"(type-sum ${cases.mkString(" ")})"
    case RecordType(fields) => {
      val fieldsExpr = fields.map((fieldName, `type`) => s"(type-field $fieldName ${`type`})")
      s"(type-record ${fieldsExpr.mkString(" ")})"
    }
    case RecordValue(fields) => {
      val fieldsExpr = fields.map((fieldName, value) => s"(val-field $fieldName $value)")
      s"(val-record ${fieldsExpr.mkString(" ")})"
    }
    case Universe(level) => s"(universe $level)"
  }
}

class MatchCase private(pattern: Pattern, `type`: Option[Term], body: Term) {
  override def toString: String = {
    `type` match {
      case Some(ty) => s"(case $pattern ${`type`} $body)"
      case None => s"(case $pattern undef $body)"
    }
  }
}

object MatchCase {
  def apply(pattern: Pattern, `type`: Option[Term], body: Term): MatchCase = pattern match {
    case Pattern.Record(fields, ty) => new MatchCase(pattern, ty, body)
    case _ => new MatchCase(pattern, `type`, body)
  }
}

enum Pattern {
  case Literal(value: PrimitiveValue)
  case Variable(name: String)
  case Variant(name: String, pattern: List[Pattern])
  case Record(fields: List[(String, Pattern)], `type`: Option[Term])

  override def toString: String = this match {
    case Literal(value) => s"(pattern-literal $value)"
    case Variable(name) => s"(pattern-var $name)"
    case Variant(name, patterns) => s"(pattern-variant $name ${patterns.mkString(" ")})"
    case Record(fields, ty) => {
      val fieldsExpr = fields.map((fieldName, pattern) => s"(pattern-field $fieldName $pattern)")
      s"(pattern-record ${fieldsExpr.mkString(" ")})"
    }
  }
}
