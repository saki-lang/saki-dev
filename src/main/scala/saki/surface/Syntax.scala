package saki.surface

import org.antlr.v4.runtime.ParserRuleContext
import util.*

enum LiteralValue {
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

enum Term(val span: SourceSpan) {

  case Variable(name: String)(implicit ctx: ParserRuleContext) extends Term(ctx.span)
  case PrimitiveValue(value: LiteralValue)(implicit ctx: ParserRuleContext) extends Term(ctx.span)
  case Application(function: Term, applyMode: ApplyMode, argument: Term)(implicit ctx: ParserRuleContext) extends Term(ctx.span)
  case Projection(record: Term, fieldName: String)(implicit ctx: ParserRuleContext) extends Term(ctx.span)
  case Function(param: BoundVariable, applyMode: ApplyMode, body: Term, returnType: Option[Term])(implicit ctx: ParserRuleContext) extends Term(ctx.span)
  case FunctionType(paramType: Term, applyMode: ApplyMode, returnType: Term)(implicit ctx: ParserRuleContext) extends Term(ctx.span)
  case CodeBlock(statements: Seq[Term])(implicit ctx: ParserRuleContext) extends Term(ctx.span)
  case Let(name: String, `type`: Option[Term], value: Term)(implicit ctx: ParserRuleContext) extends Term(ctx.span)
  case If(condition: Term, thenBranch: Term, elseBranch: Option[Term])(implicit ctx: ParserRuleContext) extends Term(ctx.span)
  case Match(scrutinee: Term, cases: Seq[MatchCase])(implicit ctx: ParserRuleContext) extends Term(ctx.span)
  case SumType(cases: Seq[Term])(implicit ctx: ParserRuleContext) extends Term(ctx.span)
  case RecordType(fields: Seq[(String, Term)])(implicit ctx: ParserRuleContext) extends Term(ctx.span)
  case RecordValue(fields: Seq[(String, Term)])(implicit ctx: ParserRuleContext) extends Term(ctx.span)
  case Universe(level: Int)(implicit ctx: ParserRuleContext) extends Term(ctx.span)

  // To s-expression (lisp-like language)
  override def toString: String = this match {
    case Variable(name) => s"(var $span $name)"
    case Term.PrimitiveValue(value) => s"(prim $span $value)"
    case Application(function, applyMode, argument) => s"(apply $span $function $applyMode $argument)"
    case Projection(record, fieldName) => s"(proj $span $record $fieldName)"
    case Function(param, applyMode, body, returnType) => {
      val paramString = s"(bind ${param.name} ${param.`type`})"
      val returnTypeStr = returnType.map(_.toString).getOrElse("undef")
      s"(val-fun $span $paramString $applyMode $body $returnTypeStr)"
    }
    case FunctionType(paramType, applyMode, returnType) => {
      s"(type-fun $span $paramType $applyMode $returnType)"
    }
    case CodeBlock(statements) => s"(block $span ${statements.mkString(" ")})"
    case Let(name, ty, value) => ty match {
      case Some(ty) => s"(let $span $name $ty $value)"
      case None => s"(let $span $name undef $value)"
    }
    case If(condition, thenBranch, elseBranch) => elseBranch match {
      case Some(elseBranch) => s"(if $span $condition $thenBranch $elseBranch)"
      case None => s"(if $span $condition $thenBranch)"
    }
    case Match(scrutinee, cases) => s"(match $span $scrutinee ${cases.mkString(" ")})"
    case SumType(cases) => s"(type-sum $span ${cases.mkString(" ")})"
    case RecordType(fields) => {
      val fieldsExpr = fields.map((fieldName, `type`) => s"(type-field $fieldName ${`type`})")
      s"(type-record $span ${fieldsExpr.mkString(" ")})"
    }
    case RecordValue(fields) => {
      val fieldsExpr = fields.map((fieldName, value) => s"(val-field $fieldName $value)")
      s"(val-record $span ${fieldsExpr.mkString(" ")})"
    }
    case Universe(level) => s"(universe $span $level)"
  }
}

class MatchCase private(pattern: Pattern, `type`: Option[Term], body: Term, span: SourceSpan) {
  override def toString: String = {
    `type` match {
      case Some(ty) => s"(case $span $pattern ${`type`} $body)"
      case None => s"(case $span $pattern undef $body)"
    }
  }
}

object MatchCase {
  def apply(
    pattern: Pattern, `type`: Option[Term], body: Term
  )(implicit ctx: ParserRuleContext): MatchCase = pattern match {
    case Pattern.Record(fields, ty) => new MatchCase(pattern, ty, body, ctx.span)
    case _ => new MatchCase(pattern, `type`, body, ctx.span)
  }
}

enum Pattern(val span: SourceSpan) {
  case Literal(value: LiteralValue)(implicit ctx: ParserRuleContext) extends Pattern(ctx.span)
  case Variable(name: String)(implicit ctx: ParserRuleContext) extends Pattern(ctx.span)
  case Variant(name: String, pattern: Seq[Pattern])(implicit ctx: ParserRuleContext) extends Pattern(ctx.span)
  case Record(fields: Seq[(String, Pattern)], `type`: Option[Term])(implicit ctx: ParserRuleContext) extends Pattern(ctx.span)

  override def toString: String = this match {
    case Literal(value) => s"(pattern-literal $span $value)"
    case Variable(name) => s"(pattern-var $span $name)"
    case Variant(name, patterns) => s"(pattern-variant $span $name ${patterns.mkString(" ")})"
    case Record(fields, ty) => {
      val fieldsExpr = fields.map((fieldName, pattern) => s"(pattern-field $fieldName $pattern)")
      s"(pattern-record $span ${fieldsExpr.mkString(" ")})"
    }
  }
}
