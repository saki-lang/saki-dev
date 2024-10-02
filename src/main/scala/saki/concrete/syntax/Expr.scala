package saki.concrete.syntax

import org.antlr.v4.runtime.ParserRuleContext
import saki.concrete.span
import saki.core.{
  ApplyMode, LiteralType,
  Param, SourceSpan,
  Expr as CoreExpr
}
import saki.core.syntax.*

import scala.collection.Seq

enum Expr(implicit ctx: ParserRuleContext) extends SyntaxTree[CoreExpr] {

  override def span: SourceSpan = ctx.span

  case Universe()(implicit ctx: ParserRuleContext)

  case Variable(
    name: String,
  )(implicit ctx: ParserRuleContext)

  case PrimitiveValue(
    value: Literal,
  )(implicit ctx: ParserRuleContext)

  case PrimitiveType(
    `type`: LiteralType,
  )(implicit ctx: ParserRuleContext)

  case Elimination(
    value: Expr,
    member: String,
  )(implicit ctx: ParserRuleContext)

  case FunctionCall(
    function: Expr,
    arguments: Seq[Argument[Expr]],
  )(implicit ctx: ParserRuleContext)

  case Constructor(
    inductive: String,
    inductiveArguments: Seq[Argument[Expr]],
    constructor: String,
  )(implicit ctx: ParserRuleContext)

  case Lambda(
    param: Param[Option[Expr]],
    body: Expr,
    returnType: Option[Expr],
  )(implicit ctx: ParserRuleContext)

  case Pi(
    param: Param[Expr],
    codomain: Expr,
  )(implicit ctx: ParserRuleContext)

  case Sigma(
    param: Param[Expr],
    codomain: Expr,
  )(implicit ctx: ParserRuleContext)

  case CodeBlock(
    statements: Seq[Statement]
  )(implicit ctx: ParserRuleContext)

  case If(
    condition: Expr,
    thenBranch: Expr,
    elseBranch: Option[Expr],
  )(implicit ctx: ParserRuleContext)

  case Match(
    scrutinees: Seq[Expr],
    cases: Seq[Clause[Expr]],
  )(implicit ctx: ParserRuleContext)

  case RecordType(
    fields: Seq[(String, Expr)]
  )(implicit ctx: ParserRuleContext)

  case RecordValue(
    fields: Seq[(String, Expr)],
    `type`: Expr,
  )(implicit ctx: ParserRuleContext)

  given SourceSpan = ctx.span

  override def emit: CoreExpr = this match {

    case Universe() => CoreExpr.Universe()
    case Variable(name) => CoreExpr.Unresolved(name)
    case PrimitiveValue(value) => CoreExpr.Primitive(value)
    case PrimitiveType(ty) => CoreExpr.PrimitiveType(ty)

    case Lambda(paramExpr, bodyExpr, returnTypeExpr) => {
      val param: Param[Option[CoreExpr]] = paramExpr.map(param => param.map(_.emit))
      val body: CoreExpr = bodyExpr.emit
      val returnType: Option[CoreExpr] = returnTypeExpr.map(_.emit)
      CoreExpr.Lambda(param, body, returnType)
    }

    case Pi(param, codomain) => CoreExpr.Pi(param.map(_.emit), codomain.emit)

    case Sigma(param, codomain) => CoreExpr.Sigma(param.map(_.emit), codomain.emit)

    case CodeBlock(statements) => {
      val returnValue = statements.last match {
        case Statement.Let(_, _, _) => CoreExpr.unit
        case Statement.Expression(expr) => expr.emit
      }
      statements.init.foldRight(returnValue) {
        case (statement, body) => statement match {
          case Statement.Let(name, ty, value) => {
            val bodyLambda = CoreExpr.Lambda(Param(Var.Local(name), ty.map(_.emit)), body)
            CoreExpr.Apply(bodyLambda, Argument(value.emit))
          }
          case Statement.Expression(expr) => expr.emit
        }
      }
    }

    case If(condition, thenBranch, elseBranch) => {
      Expr.Match(
        scrutinees = Seq(condition),
        cases = Seq(
          Clause(
            patterns = Seq(Pattern.Primitive(Literal.trueValue)),
            body = thenBranch,
          ),
          Clause(
            patterns = Seq(Pattern.Primitive(Literal.falseValue)),
            body = elseBranch.getOrElse(Expr.PrimitiveValue(Literal.unit)),
          ),
        ),
      ).emit
    }

    case Match(scrutinees, cases) => CoreExpr.Match(
      scrutinees = scrutinees.map(_.emit),
      clauses = cases.map(clause => clause.map(_.emit)),
    )

    case Elimination(value, member) => CoreExpr.Elimination(value.emit, member)

    case FunctionCall(function, arguments) => {
      val args = arguments.map { case Argument(arg, mode) => Argument(arg.emit, mode) }
      args.foldLeft(function.emit) { case (fn, arg) => CoreExpr.Apply(fn, arg) }
    }

    case Constructor(inductive, inductiveArguments, constructor) => {
      val inductiveArgs = inductiveArguments.map(arg => arg.map(_.emit))
      inductiveArgs.foldLeft[CoreExpr](CoreExpr.Unresolved(s"$inductive::$constructor")) {
        case (fn, arg) => CoreExpr.Apply(fn, arg)
      }
    }

    case RecordType(fields) => CoreExpr.RecordType(
      fields.map { (name, ty) => (name, ty.emit) }.toMap
    )

    case RecordValue(fields, ty) => CoreExpr.Record(
      fields.map { (name, value) => (name, value.emit) }.toMap
    )
  }

}
