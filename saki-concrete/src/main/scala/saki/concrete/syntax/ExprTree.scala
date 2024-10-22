package saki.concrete.syntax

import org.antlr.v4.runtime.ParserRuleContext
import saki.concrete.span
import saki.core.{Entity, LiteralType, Param, Expr as CoreExpr}
import saki.core.syntax.*
import saki.error.CoreErrorKind
import saki.error.CoreErrorKind.UnsupportedFeature
import saki.util.SourceSpan

import scala.collection.Seq

enum ExprTree(implicit ctx: ParserRuleContext) extends SyntaxTree[CoreExpr] with Entity {

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

  case TypeOf(
    value: ExprTree,
  )(implicit ctx: ParserRuleContext)

  case Elimination(
    value: ExprTree,
    member: String,
  )(implicit ctx: ParserRuleContext)

  case FunctionCall(
    function: ExprTree,
    arguments: Seq[Argument[ExprTree]],
  )(implicit ctx: ParserRuleContext)

  case Constructor(
    inductive: ExprTree,
    constructor: String,
  )(implicit ctx: ParserRuleContext)

  case Lambda(
    param: Param[Option[ExprTree]],
    body: ExprTree,
    returnType: Option[ExprTree],
  )(implicit ctx: ParserRuleContext)

  case Pi(
    param: Param[ExprTree],
    codomain: ExprTree,
  )(implicit ctx: ParserRuleContext)

  case Sigma(
    param: Param[ExprTree],
    codomain: ExprTree,
  )(implicit ctx: ParserRuleContext)

  case CodeBlock(
    statements: Seq[Statement]
  )(implicit ctx: ParserRuleContext)

  case If(
    condition: ExprTree,
    thenBranch: ExprTree,
    elseBranch: Option[ExprTree],
  )(implicit ctx: ParserRuleContext)

  case Match(
    scrutinees: Seq[ExprTree],
    cases: Seq[Clause[ExprTree]],
  )(implicit ctx: ParserRuleContext)

  case RecordType(
    fields: Seq[(String, ExprTree)]
  )(implicit ctx: ParserRuleContext)

  case RecordValue(
    fields: Seq[(String, ExprTree)],
    `type`: ExprTree,
  )(implicit ctx: ParserRuleContext)

  given SourceSpan = ctx.span

  override def emit: CoreExpr = this match {

    case Universe() => CoreExpr.Universe()
    case Variable(name) => CoreExpr.Unresolved(name)
    case PrimitiveValue(value) => CoreExpr.Primitive(value)
    case PrimitiveType(ty) => CoreExpr.PrimitiveType(ty)

    case TypeOf(value) => CoreExpr.TypeOf(value.emit)

    case Lambda(paramExpr, bodyExpr, returnTypeExpr) => {
      // TODO: optional-param lambda
      val param: Param[CoreExpr] = paramExpr.map { param =>
        param.map(_.emit).getOrElse {
            UnsupportedFeature.raise(ctx.span) {
              "Optional typed parameter in lambda is not supported"
            }
        }
      }
      val body: CoreExpr = bodyExpr.emit
      val returnType: Option[CoreExpr] = returnTypeExpr.map(_.emit)
      CoreExpr.Lambda(param, body, returnType)
    }

    case Pi(param, codomain) => CoreExpr.Pi(param.map(_.emit), codomain.emit)

    case Sigma(param, codomain) => CoreExpr.Sigma(param.map(_.emit), codomain.emit)

    case CodeBlock(statements) => {
      if (statements.isEmpty) {
        return CoreExpr.Primitive(Literal.unit)
      }
      val returnValue = statements.last match {
        case Statement.Let(_, _, _) => CoreExpr.Primitive(Literal.unit)
        case Statement.Expression(expr) => expr.emit
      }
      statements.init.foldRight(returnValue) {
        case (statement, body) => statement match {
          case Statement.Let(name, ty, value) => {
            val valueCoreExpr = value.emit
            val typeCoreExpr = ty match {
              case Some(ty) => ty.emit
              case None => CoreExpr.TypeOf(valueCoreExpr)
            }
            val bodyLambda = CoreExpr.Lambda(Param(Var.Local(name), typeCoreExpr), body)
            CoreExpr.Apply(bodyLambda, Argument(valueCoreExpr))
          }
          case Statement.Expression(expr) => expr.emit
        }
      }
    }

    case If(condition, thenBranch, elseBranch) => {
      ExprTree.Match(
        scrutinees = Seq(condition),
        cases = Seq(
          Clause(
            patterns = Seq(Pattern.Primitive(Literal.trueValue)),
            body = thenBranch,
          ),
          Clause(
            patterns = Seq(Pattern.Primitive(Literal.falseValue)),
            body = elseBranch.getOrElse(ExprTree.PrimitiveValue(Literal.unit)),
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

    case Constructor(inductive, constructor) => {
      CoreExpr.Constructor(inductive.emit, constructor)
    }

    case RecordType(fields) => CoreExpr.RecordType(
      fields.map { (name, ty) => (name, ty.emit) }.toMap
    )

    case RecordValue(fields, ty) => CoreExpr.Record(
      fields.map { (name, value) => (name, value.emit) }.toMap,
      Some(ty.emit),
    )
  }

}
