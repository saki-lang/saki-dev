package saki.concrete.syntax

import org.antlr.v4.runtime.ParserRuleContext
import saki.util.{SourceSpan, span, unreachable}

enum Statement(implicit ctx: ParserRuleContext) extends SyntaxTree[Nothing] {

  override def span: SourceSpan = ctx.span

  given ParserRuleContext = ctx

  case Let(
    name: String,
    `type`: Option[Expr],
    value: Expr,
  )(implicit ctx: ParserRuleContext)

  case Expression(
    expr: Expr,
  )(implicit ctx: ParserRuleContext)

  @deprecated
  override def emit: Nothing = {
    unreachable("Statements should be emitted in the context of a code block")
  }
}
