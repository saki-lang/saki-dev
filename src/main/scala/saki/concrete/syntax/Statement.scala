package saki.concrete.syntax

import org.antlr.v4.runtime.ParserRuleContext
import saki.concrete.span
import saki.core.SourceSpan
import saki.util.unreachable

enum Statement(implicit ctx: ParserRuleContext) extends SyntaxTree[Nothing] {

  override def span: SourceSpan = ctx.span

  given ParserRuleContext = ctx

  case Let(
    name: String,
    `type`: Option[ExprTree],
    value: ExprTree,
  )(implicit ctx: ParserRuleContext)

  case Expression(
    expr: ExprTree,
  )(implicit ctx: ParserRuleContext)

  @deprecated
  override def emit: Nothing = {
    unreachable("Statements should be emitted in the context of a code block")
  }
}
