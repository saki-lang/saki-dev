package saki.concrete.syntax

import org.antlr.v4.runtime.ParserRuleContext
import saki.core
import saki.core.syntax.Param
import saki.core.syntax.Definition as CoreDefinition
import saki.core.syntax.PristineDefinition as CorePristineDefinition
import saki.util.{SourceSpan, span, unreachable}

enum Definition(implicit ctx: ParserRuleContext) extends SyntaxTree[CorePristineDefinition] {

  def ident: String

  override def span: SourceSpan = ctx.span

  case Function(
    override val ident: String,
    params: Seq[Param[Expr]],
    body: Expr,
  )(implicit ctx: ParserRuleContext)

  case Inductive(
    override val ident: String,
    constructors: Seq[Constructor],
  )(implicit ctx: ParserRuleContext)

  override def emit: CorePristineDefinition = ???

}

case class Constructor(
  ident: String,
  params: Seq[Param[Expr]],
)(implicit ctx: ParserRuleContext) extends SyntaxTree[CorePristineDefinition.Constructor] {

  override def span: SourceSpan = ctx.span

  @deprecated
  override def emit: CorePristineDefinition.Constructor = {
    unreachable("Constructor should only be emitted in the context of an inductive definition")
  }
}
