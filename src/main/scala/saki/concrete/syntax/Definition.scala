package saki.concrete.syntax

import org.antlr.v4.runtime.ParserRuleContext
import saki.concrete.span
import saki.concrete.syntax.Definition.Constructor
import saki.core.SourceSpan
import saki.core.syntax.{Param, Var, Definition as CoreDefinition, PristineDefinition as CorePristineDefinition}
import saki.util.unreachable

import scala.collection.mutable
import scala.collection.Seq

enum Definition(implicit ctx: ParserRuleContext) extends SyntaxTree[CorePristineDefinition] {

  def ident: String

  override def span: SourceSpan = ctx.span

  case Function(
    override val ident: String,
    params: Seq[Param[Expr]],
    resultType: Expr,
    body: Expr,
  )(implicit ctx: ParserRuleContext)

  case Inductive(
    override val ident: String,
    params: Seq[Param[Expr]],
    constructors: Seq[Constructor],
  )(implicit ctx: ParserRuleContext)

  override def emit: CorePristineDefinition = this match {

    case Function(name, paramsExpr, resultType, body) => {
      val params = paramsExpr.map(param => param.map(_.emit))
      val ident: Var.Defined[CoreDefinition.Function] = Var.Defined(name)
      CorePristineDefinition.Function(ident, params, resultType.emit, body.emit)
    }

    case Inductive(name, paramsExpr, constructorsExpr) => {
      val constructors: mutable.ListBuffer[CorePristineDefinition.Constructor] = mutable.ListBuffer()
      val ident: Var.Defined[CoreDefinition.Inductive] = Var.Defined[CoreDefinition.Inductive](name)
      val params = paramsExpr.map(param => param.map(_.emit))
      val inductive: CorePristineDefinition.Inductive = {
        CorePristineDefinition.Inductive(ident, params, constructors)
      }
      constructors ++= constructorsExpr.map {
        case Constructor(name, params) => {
          val ident: Var.Defined[CoreDefinition.Constructor] = Var.Defined(name)
          val resolvedParams = params.map(param => param.map(_.emit))
          CorePristineDefinition.Constructor(ident, inductive, resolvedParams)
        }
      }
      return inductive
    }
  }

}

object Definition {
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
}