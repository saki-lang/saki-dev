package saki.concrete.syntax

import org.antlr.v4.runtime.ParserRuleContext
import saki.concrete.span
import saki.core.syntax.{
  DefinedFunction, Expr, Param, Var,
  Constructor as CoreConstructor,
  Definition as CoreDefinition,
  Function as CoreFunction,
  Inductive as CoreInductive
}
import saki.util.{unreachable, LateInit, SourceSpan}

import scala.collection.{mutable, Seq}

enum Definition(implicit ctx: ParserRuleContext) extends SyntaxTree[CoreDefinition[Expr]] {

  def ident: String

  override def span: SourceSpan = ctx.span

  case Function(
    override val ident: String,
    params: Seq[Param[ExprTree]],
    resultType: ExprTree,
    body: ExprTree,
  )(implicit ctx: ParserRuleContext)

  case Inductive(
    override val ident: String,
    params: Seq[Param[ExprTree]],
    constructors: Seq[Definition.Constructor],
  )(implicit ctx: ParserRuleContext)

  override def emit: CoreDefinition[Expr] = this match {

    case Function(name, paramsExpr, resultType, body) => {
      val params = paramsExpr.map(param => param.map(_.emit))
      val ident: Var.Defined[Expr, CoreFunction] = Var.Defined(name)
      DefinedFunction(ident, params, resultType.emit, true, LateInit(body.emit))
    }

    case Inductive(name, paramsExpr, constructorsExpr) => {
      val constructors: mutable.ListBuffer[CoreConstructor[Expr]] = mutable.ListBuffer()
      val ident: Var.Defined[Expr, CoreInductive] = Var.Defined(name)
      val params = paramsExpr.map(param => param.map(_.emit))
      val inductive: CoreInductive[Expr] = CoreInductive(ident, params, constructors)
      constructors ++= constructorsExpr.map {
        case Definition.Constructor(name, params) => {
          val resolvedParams = params.map(param => param.map(_.emit))
          CoreConstructor(name, inductive.ident, resolvedParams)
        }
      }
      return inductive
    }
  }

}

object Definition {
  case class Constructor(
    ident: String,
    params: Seq[Param[ExprTree]],
  )(implicit ctx: ParserRuleContext) extends SyntaxTree[Constructor] {

    override def span: SourceSpan = ctx.span

    @deprecated
    override def emit: Constructor = {
      unreachable("Constructor should only be emitted in the context of an inductive definition")
    }
  }
}
