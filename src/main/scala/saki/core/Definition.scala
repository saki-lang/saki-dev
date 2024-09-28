package saki.core

import saki.core
import saki.core.pattern.{ClauseSet, Pattern, UnresolvedClause}

case class Param[Type](ident: Var.Local, `type`: Type) {
  def name: String = ident.name
}

type ParamList[T] = Seq[Param[T]]

trait FnLike[T] {
  def params: ParamList[T]
  def resultType: T
  def arguments: Seq[Var.Local] = params.map(_.ident)
}

extension (arguments: Seq[Var.Local]) {
  def refs: Seq[Term.Ref] = arguments.map(Term.Ref.apply)
}

case class Signature(params: ParamList[Term], resultType: Type) extends FnLike[Term]

enum Definition(val ident: Var.Defined[? <: Definition]) extends FnLike[Type] {
  case Function(
    override val ident: Var.Defined[Function],
    params: ParamList[Term],
    override val resultType: Type,
    body: Either[Term, ClauseSet[Term]],
  ) extends Definition(ident)

  case Inductive(
    override val ident: Var.Defined[Inductive],
    params: ParamList[Term],
    constructors: Seq[Constructor],
  ) extends Definition(ident)

  case Constructor(
    override val ident: Var.Defined[Constructor],
    owner: Var.Defined[Inductive],
    params: ParamList[Term],
  ) extends Definition(ident)

  case Contract(
    override val ident: Var.Defined[Contract],
    params: ParamList[Term],
    override val resultType: Type,
  ) extends Definition(ident)

  def resultType: Type = this match {
    case Function(_, _, resultType, _) => resultType
    case Inductive(_, _, _) => Term.unitType
    case Constructor(_, owner, _) => Term.InductiveCall(owner, owner.definition.get.arguments.refs)
    case Contract(_, _, resultType) => resultType
  }
}

enum PristineDefinition(
  val ident: Var.Defined[? <: Definition], 
  val params: ParamList[Expr]
) {
  case Function(
    override val ident: Var.Defined[Definition.Function],
    override val params: ParamList[Expr],
    resultType: Expr,
    body: PristineDefinition.FunctionBody,
  ) extends PristineDefinition(ident, params)

  case Inductive(
    override val ident: Var.Defined[Definition.Inductive],
    override val params: ParamList[Expr],
  ) extends PristineDefinition(ident, params)

  case Constructor(
    override val ident: Var.Defined[Definition.Constructor],
    owner: Var.Defined[Definition.Inductive],
    override val params: ParamList[Expr],
  ) extends PristineDefinition(ident, params)
}

object PristineDefinition {

  private type CoreExpr = core.Expr

  enum FunctionBody {
    case Expr(body: CoreExpr)
    case Clauses(clauses: ClauseSet[CoreExpr])
    case UnresolvedClauses(clauses: Seq[UnresolvedClause])
  }
}
