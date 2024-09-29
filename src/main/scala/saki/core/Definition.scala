package saki.core

import saki.core
import saki.core.pattern.{Clause, Pattern, UnresolvedClause}

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

enum Definition(
  val ident: Var.Defined[? <: Definition],
  val signature: Signature,
) extends FnLike[Type] {
  
  case Function(
    override val ident: Var.Defined[Function],
    override val signature: Signature,
    params: ParamList[Term],
    override val resultType: Type,
    body: Either[Term, Seq[Clause[Term]]],
  ) extends Definition(ident, signature)

  case Inductive(
    override val ident: Var.Defined[Inductive],
    override val signature: Signature,
    params: ParamList[Term],
    constructors: Seq[Constructor],
  ) extends Definition(ident, signature)

  case Constructor(
    override val ident: Var.Defined[Constructor],
    override val signature: Signature,
    owner: Var.Defined[Inductive],
    params: ParamList[Term],
  ) extends Definition(ident, signature)

  case Contract(
    override val ident: Var.Defined[Contract],
    override val signature: Signature,
    params: ParamList[Term],
    override val resultType: Type,
  ) extends Definition(ident, signature)

  def resultType: Type = this match {
    case Function(_, _, _, resultType, _) => resultType
    case Inductive(_, _, _, _) => Term.unitType
    case Constructor(_, _, owner, _) => Term.InductiveCall(owner, owner.definition.get.arguments.refs)
    case Contract(_, _, _, resultType) => resultType
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
    constructors: Seq[Constructor],
  ) extends PristineDefinition(ident, params)

  case Constructor(
    override val ident: Var.Defined[Definition.Constructor],
    owner: PristineDefinition.Constructor,
    override val params: ParamList[Expr],
  ) extends PristineDefinition(ident, params)
}

object PristineDefinition {

  private type CoreExpr = core.Expr

  enum FunctionBody {
    case Expr(body: CoreExpr)
    case Clauses(clauses: Seq[Clause[CoreExpr]])
    case UnresolvedClauses(clauses: Seq[UnresolvedClause])
  }
}
