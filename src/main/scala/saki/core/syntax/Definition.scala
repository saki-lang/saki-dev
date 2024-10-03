package saki.core.syntax

import saki.core.typing.Elaborate.Context
import saki.core.typing.{Elaborate, Resolve}
import saki.util.LateInit

import scala.collection.Seq

case class Param[Type](
  ident: Var.Local,
  `type`: Type,
  applyMode: ApplyMode = ApplyMode.Explicit,
) {
  def name: String = ident.name
  def map[NewType](transform: Type => NewType): Param[NewType] = {
    copy(`type` = transform(`type`))
  }
}

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

type ParamList[T] = Seq[Param[T]]

case class Argument[Value](
  value: Value,
  applyMode: ApplyMode = ApplyMode.Explicit,
) {
  def map[NewValue](transform: Value => NewValue): Argument[NewValue] = {
    copy(value = transform(value))
  }
}

type ArgList[Value] = Seq[Argument[Value]]

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
    val body: LateInit[Term] = LateInit[Term](),
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
    case Inductive(_, _, _, _) => Term.Universe
    case Constructor(_, _, owner, _) => Term.InductiveCall(owner, owner.definition.get.arguments.refs)
    case Contract(_, _, _, resultType) => resultType
  }

  override def toString: String = this match {
    case Function(ident, signature, params, resultType, body) => {
      val paramsStr = params.map(_.toString).mkString("(", ", ", ")")
      s"def $ident$paramsStr: $resultType = $body"
    }
    case Inductive(ident, signature, params, constructors) => {
      val paramsStr = params.map(_.toString).mkString("(", ", ", ")")
      val constructorsStr = constructors.map(_.toString).mkString("\n")
      s"inductive $ident$paramsStr\n$constructorsStr"
    }
    case Constructor(ident, signature, owner, params) => {
      val paramsStr = params.map(_.toString).mkString("(", ", ", ")")
      s"constructor $ident$paramsStr"
    }
    case Contract(ident, signature, params, resultType) => {
      val paramsStr = params.map(_.toString).mkString("(", ", ", ")")
      s"contract $ident$paramsStr: $resultType"
    }
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
    body: Expr,
  ) extends PristineDefinition(ident, params)

  case Inductive(
    override val ident: Var.Defined[Definition.Inductive],
    override val params: ParamList[Expr],
    constructors: Seq[Constructor],
  ) extends PristineDefinition(ident, params)

  case Constructor(
    override val ident: Var.Defined[Definition.Constructor],
    owner: PristineDefinition.Inductive,
    override val params: ParamList[Expr],
  ) extends PristineDefinition(ident, params)

  def resolve(implicit ctx: Resolve.Context): (PristineDefinition, Resolve.Context) = {
    Resolve.resolvePristineDefinition(this)
  }

  def synth(implicit ctx: Context): Definition = Elaborate.synthDefinition(this)

  override def toString: String = this match {
    case Function(ident, params, resultType, body) => {
      val paramsStr = params.map(_.toString).mkString("(", ", ", ")")
      s"Function $ident$paramsStr: $resultType = $body"
    }
    case Inductive(ident, params, constructors) => {
      val paramsStr = params.map(_.toString).mkString("(", ", ", ")")
      val constructorsStr = constructors.map(_.toString).mkString("\n")
      s"Inductive $ident$paramsStr\n$constructorsStr"
    }
    case Constructor(ident, _, params) => {
      val paramsStr = params.map(_.toString).mkString("(", ", ", ")")
      s"Constructor $ident$paramsStr"
    }
  }
}
