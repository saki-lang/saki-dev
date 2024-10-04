package saki.core.syntax

import saki.core.{Entity, EntityFactory}
import saki.util.LateInit

import scala.collection.Seq

case class Param[T <: Entity | Option[Entity]](
  ident: Var.Local, `type`: T,
  applyMode: ApplyMode = ApplyMode.Explicit,
) {
  def name: String = ident.name
  def map[U <: Entity | Option[Entity]](transform: T => U): Param[U] = {
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

type ParamList[T <: Entity | Option[Entity]] = Seq[Param[T]]

case class Argument[T <: Entity](
  value: T,
  applyMode: ApplyMode = ApplyMode.Explicit,
) {
  def map[U <: Entity](transform: T => U): Argument[U] = {
    copy(value = transform(value))
  }
}

case class Signature[T <: Entity](params: ParamList[T], resultType: T) extends FnLike[T] {
  def map[U <: Entity](transform: T => U): Signature[U] = {
    copy(params = params.map(_.map(transform)), resultType = transform(resultType))
  }
}

type ArgList[T <: Entity] = Seq[Argument[T]]

trait FnLike[T <: Entity] {
  def params: ParamList[T]
  def paramToVars(implicit factory: EntityFactory[T]): Seq[T] = params.map(_.ident).map(factory.variable)
  def arguments[T1 <: Entity](argValues: Seq[T1]): Seq[(Var.Local, T1)] = params.map(_.ident).zip(argValues)
}

sealed trait Definition[T <: Entity] extends FnLike[T] {
  def ident: Var.Defined[T, ?]
  def resultType(implicit ev: EntityFactory[T]): T
  def signature(implicit ev: EntityFactory[T]): Signature[T] = Signature(params, resultType)
}

case class Function[T <: Entity](
  override val ident: Var.Defined[T, Function],
  override val params: ParamList[T],
  val resultType: T,
  // Mark this function as a neutral function (recursive)
  // When a function is a neutral function, its invocation will not be evaluated instantly 
  // unless all its arguments are pure values
  val isNeutral: Boolean = true,
  val body: LateInit[T] = LateInit[T](),
) extends Definition[T] {
  def resultType(implicit ev: EntityFactory[T]): T = resultType
}

case class Inductive[T <: Entity](
  override val ident: Var.Defined[T, Inductive],
  override val params: ParamList[T],
  constructors: Seq[Constructor[T]],
) extends Definition[T] {
  def resultType(implicit ev: EntityFactory[T]): T = ev.universe
}

case class Constructor[T <: Entity](
  override val ident: Var.Defined[T, Constructor],
  owner: Var.Defined[T, Inductive],
  override val params: ParamList[T],
) extends Definition[T] {
  def resultType(implicit factory: EntityFactory[T]): T = {
    factory.inductiveType(owner, owner.definition.get.paramToVars)
  }
}
