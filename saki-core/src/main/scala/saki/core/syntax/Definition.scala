package saki.core.syntax

import saki.core.{Entity, EntityFactory}
import saki.core.domain.Value
import saki.util.LateInit

import scala.collection.Seq

enum Var {

  case Defined[T <: Entity, Def[A <: Entity] <: Definition[A]](
    override val name: String,
    definition: LateInit[Def[T]] = LateInit[Def[T]](),
  )

  case Local(override val name: String)

  def name: String = this match {
    case Defined(name, _) => name
    case Local(name) => name
  }

  override def equals(that: Any): Boolean = that match {
    case that: Var.Defined[Term@unchecked, ?] => this.name == that.name
    case that: Var.Local => this.name == that.name
    case _ => false
  }

  override def toString: String = this.name

  def to[U <: Entity, Def[E <: Entity] <: Definition[E]]: Var.Defined[U, Def] = {
    this match {
      case Defined(name, _) => Defined(name)
      case Local(name) => Local(name).asInstanceOf[Var.Defined[U, Def]]
    }
  }
}

case class Param[T <: Entity | Option[Entity]](
  ident: Var.Local, `type`: T,
  applyMode: ApplyMode = ApplyMode.Explicit,
) {

  def name: String = ident.name

  def map[U <: Entity | Option[Entity]](transform: T => U): Param[U] = {
    copy(`type` = transform(`type`))
  }

  def unapply: (Var.Local, T) = (ident, `type`)

  override def toString: String = applyMode match {
    case ApplyMode.Explicit => s"$name: ${`type`}"
    case ApplyMode.Implicit => s"[$name: ${`type`}]"
    case ApplyMode.Instance => s"{$name: ${`type`}}"
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

extension [T <: Entity | Option[Entity]](params: ParamList[T]) {
  def map[U <: Entity | Option[Entity]](transform: T => U): ParamList[U] = {
    params.map(_.map(transform))
  }
}

case class Argument[T <: Entity](
  value: T,
  applyMode: ApplyMode = ApplyMode.Explicit,
) {
  def map[U <: Entity](transform: T => U): Argument[U] = {
    copy(value = transform(value))
  }

  override def toString: String = applyMode match {
    case ApplyMode.Explicit => value.toString
    case ApplyMode.Implicit => s"[$value]"
    case ApplyMode.Instance => s"{$value}"
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
  def paramToVars(implicit factory: EntityFactory[T, T]): Seq[T] = params.map(_.ident).map(factory.variable)
  def arguments[T1 <: Entity](argValues: Seq[T1]): Seq[(Param[T], T1)] = params.zip(argValues)
}

trait Declaration[T <: Entity] {
  def ident: Var.Defined[T, ?]
  def toIdent[U <: Entity]: Var.Defined[U, ?]
}

sealed trait Definition[T <: Entity] extends Declaration[T]

// Too young, too simple, sometimes naive
sealed trait NaiveDefinition[T <: Entity] extends Definition[T] with FnLike[T] {

  def resultType(implicit ev: EntityFactory[T, T]): T

  def signature(implicit ev: EntityFactory[T, T]): Signature[T] = Signature(params, resultType)

  def buildInvoke(implicit factory: EntityFactory[T, T]): T = this match {
    case definition: Function[T] => factory.functionInvoke(definition.ident, definition.signature.paramToVars)
    case definition: Inductive[T] => factory.inductiveType(definition.ident, definition.signature.paramToVars)
  }
}

trait Function[T <: Entity] extends NaiveDefinition[T] {
  override def ident: Var.Defined[T, Function]
  def isRecursive: Boolean
  def resultType: T
}

case class DefinedFunction[T <: Entity](
  override val ident: Var.Defined[T, Function],
  override val params: ParamList[T],
  override val resultType: T,
  // Mark this function as a recursive or mutual recursive function
  // When a function is a recursive function, its invocation will not be evaluated instantly
  // unless all its arguments are pure values
  override val isRecursive: Boolean,
  body: LateInit[T] = LateInit[T](),
) extends Function[T] {

  override def resultType(implicit ev: EntityFactory[T, T]): T = resultType

  override def toIdent[U <: Entity]: Var.Defined[U, Function] = Var.Defined(ident.name)

  override def toString: String = {
    s"def ${ident.name}(${params.mkString(", ")}): $resultType = \n\t$body"
  }
}

case class NativeFunction[T <: Entity](
  override val ident: Var.Defined[T, Function],
  override val params: ParamList[T],
  override val resultType: T,
  override val isRecursive: Boolean = false,
  nativeImpl: ArgList[Value] => Value,
) extends Function[T] {
  override def toIdent[U <: Entity]: Var.Defined[U, Function] = Var.Defined(ident.name)
  override def resultType(implicit ev: EntityFactory[T, T]): T = resultType
  def invoke(args: ArgList[Value]): Value = {
    assert(args.length == params.length)
    nativeImpl(args)
  }
}

case class Overloaded[T <: Entity](
  override val ident: Var.Defined[T, Overloaded],
  overloads: Seq[Function[T]],
) extends Definition[T] {
  override def toIdent[U <: Entity]: Var.Defined[U, Overloaded] = Var.Defined(ident.name)
}

case class Inductive[T <: Entity](
  override val ident: Var.Defined[T, Inductive],
  override val params: ParamList[T],
  constructors: Seq[Constructor[T]],
) extends NaiveDefinition[T] {

  def resultType(implicit ev: EntityFactory[T, T]): T = ev.universe

  override def toString: String = {
    s"inductive ${ident.name}(${params.mkString(", ")})"
  }

  override def toIdent[U <: Entity]: Var.Defined[U, Inductive] = Var.Defined(ident.name)

  def getConstructor(name: String): Option[Constructor[T]] = {
    constructors.find(_.ident == name)
  }
}

case class Constructor[T <: Entity](
  ident: String,
  owner: Var.Defined[T, Inductive],
  override val params: ParamList[T],
) extends FnLike[T] {
  override def toString: String = {
    s"$ident(${params.mkString(", ")})"
  }
}

case class PreDeclaration[T <: Entity, Def[E <: Entity] <: Definition[E]](
  ident: Var.Defined[T, Def], signature: Signature[T],
) extends Declaration[T] {
  override def toIdent[U <: Entity]: Var.Defined[U, Def] = Var.Defined(ident.name)
}
