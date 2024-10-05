package saki.core.syntax

import saki.core.{Entity, EntityFactory, TypeError}
import saki.util.LateInit

import scala.collection.Seq

enum Var {

  case Defined[T <: Entity, Def[A <: Entity] <: Definition[A]](
    override val name: String,
    val definition: LateInit[Def[T]] = LateInit[Def[T]](),
  )

  case Local(override val name: String)

  def name: String = this match {
    case Defined(name, _) => name
    case Local(name) => name
  }

  override def equals(that: Any): Boolean = that match {
    case that: Var.Defined[?, ?] => this.name == that.name
    case that: Var.Local => this.name == that.name
    case _ => false
  }

  override def toString: String = this.name
}

extension [T <: Entity, Def[E <: Entity] <: Definition[E]](self: Var.Defined[T, Def]) {

  def signature(implicit factory: EntityFactory[T]): Signature[T] = self.definition.get.signature

  def call(implicit factory: EntityFactory[T]): T = self.definition.toOption match {

    case Some(_: Function[T]) => {
      val signature: Signature[T] = self.definition.get.signature
      factory.functionInvoke(self.asInstanceOf[Var.Defined[T, Function]], signature.paramToVars)
    }

    case Some(_: Inductive[T]) => {
      val signature: Signature[T] = self.definition.get.signature
      factory.inductiveType(self.asInstanceOf[Var.Defined[T, Inductive]], signature.paramToVars)
    }

    case Some(_: Constructor[T]) => {
      val cons = self.asInstanceOf[Var.Defined[T, Constructor]]
      factory.inductiveVariant(
        cons = cons,
        consArgs = self.signature.paramToVars,
        inductiveArgs = cons.owner.signature.paramToVars
      )
    }

    case None => TypeError(s"Unresolved reference: ${self.name}").raise
  }
}

extension [T <: Entity](variable: Var.Defined[T, Constructor]) {
  def owner: Var.Defined[T, Inductive] = variable.definition.get.owner
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

  override def toString: String = {
    s"def ${ident.name}(${params.mkString(", ")}): $resultType = \n\t$body"
  }
}

case class Inductive[T <: Entity](
  override val ident: Var.Defined[T, Inductive],
  override val params: ParamList[T],
  constructors: Seq[Constructor[T]],
) extends Definition[T] {

  def resultType(implicit ev: EntityFactory[T]): T = ev.universe

  override def toString: String = {
    s"inductive ${ident.name}(${params.mkString(", ")})"
  }
}

case class Constructor[T <: Entity](
  override val ident: Var.Defined[T, Constructor],
  owner: Var.Defined[T, Inductive],
  override val params: ParamList[T],
) extends Definition[T] {
  
  def resultType(implicit factory: EntityFactory[T]): T = {
    factory.inductiveType(owner, owner.definition.get.paramToVars)
  }

  override def toString: String = {
    s"cons(${owner.name}) ${ident.name}(${params.mkString(", ")})"
  }
}
