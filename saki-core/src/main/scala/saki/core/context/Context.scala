package saki.core.context

import saki.core.*
import saki.core.syntax.Var

trait Context[K, +V] {
  def get(key: K): Option[V]
}

trait LocalContext[T <: Entity] extends Context[Var.Local, T] {

  def getValue(local: Var.Local): Option[T]
  def contains(local: Var.Local): Boolean

  def uniqueVariable: Var.Local = uniqueVariable("$")

  def uniqueVariable(prefix: String): Var.Local = {
    // Iterator that generates names in the format "$0", "$1", ...
    val nameIterator: Iterator[String] = Iterator.from(0).map(i => s"$prefix$i")
    // Find the first unique name that does not collide with any key in env
    val uniqueName = nameIterator.find(name => !this.contains(Var.Local(name))).get
    // Return a new Var.Local with the unique name
    Var.Local(uniqueName)
  }
}

trait LocalMutableContext[T <: Entity] extends LocalContext[T] {
  def add(ident: Var.Local, value: T): LocalMutableContext[T]
  def addAll(locals: Map[Var.Local, T]): LocalMutableContext[T]
}

trait TypedLocalContext[T <: Entity] extends LocalContext[T] {
  def getTyped(local: Var.Local): Option[Typed[T]]
}

trait TypedLocalMutableContext[T <: Entity] extends TypedLocalContext[T] {
  def add(ident: Var.Local, value: T, `type`: T): TypedLocalMutableContext[T]
  def add(ident: Var.Local, value: Typed[T]): TypedLocalMutableContext[T] = this.add(ident, value.value, value.`type`)
  def addAll(locals: Map[Var.Local, Typed[T]]): TypedLocalMutableContext[T]
}

trait DefinitionContext {

  def definitions: Map[Var.Defined[Term, ?], Definition[Term]]

  def getDefinition(definition: Var.Defined[Term, ?]): Option[Definition[Term]]

  def getDefinitionByName(name: String): Option[Definition[Term]] = {
    definitions.collectFirst {
      case (varDef, definition) if varDef.name == name => definition
    }
  }

}

trait CurrentDefinition {
  def currentDefinition: Option[Var.Defined[Term, ?]]
}

trait CurrentDefinitionContext extends DefinitionContext with CurrentDefinition

trait MutableDefinitionContext extends DefinitionContext {
  def add[Def[E <: Entity] <: Definition[E]](definition: Def[Term]): MutableDefinitionContext
}

case class Typed[+T <: Entity](value: T, `type`: T) {
  def unapply: (T, T) = (value, `type`)
  override def toString: String = s"($value: ${`type`})"
}
