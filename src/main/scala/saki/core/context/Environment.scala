package saki.core.context

import saki.core.*
import saki.core.syntax.*

trait Environment[T <: Entity] extends LocalContext[T]
  with MutableDefinitionContext
  with CurrentDefinitionContext

object Environment {
  export saki.core.context.TypedEnvironment as Typed
  export saki.core.context.UntypedEnvironment as Untyped
}

case class TypedEnvironment[T <: Entity](
  override val definitions: Map[Var.Defined[Term, ?], Definition[Term]] = Map.empty,
  override val currentDefinition: Option[Var.Defined[Term, ?]] = None,
  val locals: Map[Var.Local, Typed[T]] = Map.empty[Var.Local, Typed[T]],
) extends Environment[T] with TypedLocalMutableContext[T] {

  override def get(key: Var.Local): Option[T] = locals.get(key).map(_.value)
  
  override def add(ident: Var.Local, value: T, `type`: T): TypedEnvironment[T] = {
    TypedEnvironment[T](
      definitions = definitions,
      currentDefinition = currentDefinition,
      locals = locals + (ident -> Typed(value, `type`)),
    )
  }

  override def addAll(locals: Map[Var.Local, Typed[T]]): TypedEnvironment[T] = {
    TypedEnvironment[T](
      definitions = definitions,
      currentDefinition = currentDefinition,
      locals = this.locals ++ locals,
    )
  }
  
  override def getTyped(ident: Var.Local): Option[Typed[T]] = locals.get(ident)
  
  override def add[Def[E <: Entity] <: Definition[E]](definition: Def[Term]): TypedEnvironment[T] = {
    TypedEnvironment[T](
      definitions = definitions + (definition.ident -> definition),
      currentDefinition = currentDefinition,
      locals = locals,
    )
  }

  override def getDefinition(definition: Var.Defined[Term, ?]): Option[Definition[Term]] = {
    definitions.get(definition)
  }
  
  override def getValue(local: Var.Local): Option[T] = locals.get(local).map(_.value)
  
  override def contains(local: Var.Local): Boolean = locals.contains(local)

  def withCurrentDefinition[R](definition: Var.Defined[Term, ?])(action: TypedEnvironment[T] => R): R = {
    action(TypedEnvironment[T](definitions + (definition -> definition.definition.get), Some(definition), locals))
  }

  def withLocal[R](ident: Var.Local, value: T, `type`: T)(action: TypedEnvironment[T] => R): R = {
    action(add(ident, value, `type`))
  }

  def withLocal[R](ident: Var.Local, value: Typed[T])(action: TypedEnvironment[T] => R): R = {
    action(this.add(ident, value.value, value.`type`))
  }

  def withLocals[R](locals: Map[Var.Local, Typed[T]])(action: TypedEnvironment[T] => R): R = {
    action(addAll(locals))
  }
}

object TypedEnvironment {

  def empty[T <: Entity]: TypedEnvironment[T] = TypedEnvironment[T]()
  
  def global[T <: Entity](definitions: Map[Var.Defined[Term, ?], Definition[Term]]): TypedEnvironment[T] = {
    TypedEnvironment[T](definitions = definitions)
  }

  def apply[T <: Entity](other: CurrentDefinitionContext): TypedEnvironment[T] = {
    new TypedEnvironment[T](
      currentDefinition = other.currentDefinition,
      definitions = other.definitions,
    )
  }
  
}

case class UntypedEnvironment[T <: Entity](
  override val definitions: Map[Var.Defined[Term, ?], Definition[Term]] = Map.empty,
  override val currentDefinition: Option[Var.Defined[Term, ?]] = None,
  val locals: Map[Var.Local, T] = Map.empty[Var.Local, T],
) extends Environment[T] with LocalMutableContext[T] {

  override def get(key: Var.Local): Option[T] = locals.get(key)

  override def add(ident: Var.Local, value: T): UntypedEnvironment[T] = {
    UntypedEnvironment[T](
      definitions = definitions,
      currentDefinition = currentDefinition,
      locals = locals + (ident -> value),
    )
  }

  override def addAll(locals: Map[Var.Local, T]): UntypedEnvironment[T] = {
    UntypedEnvironment[T](
      definitions = definitions,
      currentDefinition = currentDefinition,
      locals = this.locals ++ locals,
    )
  }

  override def getValue(local: Var.Local): Option[T] = locals.get(local)

  override def contains(local: Var.Local): Boolean = locals.contains(local)

  override def add[Def[E <: Entity] <: Definition[E]](definition: Def[Term]): UntypedEnvironment[T] = {
    UntypedEnvironment[T](
      definitions = definitions + (definition.ident -> definition),
      currentDefinition = currentDefinition,
      locals = locals,
    )
  }

  override def getDefinition(definition: Var.Defined[Term, ?]): Option[Definition[Term]] = {
    definitions.get(definition)
  }

  def withCurrentDefinition[R](definition: Var.Defined[Term, ?])(action: UntypedEnvironment[T] => R): R = {
    action(UntypedEnvironment[T](definitions, Some(definition), locals))
  }

  def withLocal[R](ident: Var.Local, value: T)(action: UntypedEnvironment[T] => R): R = {
    action(add(ident, value))
  }

  def withLocals[R](locals: Map[Var.Local, T])(action: UntypedEnvironment[T] => R): R = {
    action(addAll(locals))
  }
}

object UntypedEnvironment {

  def empty[T <: Entity]: UntypedEnvironment[T] = UntypedEnvironment[T]()

  def global[T <: Entity](definitions: Map[Var.Defined[Term, ?], Definition[Term]]): UntypedEnvironment[T] = {
    UntypedEnvironment[T](definitions = definitions)
  }

  def apply[T <: Entity](other: CurrentDefinitionContext): UntypedEnvironment[T] = {
    new UntypedEnvironment[T](
      definitions = other.definitions,
      currentDefinition = other.currentDefinition,
    )
  }

}
