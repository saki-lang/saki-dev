package saki.core.domain

import saki.core.Entity
import saki.core.domain.Environment.{DefinitionKind, TypedValue}
import saki.core.domain.Type
import saki.core.syntax.*

case class Environment private(
  private val definitions: Map[Var.Defined[?, ?], (Definition[?], DefinitionKind[?])],
  private val locals: Map[Var.Local, TypedValue],
  currentDefinition: Option[Var.Defined[Value, ?]] = None,
) {

  def add[T <: Entity, Def[E <: Entity] <: Definition[E]](definition: Def[T])(
    implicit defType: Environment.DefinitionKind[Def[T]]
  ): Environment = defType match {
    case Environment.DefinitionKindTerm => addTermDef(definition.asInstanceOf[Definition[Term]])
    case Environment.DefinitionKindValue => addValueDef(definition.asInstanceOf[Definition[Value]])
  }

  def addValueDef(definition: Definition[Value]): Environment = {
    copy(definitions = definitions.updated(definition.ident, (definition, Environment.DefinitionKindValue)))
  }

  def addTermDef(definition: Definition[Term]): Environment = {
    copy(definitions = definitions.updated(definition.ident, (definition, Environment.DefinitionKindTerm)))
  }

  def add(local: TypedValue): Environment = {
    copy(locals = locals.updated(local.value.asInstanceOf[Var.Local], local))
  }

  def add(local: Var.Local, value: Value, `type`: Type): Environment = {
    copy(locals = locals.updated(local, TypedValue(value, `type`)))
  }

  def addVars(locals: Map[Var.Local, TypedValue]): Environment = {
    copy(locals = this.locals ++ locals)
  }

  def addArgs(args: (Argument[Value], Type)*): Environment = {
    copy(locals = this.locals ++ args.map {
      case (Argument(value, _), ty) => value.asInstanceOf[Var.Local] -> TypedValue(value, ty)
    })
  }

  def lookup(local: Var.Local): Option[TypedValue] = locals.get(local)

  def lookup[T <: Entity, Def[E <: Entity] <: Definition[E]](definition: Var.Defined[T, Def]): Option[Def[T]] = {
    definitions.get(definition).map(_._1.asInstanceOf[Def[T]])
  }

  def apply(local: Var.Local): TypedValue = locals(local)

  def apply[T <: Entity, Def[E <: Entity] <: Definition[E]](definition: Var.Defined[T, Def]): Def[T] = {
    definitions(definition)._1.asInstanceOf[Def[T]]
  }

  private[core] def withVars[R](vars: Map[Var.Local, TypedValue])(action: Environment => R): R = {
    action(this.copy(locals = this.locals ++ vars))
  }
}

object Environment {

  sealed trait DefinitionKind[D <: Definition[?]]
  private case object DefinitionKindTerm extends DefinitionKind[Definition[Term]]
  private case object DefinitionKindValue extends DefinitionKind[Definition[Value]]

  case class TypedValue(value: Value, `type`: Type) {
    def unapply: (Value, Type) = (value, `type`)
  }
}


