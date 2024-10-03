package saki.core.domain

import saki.core.Entity
import saki.core.domain.Environment.{DefinitionKind, LocalVariable}
import saki.core.domain.{Type, Value}
import saki.core.syntax.{Argument, Definition, Param, ParamList, Term, Var}
import saki.util.unreachable

import scala.reflect.ClassTag

case class Environment private(
  private val definitions: Map[Var.Defined[?, ?], (Definition[?], DefinitionKind[?])],
  private val locals: Map[Var.Local, LocalVariable],
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

  def add(local: LocalVariable): Environment = {
    copy(locals = locals.updated(local.value.asInstanceOf[Var.Local], local))
  }

  def add(local: Var.Local, `type`: Type): Environment = {
    copy(locals = locals.updated(local, LocalVariable(Value.variable(local), `type`)))
  }

  def addVars(locals: Map[Var.Local, LocalVariable]): Environment = {
    copy(locals = this.locals ++ locals)
  }

  def addArgs(args: (Argument[Value], Type)*): Environment = {
    copy(locals = this.locals ++ args.map {
      case (Argument(value, _), ty) => value.asInstanceOf[Var.Local] -> LocalVariable(value, ty)
    })
  }

  def addParams(params: ParamList[Type]): Environment = {
    copy(locals = this.locals ++ params.map {
      case Param(ident, ty, _) => ident -> LocalVariable(Value.variable(ident), ty)
    })
  }

  def addParams(locals: Map[Var.Local, Type]): Environment = {
    copy(locals = this.locals ++ locals.map {
      case (local, ty) => local -> LocalVariable(Value.variable(local), ty)
    })
  }

  def get(local: Var.Local): Option[LocalVariable] = locals.get(local)

  def get(definition: Var.Defined[?, ?]): Option[Definition[?]] = {
    definitions.get(definition).map(_._1)
  }

  def apply(local: Var.Local): LocalVariable = locals(local)

  def apply(definition: Var.Defined[?, ?]): Definition[?] = definitions(definition)._1

  private[core] def withParam[R](local: Var.Local, `type`: Type)(action: Environment => R): R = {
    action(this.add(local, `type`))
  }

  private[core] def withParams[R](locals: Map[Var.Local, Type])(action: Environment => R): R = {
    action(this.addParams(locals))
  }

  private[core] def withParams[R](params: ParamList[Type])(action: Environment => R): R = {
    action(this.addParams(params))
  }

  private[core] def withVars[R](vars: Map[Var.Local, LocalVariable])(action: Environment => R): R = {
    action(this.copy(locals = this.locals ++ vars))
  }
}

object Environment {

  sealed trait DefinitionKind[D <: Definition[?]]
  private case object DefinitionKindTerm extends DefinitionKind[Definition[Term]]
  private case object DefinitionKindValue extends DefinitionKind[Definition[Value]]

  case class LocalVariable(value: Value, `type`: Type) {
    def unapply: (Value, Type) = (value, `type`)
  }
}


