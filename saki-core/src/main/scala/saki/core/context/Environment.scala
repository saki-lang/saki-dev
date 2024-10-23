package saki.core.context

import saki.core.*
import saki.core.syntax.*
import saki.error.CoreErrorKind.*

import scala.collection.Seq

trait Environment[T <: Entity] extends LocalContext[T]
  with MutableDefinitionContext
  with CurrentDefinitionContext

object Environment {
  export saki.core.context.TypedEnvironment as Typed
}

case class TypedEnvironment[T <: Entity] private (
  override val definitions: Map[Var.Defined[Term, ?], Definition[Term]] = Map.empty,
  override val currentDefinition: Option[Var.Defined[Term, ?]] = None,
  declarations: Map[Var.Defined[Term, ?], Declaration[Term, ?]] = Map.empty,
  locals: Map[Var.Local, Typed[T]] = Map.empty[Var.Local, Typed[T]],
) extends Environment[T] with TypedLocalMutableContext[T] {

  override def get(key: Var.Local): Option[T] = locals.get(key).map(_.value)

  override def add(ident: Var.Local, value: T, `type`: T): TypedEnvironment[T] = {
    TypedEnvironment[T](
      definitions = definitions,
      currentDefinition = currentDefinition,
      declarations = declarations,
      locals = locals + (ident -> Typed(value, `type`)),
    )
  }

  override def addAll(locals: Map[Var.Local, Typed[T]]): TypedEnvironment[T] = {
    TypedEnvironment[T](
      definitions = definitions,
      currentDefinition = currentDefinition,
      declarations = declarations,
      locals = this.locals ++ locals,
    )
  }
  
  override def getTyped(ident: Var.Local): Option[Typed[T]] = locals.get(ident)

  def getTyped(name: String): Option[Typed[T]] = locals.get(Var.Local(name))
  
  override def add[Def[E <: Entity] <: Definition[E]](definition: Def[Term]): TypedEnvironment[T] = {
    val updatedDefinition: Definition[Term] = this.getDefinition(definition.ident) match {
      case Some(existing) => (existing, definition) match {
        case (existing: (Overloaded[Term] | Function[Term]), definition: (Overloaded[Term] | Function[Term])) => {
          Overloaded.merge(existing, definition)
        }
        case _ => DefinitionNotMatch.raise {
          "Definition kind not match: " + existing + " and " + definition
        }
      }
      case None => definition
    }
    TypedEnvironment[T](
      definitions = definitions + (updatedDefinition.ident -> updatedDefinition),
      currentDefinition = currentDefinition,
      declarations = declarations,
      locals = locals,
    )
  }

  def addDeclaration(declaration: Declaration[Term, ?]): TypedEnvironment[T] = {
    TypedEnvironment[T](
      definitions = definitions,
      currentDefinition = currentDefinition,
      declarations = declarations + (declaration.ident -> declaration),
      locals = locals,
    )
  }

  override def getDefinition(definition: Var.Defined[Term, ?]): Option[Definition[Term]] = {
    definitions.get(definition)
  }

  def getSymbol(ident: Var.Defined[Term, ?]): Option[Symbol[Term]] = {
    // The precedence of declarations is higher than definitions
    // since we need to make sure the mutual recursive overloading
    // of the same function can be resolved correctly
    declarations.get(ident).orElse(definitions.get(ident))
  }

  def getSymbolByName(name: String): Option[Symbol[Term]] = {
    declarations.collectFirst {
      // The precedence of declarations is higher than definitions, same as `getSymbol`
      case (ident, symbol: Symbol[Term]) if ident.name == name => symbol
    }.orElse(this.getDefinitionByName(name))
  }

  override def getValue(local: Var.Local): Option[T] = locals.get(local).map(_.value)
  
  override def contains(local: Var.Local): Boolean = locals.contains(local)

  def contains(definition: Var.Defined[?, ?]): Boolean = definitions.contains(Var.Defined(definition.name))

  def withCurrentDefinition[R](definition: Var.Defined[Term, ?])(action: TypedEnvironment[T] => R): R = {
    action(TypedEnvironment[T](
      definitions = definitions + (definition -> definition.definition.get),
      currentDefinition = Some(definition),
      declarations = declarations,
      locals = locals,
    ))
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

  def global[T <: Entity](definitions: Seq[Definition[Term]]): TypedEnvironment[T] = {
    TypedEnvironment[T](
      definitions = definitionsToMap(definitions),
      currentDefinition = None,
      declarations = Map.empty,
      locals = Map.empty[Var.Local, Typed[T]],
    )
  }

  def apply[T <: Entity](other: CurrentDefinitionContext): TypedEnvironment[T] = {
    new TypedEnvironment[T](
      currentDefinition = other.currentDefinition,
      definitions = other.definitions,
      declarations = Map.empty,
    )
  }

  private def definitionsToMap(definitions: Seq[Definition[Term]]): Map[Var.Defined[Term, ?], Definition[Term]] = {
    definitions.groupBy(_.ident).map { (ident, definitions) =>
      val definition = if definitions.size == 1 then definitions.head else {
        definitions.map {
          case function: Function[Term] => {
            val overloadedIdent: Var.Defined[Term, Overloaded] = Var.Defined(ident.name)
            Overloaded(overloadedIdent, Seq(function))
          }
          case overloaded: Overloaded[Term] => overloaded
          case _ => DefinitionNotMatch.raise {
            "Expected function or overloaded definition, but got: " + definitions.head
          }
        }.reduce { (overloaded, function) => overloaded.merge(function) }
      }
      ident -> definition
    }
  }
  
}

