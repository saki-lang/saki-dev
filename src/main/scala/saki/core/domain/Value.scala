package saki.core.domain

import saki.core.{Entity, EntityFactory}
import saki.core.syntax.*

import scala.collection.Seq

type Type = Value

enum Value extends Entity {

  case Universe

  case Primitive(value: Literal)

  case PrimitiveType(`type`: LiteralType)

  case Neutral(value: NeutralValue)

  case Pi(param: Param[Type], codomain: Type)

  case Sigma(param: Param[Type], codomain: Type)

  case Record(fields: Map[String, Value])

  case RecordType(fields: Map[String, Type])

  case Lambda(param: Param[Option[Type]], returnType: Option[Type], body: Value)

}

object Value extends EntityFactory[Value] {

  override def unit: Type = Value.Primitive(Literal.UnitValue)

  override def unitType: Type = Value.PrimitiveType(LiteralType.UnitType)

  override def universe: Type = Universe

  override def variable(ident: Var.Local): Value = Neutral(NeutralValue.Variable(ident))

  override def inductiveCall(inductive: Var.Defined[Type, Inductive], args: Seq[Type]): Type = ???

  override def functionCall(function: Var.Defined[Type, Function], args: Seq[Type]): Type = ???

  override def constructorCall(cons: Var.Defined[Type, Constructor], consArgs: Seq[Type], inductiveArgs: Seq[Type]): Type = ???

}

enum NeutralValue {

  case Variable(ident: Var.Local)

  case Apply(fn: NeutralValue, arg: Value)

  case Projection(record: Value, field: String)

  // global function call
  case FunctionCall(fn: Var.Defined[Value, Function], args: Seq[Value])

  case InductiveCall(inductive: Var.Defined[Value, Inductive], args: Seq[Value])

  case ConstructorCall(cons: Var.Defined[Value, Constructor], consArgs: Seq[Value], inductiveArgs: Seq[Term])

  case Match(scrutinees: Seq[Value], clauses: Seq[Clause[Type]])

}
