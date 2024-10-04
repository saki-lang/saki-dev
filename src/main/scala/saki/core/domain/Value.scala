package saki.core.domain

import saki.core.{Entity, EntityFactory, RuntimeEntity}
import saki.core.syntax.*

import scala.collection.Seq

type Type = Value

enum Value extends RuntimeEntity[Type] {

  case Universe

  case Primitive(value: Literal)

  case PrimitiveType(`type`: LiteralType)

  case Neutral(value: NeutralValue)

  case Pi(paramType: Type, closure: Value => Value)

  case Sigma(paramType: Type, closure: Value => Value)

  case Lambda(paramType: Type, body: Value => Value)

  case Record(fields: Map[String, Value])

  case RecordType(fields: Map[String, Type])

  case InductiveType(
    val inductive: Var.Defined[?, Inductive],
    val args: Seq[Value],
  )

  case InductiveVariant(
    val cons: Var.Defined[?, Constructor],
    val consArgs: Seq[Value],
    val inductiveArgs: Seq[Value],
  )

  def isFinal(implicit env: Environment): Boolean = this match {
    case Universe => true
    case Primitive(_) => true
    case PrimitiveType(_) => true
    case Neutral(_) => false
    case Pi(_, _) => ???
    case Sigma(_, _) => ???
    case Lambda(_, _) => ???
    case Record(fields) => fields.values.forall(_.isFinal)
    case RecordType(fields) => fields.values.forall(_.isFinal)
    case InductiveType(_, args) => args.forall(_.isFinal)
    case InductiveVariant(_, consArgs, inductiveArgs) => consArgs.forall(_.isFinal) && inductiveArgs.forall(_.isFinal)
  }

  override def infer(implicit env: Environment): Type = ???

  def readBack(implicit env: Environment): Term = this match {
    case Universe => Term.Universe
    case Primitive(value) => Term.Primitive(value)
    case PrimitiveType(ty) => Term.PrimitiveType(ty)
    case Neutral(value) => value.readBack
    case Pi(paramType, codomain) => {
      val paramIdent = Value.uniqueVariable
      val param = Value.variable(paramIdent)
      val paramTerm = Term.Variable(paramIdent)
      val newEnv: Environment = env.add(paramIdent, param, paramType)
      Term.Pi(Param(paramIdent, paramTerm), codomain(param).readBack(newEnv))
    }
    case Sigma(paramType, codomain) => {
      val paramIdent = Value.uniqueVariable
      val param = Value.variable(paramIdent)
      val paramTerm = Term.Variable(paramIdent)
      val newEnv: Environment = env.add(paramIdent, param, paramType)
      Term.Sigma(Param(paramIdent, paramTerm), codomain(param).readBack(newEnv))
    }
    case Lambda(paramType, body) => {
      val paramIdent = Value.uniqueVariable
      val param = Value.variable(paramIdent)
      val newEnv: Environment = env.add(paramIdent, param, paramType)
      Term.Lambda(paramIdent, body(param).readBack(newEnv))
    }
    case Record(fields) => Term.Record(fields.map((name, value) => (name, value.readBack)))
    case RecordType(fields) => Term.RecordType(fields.map((name, ty) => (name, ty.readBack)))
  }

}

object Value extends EntityFactory[Value] {

  override def unit: Type = Value.Primitive(Literal.UnitValue)

  override def unitType: Type = Value.PrimitiveType(LiteralType.UnitType)

  override def universe: Type = Universe

  override def variable(ident: Var.Local): Value = Neutral(NeutralValue.Variable(ident))

  override def functionInvoke(function: Var.Defined[Type, Function], args: Seq[Type]): Type = {
    Neutral(NeutralValue.FunctionInvoke(function, args))
  }

  def functionInvokeGeneral(function: Var.Defined[?, Function], args: Seq[Value]): Value = {
    Neutral(NeutralValue.FunctionInvoke(function, args))
  }

  override def inductiveType(inductive: Var.Defined[Type, Inductive], args: Seq[Type]): Type = {
    Value.InductiveType(inductive, args)
  }

  def inductiveTypeGeneral(inductive: Var.Defined[?, Inductive], args: Seq[Value]): Value = {
    Value.InductiveType(inductive, args)
  }

  override def inductiveVariant(cons: Var.Defined[Type, Constructor], consArgs: Seq[Type], inductiveArgs: Seq[Type]): Type = {
    Value.InductiveVariant(cons, consArgs, inductiveArgs)
  }

  def inductiveVariantGeneral(cons: Var.Defined[?, Constructor], consArgs: Seq[Value], inductiveArgs: Seq[Value]): Value = {
    Value.InductiveVariant(cons, consArgs, inductiveArgs)
  }

  def uniqueVariable(implicit env: Environment): Var.Local = {
    // Iterator that generates names in the format "param$0", "param$1", ...
    val nameIterator = Iterator.from(0).map(i => s"param$$$i")
    // Find the first unique name that does not collide with any key in env
    val uniqueName = nameIterator.find(name => !env.contains(Var.Local(name))).get
    // Return a new Var.Local with the unique name
    Var.Local(uniqueName)
  }

}

enum NeutralValue {

  case Variable(ident: Var.Local)

  case Apply(fn: NeutralValue, arg: Value)

  case Projection(record: Value, field: String)

  // global function call
  case FunctionInvoke(
    val fn: Var.Defined[?, Function],
    val args: Seq[Value],
  )

  case Match(
    val scrutinees: Seq[Value],
    val clauses: Seq[Clause[Type]]
  )
  
  def infer(implicit env: Environment): Type = ???

  def readBack(implicit env: Environment): Term = this match {
    case Variable(ident) => Term.Variable(ident)
    case Apply(fn, arg) => Term.Apply(fn.readBack, arg.readBack)
    case Projection(record, field) => Term.Projection(record.readBack, field)
  }
}
