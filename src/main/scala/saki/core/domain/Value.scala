package saki.core.domain

import saki.core.context.{CurrentDefinitionContext, Environment, LocalContext, TypedEnvironment, TypedLocalMutableContext}
import saki.core.{Entity, EntityFactory, RuntimeEntity, RuntimeEntityFactory}
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
    val inductive: Var.Defined[Term, Inductive],
    val args: Seq[Value],
  )

  case InductiveVariant(
    val cons: Var.Defined[Term, Constructor],
    val consArgs: Seq[Value],
    val inductiveArgs: Seq[Value],
  )

  override def infer(
    implicit env: Environment.Typed[Value]
  ): Type = this.readBack.infer

  def readBack(implicit env: Environment.Typed[Value]): Term = this match {

    case Universe => Term.Universe

    case Primitive(value) => Term.Primitive(value)

    case PrimitiveType(ty) => Term.PrimitiveType(ty)

    case Neutral(value) => value.readBack

    case Pi(paramType, codomain) => {
      val paramIdent = env.uniqueVariable
      val param = Value.variable(paramIdent)
      Term.Pi(
        Param(paramIdent, paramType.readBack),
        env.withLocal(paramIdent, param, paramType) {
          codomain(Value.variable(paramIdent)).readBack
        }
      )
    }

    case Sigma(paramType, codomain) => {
      val paramIdent = env.uniqueVariable
      val param = Value.variable(paramIdent)
      Term.Sigma(
        Param(paramIdent, paramType.readBack),
        env.withLocal(paramIdent, param, paramType) {
          codomain(Value.variable(paramIdent)).readBack
        }
      )
    }

    case Lambda(paramType, body) => {
      val paramIdent = env.uniqueVariable
      val param = Value.variable(paramIdent)
      Term.Lambda(
        Param(paramIdent, paramType.readBack),
        env.withLocal(paramIdent, param, paramType) {
          body(Value.variable(paramIdent)).readBack
        }
      )
    }

    case Record(fields) => Term.Record(fields.map((name, value) => (name, value.readBack)))

    case RecordType(fields) => Term.RecordType(fields.map((name, ty) => (name, ty.readBack)))

    case InductiveType(inductive, args) => {
      Term.inductiveType(inductive, args.map(_.readBack))
    }

    case InductiveVariant(cons, consArgs, inductiveArgs) => {
      Term.inductiveVariant(cons, consArgs.map(_.readBack), inductiveArgs.map(_.readBack))
    }
  }
  
  infix def unify(that: Type): Boolean = (this, that) match {
    case (Universe, Universe) => true
    case (Primitive(value1), Primitive(value2)) => value1 == value2
    case (PrimitiveType(ty1), PrimitiveType(ty2)) => ty1 == ty2
    case (Neutral(value1), Neutral(value2)) => value1 unify value2
    case (Pi(paramType1, closure1), Pi(paramType2, closure2)) => {
      val param = Value.variable(Var.Local("param"))
      (paramType1 unify paramType2) && (closure1(param) unify closure2(param))
    }
    case (Sigma(paramType1, closure1), Sigma(paramType2, closure2)) => {
      val param = Value.variable(Var.Local("param"))
      (paramType1 unify paramType2) && (closure1(param) unify closure2(param))
    }
    case (Lambda(paramType1, body1), Lambda(paramType2, body2)) => {
      val param = Value.variable(Var.Local("param"))
      (paramType1 unify paramType2) && (body1(param) unify body2(param))
    }
    case (Record(fields1), Record(fields2)) => {
      fields1.size == fields2.size &&
      fields1.forall { (name, value1) =>
        fields2.get(name) match {
          case Some(value2) => value1 unify value2
          case None => false
        }
      }
    }
    case (RecordType(fields1), RecordType(fields2)) => {
      fields1.size == fields2.size &&
      fields1.forall { (name, ty1) =>
        fields2.get(name) match {
          case Some(ty2) => ty1 unify ty2
          case None => false
        }
      }
    }
    case (InductiveType(inductive1, args1), InductiveType(inductive2, args2)) => {
      inductive1 == inductive2 && args1.zip(args2).forall((arg1, arg2) => arg1 unify arg2)
    }
    case (InductiveVariant(cons1, consArgs1, inductiveArgs1), InductiveVariant(cons2, consArgs2, inductiveArgs2)) => {
      cons1 == cons2 && consArgs1.zip(consArgs2).forall((arg1, arg2) => arg1 unify arg2) &&
      inductiveArgs1.zip(inductiveArgs2).forall((arg1, arg2) => arg1 unify arg2)
    }
    case _ => false
  }

}

object Value extends RuntimeEntityFactory[Value] {

  override def unit: Type = Value.Primitive(Literal.UnitValue)

  override def unitType: Type = Value.PrimitiveType(LiteralType.UnitType)

  override def universe: Type = Universe

  override def variable(ident: Var.Local): Value = Neutral(NeutralValue.Variable(ident))

  override def functionInvoke(function: Var.Defined[Term, Function], args: Seq[Type]): Type = {
    Neutral(NeutralValue.FunctionInvoke(function, args))
  }

  override def inductiveType(inductive: Var.Defined[Term, Inductive], args: Seq[Type]): Type = {
    Value.InductiveType(inductive, args)
  }

  override def inductiveVariant(
    cons: Var.Defined[Term, Constructor], consArgs: Seq[Type], inductiveArgs: Seq[Type]
  ): Type = {
    Value.InductiveVariant(cons, consArgs, inductiveArgs)
  }

}

enum NeutralValue {

  case Variable(ident: Var.Local)

  case Apply(fn: NeutralValue, arg: Value)

  case Projection(record: Value, field: String)

  // global function call
  case FunctionInvoke(
    val fn: Var.Defined[Term, Function],
    val args: Seq[Value],
  )

  case Match(
    val scrutinees: Seq[Value],
    val clauses: Seq[Clause[Value]]
  )
  
  def infer(implicit env: Environment.Typed[Value]): Type = ???

  def readBack(implicit env: Environment.Typed[Value]): Term = this match {
    case Variable(ident) => Term.Variable(ident)
    case Apply(fn, arg) => Term.Apply(fn.readBack, arg.readBack)
    case Projection(record, field) => Term.Projection(record.readBack, field)
    case FunctionInvoke(fnRef, args) => Term.FunctionInvoke(fnRef, args.map(_.readBack))
    case Match(scrutinees, clauses) => Term.Match(scrutinees.map(_.readBack), clauses.map(_.map(_.readBack)))
  }

  infix def unify(that: NeutralValue): Boolean = (this, that) match {

    case (Variable(ident1), Variable(ident2)) => ident1 == ident2
    case (Apply(fn1, arg1), Apply(fn2, arg2)) => (fn1 unify fn2) && (arg1 unify arg2)

    case (Projection(record1, field1), Projection(record2, field2)) => {
      (record1 unify record2) && (field1 == field2)
    }

    case (FunctionInvoke(fn1, args1), FunctionInvoke(fn2, args2)) => {
      fn1 == fn2 && args1.zip(args2).forall((arg1, arg2) => arg1 unify arg2)
    }

    case (Match(scrutinees1, clauses1), Match(scrutinees2, clauses2)) => {
      scrutinees1.zip(scrutinees2).forall((scrutinee1, scrutinee2) => scrutinee1 unify scrutinee2) &&
      clauses1.forall { clause1 =>
        clauses2.find(_.patterns == clause1.patterns) match {
          case Some(clause2) => clause1.body unify clause2.body
          case None => false
        }
      }
    }

    case _ => false
  }

}
