package saki.core

import saki.TODO

import scala.annotation.targetName

enum Value {
  /**
   * Neutral values
   *
   * Terms for which computation has stopped because of an attempt to
   * evaluate a variable.
   *
   * These are known as neutral values or accumulators.
   */
  case Neutral(head: Head, spine: Spine)

  case Primitive(value: Literal)

  case PrimitiveType(`type`: LiteralType)

  case Universe(level: Int)

  /**
   * Dependent function type.
   *
   * This is the type of a function that takes an argument of the given type
   * and returns a type that depends on the value of the argument.
   *
   * Π (x : A) -> B
   */
  case Pi(paramName: String, paramType: Value, returnTypeClosure: Value => Value)

  /**
   * Dependent function value.
   */
  case Lambda(paramName: String, bodyClosure: Value => Value)

  case RecordType(fieldTypes: Map[String, Value])

  case RecordValue(fields: Map[String, Value])

//  def universe(implicit ctx: Context): Int = this.ty match {
//    case Universe(level) => level
//    case _ => 0
//  }

  def apply(arg: Value): Value = this match {
    case Lambda(_, bodyClosure) => bodyClosure(arg)
    case Neutral(head, spine) => Neutral(head, Elimination.Application(arg) :: spine)
  }

  def project(name: String): Value = this match {
    case RecordValue(fields) => fields(name)
    case Neutral(head, spine) => Neutral(head, Elimination.Projection(this, name) :: spine)
  }

  def applySpine(spine: Spine): Value = spine match {
    case Nil => this
    case Elimination.Application(arg) :: rest => this.applySpine(rest).apply(arg)
    case Elimination.Projection(record, field) :: rest => record.applySpine(rest).project(field)
  }

  def force(implicit ctx: Context): Value = this match {
    case Neutral(Head.Meta(metaLevel), args) => ctx.metas(metaLevel) match {
      case MetaInfo.Unresolved(_) => this
      case MetaInfo.Resolved(value) => value.applySpine(args)
    }
    case _ => this
  }

  // also known as `quote` in other implementations
  def toExpr(implicit ctx: Context): CoreExpr = this.force match {
    case Primitive(value) => CoreExpr.Primitive(value)
    case PrimitiveType(ty) => CoreExpr.PrimitiveType(ty)
    case Universe(level) => CoreExpr.Universe(level)
    case Neutral(head, spine) => head.toExpr(ctx).applySpine(ctx, spine)
    case Pi(paramName, paramType, returnTypeClosure) => {
      val paramTypeExpr = paramType.toExpr(ctx)
      given newCtx: Context = ctx.addBound(paramName, paramType)
      CoreExpr.Pi(paramName, paramTypeExpr, returnTypeClosure(Value.stuckLocal(ctx.level)).toExpr(newCtx))
    }
    case Lambda(paramName, bodyClosure) => {
      val body = bodyClosure(Value.stuckLocal(ctx.level))
      // FIXME: `paramName` isn't typed `Value.Universe(0)` here,
      //  we just want the index increment.
      //  It's actual type should be a new metavariable.
      // TODO: add a `ty` method for `Value`?
      given newCtx: Context = ctx.addBound(paramName, Value.Universe(0))
      CoreExpr.Lambda(paramName, body.toExpr(newCtx))
    }
    case RecordType(fieldTypes) => {
      val fieldsExpr = fieldTypes.map {
        (fieldName, `type`) => fieldName -> `type`.toExpr(ctx)
      }
      CoreExpr.RecordType(fieldsExpr)
    }
    case RecordValue(fields) => {
      val fieldsExpr = fields.map {
        (fieldName, value) => fieldName -> value.toExpr(ctx)
      }
      CoreExpr.RecordValue(fieldsExpr)
    }
  }

//  def ty(implicit ctx: Context): Value = this match {
//    case Neutral(Head.Meta(metaLevel), spine) => ctx.metas(metaLevel) match {
//      case MetaInfo.Resolved(value) => value.applySpine(spine).ty
//      case MetaInfo.Unresolved(_) => TODO("WIP")
//    }
//    case Primitive(value) => PrimitiveType(value.ty)
//    case PrimitiveType(_) => Universe(0)
//    case Universe(level) => Universe(level + 1)
//    case Pi(paramName, paramType, returnTypeClosure) => {
//      val paramTypeUniverse = paramType.universe
//      given newCtx: Context = ctx.addBound(paramName, paramType)
//      val returnType = returnTypeClosure(Value.stuckLocal(ctx.level)).universe(newCtx)
//      Universe(paramTypeUniverse max returnType)
//    }
//    case Lambda(paramName, bodyClosure) => ???
//  }
}

type Type = Value

object Value {
  def stuckLocal(level: VarLevel): Value.Neutral = Neutral(Head.Var(level), List.empty)
}

opaque type VarLevel = Int
opaque type MetaLevel = Int

object VarLevel {
  val zero: VarLevel = 0
  def apply(level: Int): VarLevel = level
}

extension (level: VarLevel) {
  def increment: VarLevel = level + 1
  def value: Int = level
  @targetName("plus")
  def +(offset: VarLevel): VarLevel = level + offset
  @targetName("minus")
  def -(offset: VarLevel): VarLevel = level - offset
}

object MetaLevel {
  val zero: MetaLevel = 0
  def apply(level: Int): MetaLevel = level
}

enum Head {
  /**
   * De Bruijn level.
   *
   * This counts the total number of binders that we encounter when running down
   * the syntax tree from the root.
   *
   * De Bruijn levels are useful because unlike de Bruijn indices, they don't
   * need to be shifted while moving around terms under a specific scope. This
   * makes them ideal for representing values. We'll convert these back into
   * indices during read-back.
   */
  case Var(level: VarLevel)

  /**
   * Metavariable index.
   *
   * These are used as placeholders for undetermined terms that we will need to
   * eventually fill in during elaboration. They can also be used to stand for
   * 'holes' in the concrete syntax, to support type-directed editing.
   */
  case Meta(index: MetaLevel)

  // also known as `quote` in other implementations
  def toExpr(implicit ctx: Context): CoreExpr = this match {
    case Var(varLevel) => CoreExpr.Variable(ctx.level - varLevel - VarLevel(1))
    case Meta(index) => CoreExpr.Meta(index)
  }
}

type Spine = List[Elimination]

enum Elimination {
  /**
   * Application of a function to an argument.
   * `func(arg)`
   */
  case Application(arg: Value)

  /**
   * Projection of a field from a record.
   * `record.field`
   */
  case Projection(record: Value, field: String)
}

enum MetaInfo {
  case Unresolved(`type`: Value)
  case Resolved(value: Value)
}

enum Element {
  case Bound(name: String, `type`: Value)
  case Defined(name: String, `type`: Value, value: Value)
}

type ValueEnv = List[Element]
