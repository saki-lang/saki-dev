package saki

sealed trait Expr
sealed trait Type extends Expr {
  def asExpr: Expr = this.asInstanceOf[Expr]
}

case class Var(name: String) extends Expr
case class AtomType(universe: Int) extends Type
case class FuncType(paramName: String, paramType: Expr, returnType: Expr) extends Type
case class Func(paramName: String, paramType: Expr, body: Expr) extends Expr
case class Apply(func: Expr, arg: Expr) extends Expr

enum Neutral {
  case NeutralVar(name: String)
  case NeutralApply(func: Neutral, arg: Value)
}

enum Value {
  case ValueNeutral(neutral: Neutral)
  case ValueType(universe: Int)
  case ValueFunc(name: String, func: Value => Value)
  case ValueFuncType(name: String, func: Value => Value)
}

import Neutral._
import Value._

extension (self: Expr) {

  def eval(implicit env: Map[String, Value]): Value = self match {
    case Var(name) => env(name)
    case AtomType(universe) => ValueType(universe)
    case FuncType(paramName, paramType, returnType) => ValueFuncType(paramName, (arg: Value) => {
      returnType.eval(env + (paramName -> arg))
    })
    case Func(paramName, paramType, body) => ValueFunc(paramName, (arg: Value) => {
      body.eval(env + (paramName -> arg))
    })
  }


//  def inferType(env: Map[String, Expr]): Type = self match {
//    case Var(name) => env(name)
//    case AtomType(universe) => AtomType(universe + 1)
//
//    case FuncType(name, argument, result) => {
//      val argType: Type = argument.inferType(env)
//      val resultType: Type = result.inferType(env + (name -> argType.asExpr))
//      assert(argType.isInstanceOf[AtomType])
//      val argUniv = argType.asInstanceOf[AtomType].universe
//      assert(resultType.isInstanceOf[AtomType])
//      val returnUniv = resultType.asInstanceOf[AtomType].universe
//      AtomType(Math.max(argUniv, returnUniv))
//    }
//
//    case Lambda(paramName, paramType, body) => {
//      val bodyType = body.inferType(env + (paramName -> paramType))
//      FuncType(paramName, paramType, bodyType)
//    }
//
//    case Apply(func, arg) => {
//      val funcType = func.inferType(env)
//      funcType match {
//        case FuncType(name, argType, resultType) => {
//          val argTypeInferred = arg.inferType(env)
//          if (argTypeInferred == argType) {
//            resultType
//          } else {
//            throw new Exception("Type mismatch")
//          }
//        }
//        case _ => throw new Exception("Type mismatch")
//      }
//    }
//  } // end inferType
}