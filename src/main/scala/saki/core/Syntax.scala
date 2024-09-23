package saki.core

import saki.fail

import scala.annotation.targetName

case class Context(
  values: Map[String, Value] = Map.empty,
  types: Map[String, Value] = Map.empty,
) {
  def names: Set[String] = this.values.keySet

  def addVariable(name: String, `type`: Value): Context = {
    val value = Value.Neutral(NeutralValue.Variable(name))
    this.copy(values = this.values + (name -> value), types = this.types + (name -> `type`))
  }

  @targetName("addVariable")
  def +(entry: (String, Value)): Context = this.addVariable(entry._1, entry._2)

}

case class TypeTerm(`type`: Expr) {
  def expr: Expr = this.`type`
  def subst(name: String, substTo: Expr): TypeTerm = TypeTerm(this.`type`.subst(name, substTo))
  def normalize(implicit ctx: Context): TypeTerm = TypeTerm(this.`type`.normalize)
  def alphaEquivalent(other: TypeTerm, boundVariables: Map[String, String]): Boolean = {
    this.`type`.alphaEquivalent(other.`type`, boundVariables)
  }
  def eval(implicit ctx: Context): Value = this.`type`.eval
}

type ExprEnv = Map[String, Expr]
type ValueEnv = Map[String, Value]

enum Expr {
  case Variable(name: String)
  case Type(private val level: Int)
  case FuncType(paramName: String, paramType: TypeTerm, returnType: TypeTerm)
  case Lambda(paramName: String, paramType: TypeTerm, body: Expr)
  case Application(func: Expr, arg: Expr)

  def toType: TypeTerm = TypeTerm(this)

  def inferType(implicit ctx: Context): Value = this match {
    case Variable(name) => ctx.types(name)
    case Type(level) => Value.Type(level)
    case FuncType(paramName, paramType, returnType) => {
      val paramTypeUniverse = paramType.expr.universeLevel(ctx)
      // add param to the context
      given Context = ctx + (paramName -> paramType.eval(ctx))
      // infer returnType type with param in the context
      val returnTypeUniverse = returnType.expr.universeLevel
      // return type level is max of param and returnType
      Value.Type(paramTypeUniverse.max(returnTypeUniverse))
    }
    case Lambda(paramName, paramType, body) => {
      val _ = paramType.expr.universeLevel(ctx) // check if paramType is a type
      val paramTypeValue = paramType.eval(ctx)
      // add param to the context
      given newCtx: Context = ctx + (paramName -> paramType.eval(ctx))
      val returnTypeValue = body.inferType
      val returnTypeExpr = returnTypeValue.toExpr
      Value.FuncType(paramTypeValue, arg => {
        returnTypeExpr.eval(newCtx + (paramName -> arg))
      })
    }
    case Application(func, arg) => {
      val funcType: Value = func.inferType
      val argType: Value = arg.inferType
      funcType match {
        case Value.FuncType(paramType, returnTypeClosure) => {
          if paramType.alphaEquals(ctx, argType) then {
            returnTypeClosure(arg.eval)
          } else {
            fail(s"Type mismatch: $paramType != $argType")
          }
        }
        case _ => fail("Not a function")
      }
    }
  }

  def isType: Boolean = this match {
    case Type(_) => true
    case _ => false
  }

  private def asType: Type = this match {
    case ty: Type => ty
    case _ => throw new Exception("Not a type")
  }

  def universeLevel(implicit ctx: Context): Int = {
    this.inferType match {
      case Value.Type(level) => level
      case _ => throw new Exception("Not a type")
    }
  }

  def subst(name: String, substTo: Expr): Expr = this match {
    case Variable(n) if n == name => substTo
    case Variable(_) => this
    case Lambda(paramName, paramType, body) => {
      if paramName == name then {
        Expr.Lambda(paramName, paramType.subst(name, substTo), body)
      } else if substTo.isVariableOccurred(name) then {
        fail(s"Variable $name occurs in $substTo")
      } else {
        Lambda(paramName, paramType.subst(name, substTo), body.subst(name, substTo))
      }
    }
    case FuncType(paramName, paramType, returnType) => {
      if paramName == name then {
        FuncType(paramName, paramType.subst(name, substTo), returnType.subst(name, substTo))
      } else if substTo.isVariableOccurred(name) then {
        fail(s"Variable $name occurs in $substTo")
      } else {
        FuncType(paramName, paramType.subst(name, substTo), returnType.subst(name, substTo))
      }
    }
    case Type(_) => this
    case Application(func, arg) => Application(func.subst(name, substTo), arg.subst(name, substTo))
  }

  def isVariableOccurred(name: String): Boolean = this match {
    case Variable(n) => n == name
    case Type(_) => false
    case FuncType(paramName, paramType, returnType) =>
      paramName != name && paramType.expr.isVariableOccurred(name) && returnType.expr.isVariableOccurred(name)
    case Lambda(paramName, paramType, body) =>
      paramName != name && paramType.expr.isVariableOccurred(name) && body.isVariableOccurred(name)
    case Application(func, arg) => func.isVariableOccurred(name) || arg.isVariableOccurred(name)
  }

  def normalize(implicit ctx: Context): Expr = this.eval.toExpr

  def alphaEquivalent(other: Expr, boundVariables: Map[String, String]): Boolean = (this, other) match {
    case (Variable(name1), Variable(name2)) => boundVariables.get(name1) match {
      case Some(name) => name == name2
      case None => name1 == name2
    }
    case (Type(level1), Type(level2)) => level1 == level2
    case (FuncType(paramName1, paramType1, returnType1), FuncType(paramName2, paramType2, returnType2)) => {
      paramType1.alphaEquivalent(paramType2, boundVariables) &&
        returnType1.alphaEquivalent(returnType2, boundVariables + (paramName1 -> paramName2))
    }
    case (Lambda(paramName1, paramType1, body1), Lambda(paramName2, paramType2, body2)) => {
      paramType1.alphaEquivalent(paramType2, boundVariables) &&
        body1.alphaEquivalent(body2, boundVariables + (paramName1 -> paramName2))
    }
    case (Application(func1, arg1), Application(func2, arg2)) => {
      func1.alphaEquivalent(func2, boundVariables) && arg1.alphaEquivalent(arg2, boundVariables)
    }
    case _ => false
  }

  def ===(other: Expr): Boolean = {
    this.alphaEquivalent(other, Map.empty)
  }

  def eval(implicit ctx: Context): Value = this match {
    case Variable(name) => ctx.values(name)
    case Type(level) => Value.Type(level)
    case FuncType(paramName, paramType, returnType) => {
      Value.FuncType(paramType.eval, (arg: Value) => {
        given Context = ctx + (paramName -> arg)
        returnType.eval
      })
    }
    case Lambda(paramName, paramType, body) => {
      Value.Lambda(paramType.eval, (arg: Value) => {
        given Context = ctx + (paramName -> arg)
        body.eval
      })
    }
    case Application(func, arg) => {
      val funcValue = func.eval
      val argValue = arg.eval
      funcValue match {
        case Value.Lambda(paramType, bodyClosure) => bodyClosure(argValue)
        case Value.Neutral(funcNeutral) => Value.Neutral(NeutralValue.Application(funcNeutral, argValue))
        case _ => fail("Not a function")
      }
    }
  }
}

enum Value {
  case Neutral(neutral: NeutralValue)
  case Type(level: Int)
  case FuncType(paramType: Value, returnTypeClosure: Value => Value)
  case Lambda(paramType: Value, bodyClosure: Value => Value)

  private def generateName(implicit ctx: Context, prefix: String): String = {
    // prefix + "0", "1", ...
    Iterator.from(0).map(i => s"$prefix$i").find(!ctx.names.contains(_)).get
  }

  def toExpr(implicit ctx: Context): Expr = this match {
    case Neutral(neutral) => neutral.toExpr
    case Type(level) => Expr.Type(level)
    case FuncType(paramType, returnTypeClosure) => {
      val paramName = generateName(ctx, "dep_")
      val paramValue = NeutralValue.Variable(paramName).toValue
      Expr.FuncType(
        paramName,
        paramType = TypeTerm(paramType.toExpr),
        returnType = TypeTerm(
          returnTypeClosure(paramValue).toExpr(ctx + (paramName -> paramValue))
        )
      )
    }
    case Lambda(paramType, bodyClosure) => {
      val paramName = generateName(ctx, "param_")
      val paramValue = NeutralValue.Variable(paramName).toValue
      Expr.Lambda(
        paramName,
        paramType = TypeTerm(paramType.toExpr),
        body = bodyClosure(paramValue).toExpr(ctx + (paramName -> paramValue))
      )
    }
  }

  def alphaEquals(implicit ctx: Context, other: Value): Boolean = {
    this.toExpr === other.toExpr
  }
}

enum NeutralValue {
  case Variable(name: String)
  case Application(func: NeutralValue, arg: Value)

  def toValue: Value = Value.Neutral(this)

  def toExpr(implicit ctx: Context): Expr = this match {
    case Variable(name) => Expr.Variable(name)
    case Application(func, arg) => Expr.Application(func.toExpr, arg.toExpr)
  }
}
