package saki.core

import saki.fail

import scala.annotation.targetName

case class TypeTerm(`type`: Expr) {
  def expr: Expr = this.`type`
  def subst(name: String, substTo: Expr): TypeTerm = TypeTerm(this.`type`.subst(name, substTo))
  def normalize: TypeTerm = TypeTerm(this.`type`.normalize)
  def alphaEquivalent(other: TypeTerm, boundVariables: Map[String, String]): Boolean = {
    this.`type`.alphaEquivalent(other.`type`, boundVariables)
  }
}

type Env = Map[String, Expr]

enum Expr {
  case Variable(name: String)
  case Type(private val level: Int)
  case FuncType(paramName: String, paramType: TypeTerm, returnType: TypeTerm)
  case Lambda(paramName: String, paramType: TypeTerm, body: Expr)
  case Application(func: Expr, arg: Expr)

  def toType: TypeTerm = TypeTerm(this)

  def inferType(implicit env: Map[String, Expr]): TypeTerm = this match {
    case Variable(name) => env(name).toType
    case Type(level) => TypeTerm(Type(level + 1))
    case FuncType(paramName, paramType, returnType) => {
      val paramTypeUniverse = paramType.expr.universeLevel(env)
      // add param to env
      given Env = env + (paramName -> paramType.expr)
      // infer returnType
      val returnTypeUniverse = returnType.expr.universeLevel
      // return type level is max of param and returnType
      TypeTerm(Type(paramTypeUniverse.max(returnTypeUniverse)))
    }
    case Lambda(paramName, paramType, body) => {
      val paramTypeUniverse = paramType.expr.universeLevel(env)
      given Env = env + (paramName -> paramType.expr)
      val returnTypeUniverse = body.inferType.expr.universeLevel
      TypeTerm(Type(paramTypeUniverse.max(returnTypeUniverse)))
    }
    case Application(func, arg) => {
      val funcType: Expr = func.inferType(env).expr
      val argType: Expr = arg.inferType(env).expr
      funcType.normalize match {
        case FuncType(paramName, paramType, returnType) => {
          if (paramType.expr == argType) {
            returnType.subst(paramName, arg)
          } else {
            throw new Exception("Type mismatch")
          }
        }
        case _ => throw new Exception("Not a function")
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

  def universeLevel(implicit env: Env): Int = {
    this.inferType(env).normalize.expr match {
      case Type(level) => level
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

  def normalize: Expr = this match {
    case Variable(_) => this
    case Type(_) => this
    case FuncType(paramName, paramType, returnType) => {
      FuncType(paramName, paramType.normalize, returnType.normalize)
    }
    case Lambda(paramName, paramType, body) => {
      Lambda(paramName, paramType.normalize, body.normalize)
    }
    case Application(func, arg) => {
      func.normalize match {
        case Lambda(paramName, paramType, body) => {
          body.subst(paramName, arg).normalize
        }
        case func => Application(func, arg.normalize)
      }
    }
  }

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

  @targetName("alphaEquals")
  def ===(other: Expr): Boolean = {
    this.normalize.alphaEquivalent(other.normalize, Map.empty)
  }
}


