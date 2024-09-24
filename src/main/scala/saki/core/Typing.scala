package saki.core

import saki.fail

case class TypeTerm(`type`: Expr) {
  def expr: Expr = this.`type`
  def normalize(implicit ctx: Context): TypeTerm = TypeTerm(this.`type`.normalize)
  def eval(implicit ctx: Context): Value = this.`type`.eval
}

type ExprEnv = Map[String, Expr]
type ValueEnv = Map[String, Value]

enum Expr {

  case Primitive(value: Literal)
  case PrimitiveType(`type`: LiteralType)
  case Variable(name: String)

  /**
   * `Type_i`
   */
  case Universe(private val level: Int)

  /**
   * `∀(paramName: paramType) -> returnType`
   */
  case FuncType(paramName: String, paramType: TypeTerm, returnType: TypeTerm)

  /**
   * λ(paramName: paramType) -> body
   */
  case Lambda(paramName: String, paramType: TypeTerm, body: Expr)

  /**
   * `record { field1: type1, field2: type2, ... }`
   */
  case RecordType(fieldTypes: Map[String, TypeTerm])

  /**
   * `^{ field1 = value1, field2 = value2, ... }`
   */
  case RecordValue(fields: Map[String, Expr])

  /**
   * `func(arg)` | `func arg`
   */
  case Application(func: Expr, arg: Expr)

  /**
   * `value.member`
   */
  case Elimination(value: Expr, member: String)
  extension (elimination: Elimination) {
    def isApplication(implicit ctx: Context): Boolean = {
      ctx.typeOf(elimination.member) match {
        case Some(Value.FuncType(_, _)) => true
        case _ => false
      }
    }

    def toApplication(implicit ctx: Context): Option[Application] = {
      ctx.valueOf(elimination.member) match {
        case Some(func: Value.Lambda) => Some(Application(func.toExpr, elimination.value))
        case _ => None
      }
    }
  }

  def toType: TypeTerm = TypeTerm(this)

  def inferType(implicit ctx: Context): Value = this match {
    case Primitive(value) => Value.PrimitiveType(value.ty)
    case PrimitiveType(ty) => Value.Universe(0)
    case Variable(name) => ctx.types(name)
    case Universe(level) => Value.Universe(level)
    case FuncType(paramName, paramType, returnType) => {
      val paramTypeUniverse = paramType.expr.universeLevel(ctx)
      // add param to the context
      given Context = ctx + (paramName -> paramType.eval(ctx))
      // infer returnType type with param in the context
      val returnTypeUniverse = returnType.expr.universeLevel
      // return type level is max of param and returnType
      Value.Universe(paramTypeUniverse.max(returnTypeUniverse))
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
    case RecordType(fieldTypes) => {
      val fieldTypeValues = fieldTypes.map((_, fieldType) => fieldType.eval(ctx))
      Value.Universe(fieldTypeValues.map(_.toExpr.universeLevel(ctx)).max)
    }
    case RecordValue(fields) => {
      val fieldTypes = fields.map((name, field) => (name, field.inferType))
      Value.RecordType(fieldTypes)
    }

    /**
     * `value.member`
     * if `value` is a record with field `member`, then return the type of `member`
     * otherwise, treat `member` as a function and `value` as an argument
     */
    case Elimination(value, member) => {
      val valueType = value.inferType
      valueType match {
        case Value.RecordType(fieldTypes) => fieldTypes(member)
        case _ => {
          val argType = valueType
          // infer the type of the `member` (function)
          ctx.types.getOrElse(member, {
            fail(s"Unknown member: $member")
          }) match {
            case Value.FuncType(paramType, returnTypeClosure) => {
              if paramType.alphaEquals(ctx, argType) then {
                returnTypeClosure(value.eval)
              } else {
                fail(s"Type mismatch: $paramType != $argType")
              }
            }
            case _ => fail("Not a function")
          }
        }
      }
    }
  }

  def isType: Boolean = this match {
    case Universe(_) => true
    case _ => false
  }

  private def asType: Universe = this match {
    case ty: Universe => ty
    case _ => throw new Exception("Not a type")
  }

  def universeLevel(implicit ctx: Context): Int = {
    this.inferType match {
      case Value.Universe(level) => level
      case _ => throw new Exception("Not a type")
    }
  }

  def normalize(implicit ctx: Context): Expr = this.eval.toExpr

  def eval(implicit ctx: Context): Value = this match {
    case Primitive(value) => Value.Primitive(value)
    case PrimitiveType(ty) => Value.PrimitiveType(ty)
    case Variable(name) => ctx.values(name)
    case Universe(level) => Value.Universe(level)
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
    case RecordType(fieldTypes) => {
      val fieldTypeValues = fieldTypes.map((fieldName, fieldType) => (fieldName, fieldType.eval(ctx)))
      Value.RecordType(fieldTypeValues)
    }
    case RecordValue(fields) => {
      val fieldValues = fields.map((fieldName, field) => (fieldName, field.eval))
      Value.RecordValue(fieldValues)
    }
    case elimination: Elimination => elimination.toApplication(ctx) match {
      case Some(application: Application) => application.eval
      case None => elimination.value.eval match {
        case Value.RecordValue(fields) => fields(elimination.member)
        case _ => fail("Not a record")
      }
    }
  }
}

enum Value {
  case Primitive(value: Literal)
  case PrimitiveType(`type`: LiteralType)
  case Neutral(neutral: NeutralValue)
  case Universe(level: Int)
  case FuncType(paramType: Value, returnTypeClosure: Value => Value)
  case Lambda(paramType: Value, bodyClosure: Value => Value)
  case RecordType(fieldTypes: Map[String, Value])
  case RecordValue(fields: Map[String, Value])

  private def generateName(implicit ctx: Context, prefix: String): String = {
    // prefix + "0", "1", ...
    Iterator.from(0).map(i => s"$prefix$i").find(!ctx.names.contains(_)).get
  }

  def toExpr(implicit ctx: Context): Expr = this match {
    case Primitive(value) => Expr.Primitive(value)
    case PrimitiveType(ty) => Expr.PrimitiveType(ty)
    case Neutral(neutral) => neutral.toExpr
    case Universe(level) => Expr.Universe(level)
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
    case RecordType(fieldTypes) => {
      val fieldTypeValues = fieldTypes.map((fieldName, fieldType) => (fieldName, fieldType.toExpr.toType))
      Expr.RecordType(fieldTypeValues)
    }
    case RecordValue(fields) => {
      val fieldValues = fields.map((fieldName, fieldValue) => (fieldName, fieldValue.toExpr))
      Expr.RecordValue(fieldValues)
    }
  }

  def alphaEquals(implicit ctx: Context, other: Value): Boolean = {
    this.toExpr == other.toExpr
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
