package saki.core

trait SurfaceSyntax
enum SurfaceExpr extends SurfaceSyntax {

  case Primitive(value: Literal)
  case PrimitiveType(`type`: LiteralType)
  case Variable(name: String)

  case Annotated(expr: SurfaceExpr, `type`: SurfaceExpr)

  /**
   * `let name = value; body`
   * `let name = (value: type); body`
   */
  case Let(name: String, value: SurfaceExpr, body: SurfaceExpr)

  /**
   * `Type_i`
   */
  case Universe(private val level: Int)

  /**
   * `∀(paramName: paramType) -> returnType`
   */
  case FuncType(paramName: String, paramType: SurfaceExpr, returnType: SurfaceExpr)

  /**
   * λ(paramName: paramType) -> body
   */
  case Lambda(paramName: String, paramType: Option[SurfaceExpr], body: SurfaceExpr)

  /**
   * `record { field1: type1, field2: type2, ... }`
   */
  case RecordType(fieldTypes: Map[String, SurfaceExpr])

  /**
   * `^{ field1 = value1, field2 = value2, ... }`
   */
  case RecordValue(fields: Map[String, SurfaceExpr])

  /**
   * `func(arg)` | `func arg`
   */
  case Application(func: SurfaceExpr, arg: SurfaceExpr)

  /**
   * `value.member`
   */
  case Elimination(value: SurfaceExpr, member: String)

  def infer(implicit ctx: Context): InferResult = this match {
    case Primitive(value) => InferResult(CoreExpr.Primitive(value), Value.PrimitiveType(value.ty))
    case PrimitiveType(ty) => InferResult(CoreExpr.PrimitiveType(ty), Value.Universe(0))
    case Variable(name) => {
      val ContextVariable(index, ty) = ctx(name)
      InferResult(CoreExpr.Variable(index), ty)
    }
    case Let(name, value, body) => {
      // TODO: recursive should be handled here
      val (valueExpr, valueType) = value.infer(ctx).unpack
      given Context = ctx.addDefined(name, valueType, valueExpr.eval(ctx))
      val (bodyExpr, bodyType) = body.infer.unpack
      InferResult(CoreExpr.Let(name, valueExpr, bodyExpr), bodyType)
    }
    case Universe(level) => InferResult(CoreExpr.Universe(level), Value.Universe(level + 1))
    case Annotated(expr, expectedTypeExpr) => {
      val (exprCore: CoreExpr, exprType) = expr.infer.unpack
      val expectedType = expectedTypeExpr.synthCore.eval
      InferResult(expr.synthCoreChecked(ctx, expectedType), expectedType)
    }
    case FuncType(paramName, paramType, returnType) => {
      val (paramTypeExpr, _) = paramType.infer(ctx).unpack
      given Context = ctx.addBound(paramName, paramTypeExpr.eval(ctx))
      val (returnTypeExpr, _) = returnType.infer(ctx).unpack
      InferResult(CoreExpr.Pi(paramName, paramTypeExpr, returnTypeExpr), Value.Universe(0))
    }
  }

  def universe(implicit ctx: Context): Int = {
    this.infer.`type` match {
      case Value.Universe(level) => level
      case _ => fail("Expected universe type")
    }
  }

  // also known as `check_type` in other implementations
  def synthCore(implicit ctx: Context): CoreExpr = {
    val (expr: CoreExpr, typ: Type) = this.infer(ctx).unpack
    if !ctx.unify(expr.eval(ctx), typ) then {
      fail(s"Type mismatch: $expr : $typ")
    } else expr
  }

  // also known as `check` in other implementations
  def synthCoreChecked(implicit ctx: Context, expectedType: Type): CoreExpr = {
    given Context = ctx
    (this, expectedType.force) match {
      case (Let(name, term, body), ty) => {
        val (termCore: CoreExpr, termType: Type) = term.infer.unpack
        val bodyCore = body.synthCoreChecked(ctx.addDefined(name, termType, termCore.eval), ty)
        CoreExpr.Let(name, termCore, bodyCore)
      }
      case (Lambda(argName, paramTypeSurface, body), Value.Pi(_, expectedParamType, returnTypeClosure)) => {
        val argType = paramTypeSurface match {
          case Some(paramTypeSurface) => {
            val paramType = paramTypeSurface.synthCore.eval
            ctx.unify(paramType, expectedParamType)
            paramType
          }
          case None => expectedParamType
        }
        val bodyCore = body.synthCoreChecked(
          ctx = ctx.addBound(argName, argType),
          expectedType = returnTypeClosure(Value.stuckLocal(ctx.level))
        )
        CoreExpr.Lambda(argName, bodyCore)
      }
      // TODO: other cases
    }
  }
}

case class InferResult(expr: CoreExpr, `type`: Type) {
  def unpack: (CoreExpr, Type) = (expr, `type`)
}