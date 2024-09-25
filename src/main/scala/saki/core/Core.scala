package saki.core

trait CoreSyntax
enum CoreExpr extends CoreSyntax {
  case Primitive(value: Literal)
  case PrimitiveType(`type`: LiteralType)
  case Variable(index: VarLevel)
  case Meta(level: MetaLevel)
  case Universe(level: Int)
  case Let(name: String, value: CoreExpr, body: CoreExpr)
  case Pi(paramName: String, paramType: CoreExpr, returnType: CoreExpr)
  case Lambda(paramName: String, body: CoreExpr)
  case Application(func: CoreExpr, arg: CoreExpr)
  case RecordType(fieldTypes: Map[String, CoreExpr])
  case RecordValue(fields: Map[String, CoreExpr])
  case Projection(record: CoreExpr, member: String)

  def eval(implicit ctx: Context): Value = ???

  // also known as `quote_spine` in other implementations
  def applySpine(implicit ctx: Context, spine: Spine): CoreExpr = spine match {
    case Nil => this
    case Elimination.Application(arg) :: rest => CoreExpr.Application(this.applySpine(ctx, rest), arg.toExpr)
    case Elimination.Projection(record, field) :: rest => CoreExpr.Projection(this.applySpine(ctx, rest), field)
  }
}
