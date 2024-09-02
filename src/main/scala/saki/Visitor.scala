package saki

import saki.grammar.SakiBaseVisitor
import saki.grammar.SakiParser.*
import saki.syntax.*
import saki.syntax.PrimitiveValue.*

import scala.jdk.CollectionConverters.*

class Visitor extends SakiBaseVisitor[Term | Seq[Term]] {

  override def visitFile(ctx: FileContext): Seq[Term] = {
    ctx.exprs.asScala.flatMap {
      case ctx: ExprImplContext => visitExprImpl(ctx)
      case expr => Seq(expr.visit)
    }.toSeq
  }

  // Expr

  extension (self: ExprContext) {
    private def visit: Term = self match {
      case ctx: ExprAtomContext => visitExprAtom(ctx)
      case ctx: ExprParenContext => visitExprParen(ctx)
      case ctx: ExprCallContext => visitExprCall(ctx)
      case ctx: ExprFieldContext => visitExprField(ctx)
      case ctx: ExprLambdaContext => visitExprLambda(ctx)
      case ctx: ExprSeqContext => visitExprSeq(ctx)
      case ctx: ExprBlockContext => visitExprBlock(ctx)
      case ctx: ExprLetContext => visitExprLet(ctx)
      case ctx: ExprInstanceContext => visitExprInstance(ctx)
      case ctx: ExprDefContext => visitExprDef(ctx)
      case ctx: ExprFunctionTypeContext => visitExprFunctionType(ctx)
      case ctx: ExprFunctionTypeImplicitContext => visitExprFunctionTypeImplicit(ctx)
      case ctx: ExprEnumTypeContext => visitExprEnumType(ctx)
      case ctx: ExprImplContext => throw new UnsupportedOperationException("impl exist only in the top scope")
    }
  }

  override def visitExprAtom(ctx: ExprAtomContext): Term = ctx.atom match {
    case context: AtomTypeContext => visitAtomType(context)
    case context: AtomOperatorContext => visitAtomOperator(context)
    case context: AtomIdentifierContext => visitAtomIdentifier(context)
    case context: AtomLiteralContext => visitAtomLiteral(context)
  }

  override def visitExprParen(ctx: ExprParenContext): Term = ctx.expr.visit

  override def visitExprCall(ctx: ExprCallContext): Term = {
    val func = ctx.func.visit
    val args = ctx.args.asScala.map(_.visit)
    args.foldLeft(func) { (acc, arg) => Term.Application(acc, arg) }
  }

  override def visitExprField(ctx: ExprFieldContext): Term = {
    val subject: Term = ctx.subject.visit
    val member: String = ctx.member.getText
    Term.Application(Term.Variable(member), subject)
  }

  override def visitExprLambda(ctx: ExprLambdaContext): Term = {
    val params = ctx.paramList.getParams
    val body = ctx.body.visit
    functionDefinition(params.map((_, ApplyMode.Explicit)), body)
  }

  override def visitExprSeq(ctx: ExprSeqContext): Term = ???

  override def visitExprBlock(ctx: ExprBlockContext): Term = {
    val statements = ctx.block.exprs.asScala.map(_.visit)
    Term.CodeBlock(statements.toList)
  }

  override def visitExprLet(ctx: ExprLetContext): Term = {
    val variable = BoundVariable(ctx.name.getText, ctx.`type`.visit)
    val value = ctx.value.visit
    Term.Let(variable, value)
  }

  override def visitExprInstance(ctx: ExprInstanceContext): Term = {
    val variable = BoundVariable("%TYPE_HASH%", ctx.`type`.visit)
    val value = ctx.value.visit
    Term.Let(variable, value)
  }

  override def visitExprDef(ctx: ExprDefContext): Term = visitDefinition(ctx.definition)

  override def visitExprImpl(ctx: ExprImplContext): Seq[Term] = {
    val sharedImplicitParams = ctx.paramList.getParams
    val functions = for functionDef <- ctx.defs.asScala yield {
      val implicitParams = functionDef.implicitParamList.getParams
      val explicitParams = functionDef.explicitParamList.getParams
      val returnType = functionDef.returnType.visit
      val body = functionDef.body.visit
      val params =
        sharedImplicitParams.map((_, ApplyMode.Implicit)) ++
        implicitParams.map((_, ApplyMode.Implicit)) ++
        explicitParams.map((_, ApplyMode.Explicit))
      functionDefinition(params, body, Some(returnType))
    }
    return functions.toSeq
  }

  override def visitExprFunctionType(ctx: ExprFunctionTypeContext): Term = {
    Term.FunctionType(ctx.lhs.visit, ApplyMode.Explicit, ctx.rhs.visit)
  }

  override def visitExprFunctionTypeImplicit(ctx: ExprFunctionTypeImplicitContext): Term = {
    Term.FunctionType(ctx.lhs.visit, ApplyMode.Implicit, ctx.rhs.visit)
  }

  override def visitExprEnumType(ctx: ExprEnumTypeContext): Term = {
    val cases = ctx.variants.asScala.map {
      case simple: EnumVariantSimpleContext => Term.Variable(simple.getText)
      case tuple: EnumVariantTupleContext => {
        val fields = tuple.elements.asScala.zipWithIndex.map { (element, index) =>
          val fieldName = s"_${index + 1}"
          (fieldName, element.visit)
        }
        Term.RecordType(fields.toList)
      }
      case record: EnumVariantRecordContext => {
        Term.RecordType(record.fields.asScala.flatMap(_.toFields).toList)
      }
    }
    Term.SumType(cases.toList)
  }

  // Definition

  override def visitDefinition(ctx: DefinitionContext): Term = {
    val name = ctx.ident.getText
    val implicitParams = ctx.implicitParamList.getParams
    val explicitParams = ctx.explicitParamList.getParams
    val returnType = ctx.returnType.visit
    val body = ctx.body.visit
    val params = implicitParams.map((_, ApplyMode.Implicit)) ++ explicitParams.map((_, ApplyMode.Explicit))
    functionDefinition(params, body, Some(returnType))
  }

  private def functionDefinition(
    params: Seq[(BoundVariable, ApplyMode)], body: Term,
    returnType: Option[Term] = None,
  ): Term = {
    params.foldRight(body) {
      case ((variable, applyMode), term) => term match {
        case Term.Function(param, mode, body, returnType) => {
          val newReturnType: Option[Term] = returnType.map(Term.FunctionType(param.`type`, mode, _))
          Term.Function(variable, applyMode, body, newReturnType)
        }
        case _ => Term.Function(variable, applyMode, term, returnType)
      }
    }
  }

  extension (self: ValueTypePairContext) {
    private def toFields: Seq[(String, Term)] = {
      val `type` = self.`type`.visit
      self.idents.asScala.map(_.getText).map((_, `type`)).toSeq
    }
  }

  // Params

  extension (self: ParamListContext) {
    private def getParams: List[BoundVariable] = self.params.asScala.flatMap { (paramsWithType: ValueTypePairContext) =>
      val `type` = paramsWithType.`type`.visit
      paramsWithType.idents.asScala.map { ident => BoundVariable(ident.toString, `type`) }
    }.toList
  }

  // Atom

  override def visitAtomType(ctx: AtomTypeContext): Term = Term.Universe(1)

  override def visitAtomIdentifier(ctx: AtomIdentifierContext): Term = Term.Variable(ctx.ident.getText)

  override def visitAtomOperator(ctx: AtomOperatorContext): Term = Term.Variable(ctx.op.getText)

  override def visitAtomLiteral(ctx: AtomLiteralContext): Term = ctx.literal match {
    case context: LiteralBoolContext => visitLiteralBool(context)
    case context: LiteralCharContext => visitLiteralChar(context)
    case context: LiteralFloatContext => visitLiteralFloat(context)
    case context: LiteralIntContext => visitLiteralInt(context)
    case context: LiteralRawStringContext => visitLiteralRawString(context)
    case context: LiteralRegularStringContext => visitLiteralRegularString(context)
  }

  // Literal

  override def visitLiteralInt(ctx: LiteralIntContext): Term = Term.PrimitiveValue(IntValue(ctx.getText.toInt))

  override def visitLiteralFloat(ctx: LiteralFloatContext): Term = Term.PrimitiveValue(FloatValue(ctx.getText.toFloat))

  override def visitLiteralBool(ctx: LiteralBoolContext): Term = Term.PrimitiveValue(BoolValue(ctx.getText.toBoolean))

  override def visitLiteralChar(ctx: LiteralCharContext): Term = Term.PrimitiveValue(CharValue(ctx.getText.charAt(1)))

  override def visitLiteralRawString(ctx: LiteralRawStringContext): Term = {
    val text = ctx.getText
    Term.PrimitiveValue(StringValue(text.stripPrefix("#").stripPrefix("{").stripSuffix("}")))
  }

  override def visitLiteralRegularString(ctx: LiteralRegularStringContext): Term = {
    val text = ctx.getText
    Term.PrimitiveValue(StringValue(text.substring(1, text.length - 1)))
  }
}
