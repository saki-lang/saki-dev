package saki

import org.antlr.v4.runtime.ParserRuleContext
import saki.grammar.SakiParser.*
import saki.grammar.SakiBaseVisitor
import saki.syntax.*
import saki.syntax.PrimitiveValue.*
import saki.optparser.*

import scala.jdk.CollectionConverters.*

class Visitor extends SakiBaseVisitor[Term | Seq[Term] | Pattern] {

  var operators: Map[String, Operator] = Map.empty

  override def visitProgram(ctx: ProgramContext): Seq[Term] = {
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
      case ctx: ExprMemberAccessContext => visitExprMemberAccess(ctx)
      case ctx: ExprFieldProjectionContext => visitExprFieldProjection(ctx)
      case ctx: ExprTupleTypeContext => visitExprTupleType(ctx)
      case ctx: ExprTupleContext => visitExprTuple(ctx)
      case ctx: ExprLambdaContext => visitExprLambda(ctx)
      // case ctx: ExprSeqContext => visitExprSeq(ctx)
      case ctx: ExprBlockContext => visitExprBlock(ctx)
      case ctx: ExprLetContext => visitExprLet(ctx)
      case ctx: ExprIfContext => visitExprIf(ctx)
      case ctx: ExprMatchContext => visitExprMatch(ctx)
      case ctx: ExprInstanceContext => visitExprInstance(ctx)
      case ctx: ExprDefContext => visitExprDef(ctx)
      case ctx: ExprFunctionTypeContext => visitExprFunctionType(ctx)
      case ctx: ExprFunctionTypeImplicitContext => visitExprFunctionTypeImplicit(ctx)
      case ctx: ExprEnumTypeContext => visitExprEnumType(ctx)
      case ctx: ExprImplContext => throw new UnsupportedOperationException("impl exist only in the top scope")
      case _ => throw new UnsupportedOperationException(s"Unsupported expression: ${self.getText}")
    }
  }

  override def visitExprAtom(ctx: ExprAtomContext): Term = ctx.atom match {
    case context: AtomTypeContext => visitAtomType(context)
    case context: AtomOperatorContext => visitAtomOperator(context)
    case context: AtomIdentifierContext => visitAtomIdentifier(context)
    case context: AtomLiteralContext => visitAtomLiteral(context)
  }

  override def visitExprParen(ctx: ExprParenContext): Term = ctx.expr.visit

  override def visitExprTupleType(ctx: ExprTupleTypeContext): Term = {
    given ParserRuleContext = ctx
    val elements = ctx.types.asScala.map(_.visit)
    Term.RecordType(elements.zipWithIndex.map { (ty, index) =>
      val fieldType = s"_${index + 1}"
      (fieldType, ty)
    }.toList)
  }

  override def visitExprTuple(ctx: ExprTupleContext): Term = {
    given ParserRuleContext = ctx
    val elements = ctx.elements.asScala.map(_.visit)
    Term.RecordValue(elements.zipWithIndex.map { (element, index) =>
      val fieldName = s"_${index + 1}"
      (fieldName, element)
    }.toList)
  }

  override def visitExprCall(ctx: ExprCallContext): Term = {
    given ParserRuleContext = ctx
    val func = ctx.func.visit
    val args = ctx.args.asScala.map(_.visit)
    args.foldLeft(func) { (acc, arg) => Term.Application(acc, arg) }
  }

  override def visitExprMemberAccess(ctx: ExprMemberAccessContext): Term = {
    given ParserRuleContext = ctx
    val subject: Term = ctx.subject.visit
    val member: String = ctx.member.getText
    Term.Application(Term.Variable(member), subject)
  }

  override def visitExprFieldProjection(ctx: ExprFieldProjectionContext): Term = {
    given ParserRuleContext = ctx
    val record: Term = ctx.record.visit
    val field: String = ctx.field.getText
    Term.FieldProjection(record, field)
  }

  override def visitExprLambda(ctx: ExprLambdaContext): Term = {
    given ParserRuleContext = ctx
    val params = ctx.paramList.getParams
    val body = ctx.body.visit
    functionDefinition(params.map((_, ApplyMode.Explicit)), body)
  }

  // override def visitExprSeq(ctx: ExprSeqContext): Term = ???

  override def visitExprBlock(ctx: ExprBlockContext): Term = {
    given ParserRuleContext = ctx
    val statements = ctx.block.exprs.asScala.map(_.visit)
    Term.CodeBlock(statements.toList)
  }

  override def visitExprIf(ctx: ExprIfContext): Term = {
    given ParserRuleContext = ctx
    val condition = ctx.cond.visit
    val thenBranch = ctx.`then`.visit
    val elseBranch = if ctx.else_ == null then None else Some(ctx.else_.visit)
    Term.If(condition, thenBranch, elseBranch)
  }

  override def visitExprMatch(ctx: ExprMatchContext): Term = {
    given ParserRuleContext = ctx
    val scrutinee = ctx.value.visit
    val cases = ctx.cases.asScala.map { (caseCtx: MatchCaseContext) =>
      val pattern = caseCtx.pattern
      val `type` = if caseCtx.`type` != null then Some(caseCtx.`type`.visit) else None
      val body = caseCtx.value.visit
      MatchCase(pattern.visit,`type`, body)
    }
    Term.Match(scrutinee, cases.toList)
  }

  override def visitExprLet(ctx: ExprLetContext): Term = {
    given ParserRuleContext = ctx
    val ty = if ctx.`type` == null then None else Some(ctx.`type`.visit)
    Term.Let(ctx.name.getText, ty, ctx.value.visit)
  }

  override def visitExprInstance(ctx: ExprInstanceContext): Term = {
    given ParserRuleContext = ctx
    Term.Let("%TYPE_HASH%", Some(ctx.`type`.visit), ctx.value.visit)
  }

  override def visitExprDef(ctx: ExprDefContext): Term = visitDefinition(ctx.definition)

  override def visitExprImpl(ctx: ExprImplContext): Seq[Term] = {
    given ParserRuleContext = ctx
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
    given ParserRuleContext = ctx
    Term.FunctionType(ctx.lhs.visit, ApplyMode.Explicit, ctx.rhs.visit)
  }

  override def visitExprFunctionTypeImplicit(ctx: ExprFunctionTypeImplicitContext): Term = {
    given ParserRuleContext = ctx
    Term.FunctionType(ctx.lhs.visit, ApplyMode.Implicit, ctx.rhs.visit)
  }

  override def visitExprEnumType(ctx: ExprEnumTypeContext): Term = {
    given ParserRuleContext = ctx
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

  // Pattern

  extension (self: PatternContext) {
    private def visit: Pattern = self match {
      case ctx: PatternLiteralContext => visitPatternLiteral(ctx)
      case ctx: PatternVariableContext => visitPatternVariable(ctx)
      case ctx: PatternVariantContext => visitPatternVariant(ctx)
      case ctx: PatternRecordContext => visitPatternRecord(ctx)
      case ctx: PatternTupleContext => visitPatternTuple(ctx)
    }
  }

  override def visitPatternLiteral(ctx: PatternLiteralContext): Pattern = {
    given ParserRuleContext = ctx
    val valueString = ctx.literal.getText
    val value = ctx.literal match {
      case _: LiteralBoolContext => PrimitiveValue.BoolValue(valueString.toBoolean)
      case _: LiteralCharContext => PrimitiveValue.CharValue(valueString.charAt(1))
      case _: LiteralFloatContext => PrimitiveValue.FloatValue(valueString.toFloat)
      case _: LiteralIntContext => PrimitiveValue.IntValue(valueString.toInt)
      case _: LiteralRegularStringContext => PrimitiveValue.StringValue(valueString.substring(1, valueString.length - 1))
      case _: LiteralRawStringContext => {
        PrimitiveValue.StringValue(valueString.stripPrefix("#").stripPrefix("{").stripSuffix("}"))
      }
    }
    Pattern.Literal(value)
  }

  override def visitPatternVariable(ctx: PatternVariableContext): Pattern = Pattern.Variable(ctx.ident.getText)(ctx)

  override def visitPatternVariant(ctx: PatternVariantContext): Pattern = {
    given ParserRuleContext = ctx
    val variantIdent = ctx.ident.getText
    val params = ctx.elements.asScala.map(_.visit)
    Pattern.Variant(variantIdent, params.toList)
  }

  override def visitPatternRecord(ctx: PatternRecordContext): Pattern = {
    given ParserRuleContext = ctx
    val fields = ctx.fields.asScala.map { field =>
      val name = field.ident.getText
      val pattern = field.pattern.visit
      (name, pattern)
    }.toList
    Pattern.Record(fields, Some(ctx.record.visit))
  }

  override def visitPatternTuple(ctx: PatternTupleContext): Pattern = {
    given ParserRuleContext = ctx
    val patterns = ctx.elements.asScala.zipWithIndex.map { (element, index) =>
      val name = s"_${index + 1}"
      (name, element.visit)
    }.toList
    Pattern.Record(patterns, None)
  }

  // Definition

  override def visitDefinition(ctx: DefinitionContext): Term = {
    given ParserRuleContext = ctx
    val name = ctx.ident.getText
    val implicitParams = ctx.implicitParamList.getParams
    val explicitParams = ctx.explicitParamList.getParams
    val returnType = ctx.returnType.visit
    val body = ctx.body.visit
    val params = implicitParams.map((_, ApplyMode.Implicit)) ++ explicitParams.map((_, ApplyMode.Explicit))
    Term.Let(name, None, functionDefinition(params, body, Some(returnType)))
  }

  override def visitBinaryOperator(ctx: BinaryOperatorContext): Term = {
    given ParserRuleContext = ctx
    val associativity = ctx.associativity.getText match {
      case "left-assoc" => Associativity.Left
      case "right-assoc" => Associativity.Right
    }
    val precedenceDecl = ctx.operatorPrecedence.asScala
    val tighterThan = precedenceDecl.flatMap(_.tighterThan.asScala).map(_.getText).toSet
    val looserThan = precedenceDecl.flatMap(_.looserThan.asScala).map(_.getText).toSet
    val sameAs = precedenceDecl.flatMap(_.sameAs.asScala).map(_.getText).toSet
    val returnType = ctx.returnType.visit
    val implicitParams = ctx.implicitParamList.getParams
    val params = ctx.explicitParamList.getParams
    if params.length != 2 then {
      throw new IllegalArgumentException("Binary operator must have 2 parameters")
    } else {
      def getOperatorsByName(operators: Set[String]): Set[Operator.Binary] = {
        operators.map { symbol =>
          this.operators.get(symbol).flatMap {
            case operator: Operator.Binary => Some(operator)
            case _ => None
          }.orNull {
            throw new IllegalArgumentException(s"Undefined operator: ${symbol}")
          }
        }
      }
      // Register the operator
      operators += ctx.symbol.getText -> Operator.Binary(
        ctx.symbol.getText,
        associativity,
        getOperatorsByName(tighterThan),
        getOperatorsByName(looserThan),
        getOperatorsByName(sameAs),
      )
      // Define the function
      val func = functionDefinition(
        implicitParams.map((_, ApplyMode.Implicit)) ++ params.map((_, ApplyMode.Explicit)),
        ctx.body.visit, Some(returnType),
      )
      Term.Let(ctx.symbol.getText, None, func)
    }
  }

  override def visitUnaryOperator(ctx: UnaryOperatorContext): Term = {
    given ParserRuleContext = ctx
    val kind = ctx.kind.getText match {
      case "prefix" => UnaryType.Prefix
      case "postfix" => UnaryType.Postfix
    }
    val returnType = ctx.returnType.visit
    val implicitParams = ctx.implicitParamList.getParams
    val params = ctx.explicitParamList.getParams
    if params.length != 1 then {
      throw new IllegalArgumentException("Unary operator must have 1 parameter")
    } else {
      operators += ctx.symbol.getText -> Operator.Unary(ctx.symbol.getText, kind)
      val func = functionDefinition(
        implicitParams.map((_, ApplyMode.Implicit)) ++ params.map((_, ApplyMode.Explicit)),
        ctx.body.visit, Some(returnType),
      )
      Term.Let(ctx.symbol.getText, None, func)
    }
  }

  private def functionDefinition(
    params: Seq[(BoundVariable, ApplyMode)], body: Term,
    returnType: Option[Term] = None,
  )(implicit ctx: ParserRuleContext): Term = {
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
    private def getParams: List[BoundVariable] = {
      if (self == null) return Nil
      self.params.asScala.flatMap { (paramsWithType: ValueTypePairContext) =>
        val `type` = paramsWithType.`type`.visit
        paramsWithType.idents.asScala.map { ident => BoundVariable(ident.getText, `type`) }
      }.toList
    }
  }

  // Atom

  override def visitAtomType(ctx: AtomTypeContext): Term = Term.Universe(0)(ctx)

  override def visitAtomIdentifier(ctx: AtomIdentifierContext): Term = Term.Variable(ctx.ident.getText)(ctx)

  override def visitAtomOperator(ctx: AtomOperatorContext): Term = Term.Variable(ctx.op.getText)(ctx)

  override def visitAtomLiteral(ctx: AtomLiteralContext): Term = ctx.literal match {
    case context: LiteralBoolContext => visitLiteralBool(context)
    case context: LiteralCharContext => visitLiteralChar(context)
    case context: LiteralFloatContext => visitLiteralFloat(context)
    case context: LiteralIntContext => visitLiteralInt(context)
    case context: LiteralRawStringContext => visitLiteralRawString(context)
    case context: LiteralRegularStringContext => visitLiteralRegularString(context)
  }

  // Literal

  override def visitLiteralInt(ctx: LiteralIntContext): Term = Term.PrimitiveValue(IntValue(ctx.value.getText.toInt))(ctx)

  override def visitLiteralFloat(ctx: LiteralFloatContext): Term = Term.PrimitiveValue(FloatValue(ctx.value.getText.toFloat))(ctx)

  override def visitLiteralBool(ctx: LiteralBoolContext): Term = Term.PrimitiveValue(BoolValue(ctx.value.getText.toBoolean))(ctx)

  override def visitLiteralChar(ctx: LiteralCharContext): Term = Term.PrimitiveValue(CharValue(ctx.value.getText.charAt(1)))(ctx)

  override def visitLiteralRawString(ctx: LiteralRawStringContext): Term = {
    given ParserRuleContext = ctx
    val text = ctx.value.getText
    Term.PrimitiveValue(StringValue(text.stripPrefix("#").stripPrefix("{").stripSuffix("}")))
  }

  override def visitLiteralRegularString(ctx: LiteralRegularStringContext): Term = {
    given ParserRuleContext = ctx
    val text = ctx.value.getText
    Term.PrimitiveValue(StringValue(text.substring(1, text.length - 1)))
  }
}
