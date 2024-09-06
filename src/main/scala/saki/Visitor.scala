package saki

import org.antlr.v4.runtime.ParserRuleContext
import saki.grammar.SakiParser.*
import saki.grammar.SakiBaseVisitor
import saki.syntax.{Term, *}
import saki.syntax.PrimitiveValue.*
import saki.optparser.*
import saki.syntax.Term.Universe

import scala.jdk.CollectionConverters.*

class Visitor extends SakiBaseVisitor[Term | Seq[Term] | Pattern | Unit] {

  var operators: Map[String, Operator] = Map.empty

  private def getBinaryOperator(symbol: String)(implicit ctx: ParserRuleContext): Operator.Binary = {
    this.operators.get(symbol) match {
      case Some(operator: Operator.Binary) => operator
      case Some(operator: Operator.Unary) => {
        throw CompileErrorException(ctx, s"Expected binary operator, found unary operator: ${symbol}")
      }
      case None => throw CompileErrorException(ctx, s"Undeclared operator: ${symbol}")
    }
  }

  private def getOperator(symbol: String)(implicit ctx: ParserRuleContext): Operator = {
    this.operators.get(symbol) match {
      case Some(operator) => operator
      case None => throw CompileErrorException(ctx, s"Undeclared operator: ${symbol}")
    }
  }

  override def visitProgram(ctx: ProgramContext): Seq[Term] = {
    ctx.stmts.asScala.map(_.visit).flatMap {
      case stmt: Term => Seq(stmt)
      case stmts: Seq[Term] => stmts
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
      case ctx: ExprIfContext => visitExprIf(ctx)
      case ctx: ExprMatchContext => visitExprMatch(ctx)
      case ctx: ExprFunctionTypeContext => visitExprFunctionType(ctx)
      case ctx: ExprFunctionTypeImplicitContext => visitExprFunctionTypeImplicit(ctx)
      case _ => throw CompileErrorException(self, s"Unsupported expression: ${self.getText}")
    }
  }

  override def visitExprAtom(ctx: ExprAtomContext): Term = ctx.atom match {
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
    args.foldLeft(func) { (acc, arg) => Term.Application(acc, ApplyMode.Explicit, arg) }
  }

  override def visitExprMemberAccess(ctx: ExprMemberAccessContext): Term = {
    given ParserRuleContext = ctx
    val subject: Term = ctx.subject.visit
    val member: String = ctx.member.getText
    Term.Application(Term.Variable(member), ApplyMode.Explicit, subject)
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
    function(params.map((_, ApplyMode.Explicit)), body)
  }

  // override def visitExprSeq(ctx: ExprSeqContext): Term = ???

  override def visitExprBlock(ctx: ExprBlockContext): Term = {
    given ParserRuleContext = ctx
    val statements = ctx.block.stmts.asScala.map(_.visit)
    Term.CodeBlock(statements.flatMap {
      case stmt: Term => Seq(stmt)
      case stmts: Seq[Term] => stmts
    }.toList)
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

  override def visitExprFunctionType(ctx: ExprFunctionTypeContext): Term = {
    given ParserRuleContext = ctx
    Term.FunctionType(ctx.lhs.visit, ApplyMode.Explicit, ctx.rhs.visit)
  }

  override def visitExprFunctionTypeImplicit(ctx: ExprFunctionTypeImplicitContext): Term = {
    given ParserRuleContext = ctx
    Term.FunctionType(ctx.lhs.visit, ApplyMode.Implicit, ctx.rhs.visit)
  }

  // Statement

  extension (self: StmtContext) {
    private def visit: Term | Seq[Term] = self match {
      case ctx: StmtExprContext => visitStmtExpr(ctx)
      case ctx: StmtLetContext => visitStmtLet(ctx)
      case ctx: StmtInstanceContext => visitStmtInstance(ctx)
      case ctx: StmtDefContext => visitStmtDef(ctx)
      case ctx: StmtImplContext => visitStmtImpl(ctx)
      case ctx: StmtEnumContext => visitStmtEnum(ctx)
    }
  }

  override def visitStmtExpr(ctx: StmtExprContext): Term = ctx.expr.visit

  override def visitStmtLet(ctx: StmtLetContext): Term = {
    given ParserRuleContext = ctx
    val ty = if ctx.`type` == null then None else Some(ctx.`type`.visit)
    Term.Let(ctx.name.getText, ty, ctx.value.visit)
  }

  override def visitStmtInstance(ctx: StmtInstanceContext): Term = {
    given ParserRuleContext = ctx
    Term.Let("%TYPE_HASH%", Some(ctx.`type`.visit), ctx.value.visit)
  }

  override def visitStmtDef(ctx: StmtDefContext): Term = visitDefinition(ctx.definition)

  override def visitStmtImpl(ctx: StmtImplContext): Seq[Term] = {
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
      function(params, body, Some(returnType))
    }
    return functions.toSeq
  }

  /**
   *
   * {{{
   *     enum Option[T: 'Type] {
   *         case None
   *         case Some(T)
   *     }
   * }}}
   *    ====[Converts To]====
   * {{{
   *     def $Option::None[T: 'Type] = record {
   *         _$Option::None: Unit
   *     }
   *     def $Option::Some[T: 'Type] = record {
   *         _$Option::Some: Unit,
   *         _1: T,
   *     }
   *     def Option[T: 'Type]: 'Type = $Option::None[T] | $Option::Some[T]
   * }}}
   *
   *    */
  override def visitStmtEnum(ctx: StmtEnumContext): Seq[Term] = {
    given ParserRuleContext = ctx

    case class EnumVariant(name: String, fields: List[(String, Term)], definitions: Seq[Term.Let])

    val enumName = ctx.ident.getText
    val implicitParams = ctx.implicitParamList.getParams
    val explicitParams = ctx.explicitParamList.getParams
    val params = implicitParams.map((_, ApplyMode.Implicit)) ++ explicitParams.map((_, ApplyMode.Explicit))
    val variants = ctx.variants.asScala.map { variant =>
      val variantName = "$" + s"$enumName::${variant.ident.getText}"
      val variantPhantomField = (s"_$variantName", Term.PrimitiveValue(UnitValue))
      val variantFields = variant.data match {
        case null => Seq()
        case tuple: EnumVariantDataTupleContext => {
          tuple.elements.asScala.zipWithIndex.map { (element, index) => (s"_${index + 1}", element.visit) }
        }
        case record: EnumVariantDataRecordContext => record.fields.asScala.flatMap(_.toFields)
      }
      val fields = variantPhantomField +: variantFields
      val recordType = Term.RecordType(fields.toList)
      val variantFunc = function(params, Term.RecordValue(fields.toList), Some(Term.Universe(0)))

      var variantDefs = Seq(Term.Let(variantName, Some(recordType), variantFunc))
      if ctx.isOpen != null then {
        val updatedParams = params.map((param, mode) => (param, ApplyMode.Implicit))
        val func = function(params, Term.RecordValue(fields.toList), Some(Term.Universe(0)))
        variantDefs ++= Seq(Term.Let(variant.ident.getText, Some(recordType), func))
      }

      EnumVariant(variant.ident.getText, fields.toList, variantDefs)
    }

    val enumFuncBody = Term.SumType(variants.map { variant =>
      if params.isEmpty then {
        Term.Variable(variant.name)
      } else {
        funcApplication(Term.Variable(variant.name), params.map {
          case (param, mode) => (Term.Variable(param.name), mode)
        })
      }
    }.toList)
    val enumFunc = function(params, enumFuncBody, Some(Term.Universe(0)))

    variants.flatMap(_.definitions).toSeq :+ Term.Let(enumName, None, enumFunc)
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

    if ctx.Identifier == null then { // this is an operator definition
      val operator = getOperator(name)
      operator match {
        case Operator.Unary(_, _) => if explicitParams.length != 1 then {
          throw CompileErrorException(ctx, "Unary operator must have exactly one explicit parameter")
        }
        case Operator.Binary(_, _, _, _, _) => if explicitParams.length != 2 then {
          throw CompileErrorException(ctx, "Binary operator must have exactly two explicit parameters")
        }
      }
    }

    val returnType = ctx.returnType.visit
    val body = ctx.body.visit
    val params = implicitParams.map((_, ApplyMode.Implicit)) ++ explicitParams.map((_, ApplyMode.Explicit))
    val func = function(params, body, Some(returnType))

    Term.Let(name, func.asInstanceOf[Term.Function].getType, func)
  }

  override def visitBinaryOperator(ctx: BinaryOperatorContext): Unit = {
    given ParserRuleContext = ctx
    val associativity = ctx.associativity.getText match {
      case "left-assoc" => Associativity.Left
      case "right-assoc" => Associativity.Right
    }
    val precedenceDecl = ctx.operatorPrecedence.asScala
    val tighterThan = precedenceDecl.flatMap(_.tighterThan.asScala).map(_.getText).toSet
    val looserThan = precedenceDecl.flatMap(_.looserThan.asScala).map(_.getText).toSet
    val sameAs = precedenceDecl.flatMap(_.sameAs.asScala).map(_.getText).toSet

    // Register the operator
    operators += ctx.symbol.getText -> Operator.Binary(
      ctx.symbol.getText,
      associativity,
      tighterThan.map(getBinaryOperator),
      looserThan.map(getBinaryOperator),
      sameAs.map(getBinaryOperator),
    )
  }

  override def visitUnaryOperator(ctx: UnaryOperatorContext): Unit = {
    given ParserRuleContext = ctx
    val kind = ctx.kind.getText match {
      case "prefix" => UnaryType.Prefix
      case "postfix" => UnaryType.Postfix
    }
    operators += ctx.symbol.getText -> Operator.Unary(ctx.symbol.getText, kind)
  }

  private def function(
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

  private def funcApplication(func: Term, args: Seq[(Term, ApplyMode)])(implicit ctx: ParserRuleContext): Term = {
    args.foldLeft(func) { case (acc, (arg, mode)) => Term.Application(acc, mode, arg) }
  }

  extension (self: Term.Function) {
    // Get the type of the function, like String -> Int, not the return type
    def getType: Option[Term] = self.returnType match {
      case None => None
      case Some(returnType) => Some(Term.FunctionType(self.param.`type`, self.applyMode, returnType)(self.span.context))
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
