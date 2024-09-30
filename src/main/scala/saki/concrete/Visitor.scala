//package saki.concrete
//
//import org.antlr.v4.runtime.ParserRuleContext
//import saki.CompileErrorException
//import saki.concrete.syntax.{MatchCase, Pattern}
//import saki.core.syntax.Literal as LiteralValue
//import saki.core.syntax.Literal.*
//import saki.grammar.SakiBaseVisitor
//import saki.grammar.SakiParser.*
//import saki.util.*
//
//import scala.jdk.CollectionConverters.*
//
//class Visitor extends SakiBaseVisitor[Expr | Seq[Expr] | Pattern | Unit] {
//
//  var symbols: ScopedMap[String, Operator | String] = ScopedMap.empty
//
//  private def getBinaryOperator(symbol: String)(implicit ctx: ParserRuleContext): Operator.Binary = {
//    this.symbols.get(symbol) match {
//      case Some(operator: Operator.Binary) => operator
//      case Some(operator: Operator.Unary) => throw CompileErrorException(ctx, s"Expected binary operator, found unary operator: ${symbol}")
//      case Some(symbol) => throw CompileErrorException(ctx, s"Expected binary operator, found non-operator: ${symbol}")
//      case None => throw CompileErrorException(ctx, s"Undeclared operator: ${symbol}")
//    }
//  }
//
//  private def getOperator(symbol: String)(implicit ctx: ParserRuleContext): Operator = {
//    this.symbols.get(symbol) match {
//      case Some(operator: Operator) => operator
//      case Some(symbol) => throw CompileErrorException(ctx, s"Expected operator, found non-operator: ${symbol}")
//      case None => throw CompileErrorException(ctx, s"Undeclared operator: ${symbol}")
//    }
//  }
//
//  private def registerSymbol(symbol: String, subject: Operator | Option[Nothing] = None)(
//    implicit ctx: ParserRuleContext
//  ): Unit = subject match {
//    case operator: Operator => { // Operator can only be declared once
//      if this.symbols.contains(symbol) then {
//        throw CompileErrorException(ctx, s"Operator redeclaration: ${symbol}")
//      }
//      this.symbols + (symbol -> operator)
//    }
//    case _ => this.symbols + (symbol -> symbol)
//  }
//
//  private def withInScope[T](block: => T): T = {
//    this.symbols = this.symbols.enter
//    val result = block
//    this.symbols = this.symbols.exit
//    result
//  }
//
//  override def visitProgram(ctx: ProgramContext): Seq[Expr] = {
//    ctx.stmts.asScala.map(_.visit).flatMap {
//      case stmt: Expr => Seq(stmt)
//      case stmts: Seq[Expr] => stmts
//    }.toSeq
//  }
//
//  // Expr
//
//  extension (self: ExprContext) {
//    private def visit: Expr = self match {
//      case ctx: ExprAtomContext => visitExprAtom(ctx)
//      case ctx: ExprParenContext => visitExprParen(ctx)
//      case ctx: ExprCallContext => visitExprCall(ctx)
//      case ctx: ExprProjectionContext => visitExprProjection(ctx)
//      case ctx: ExprTupleTypeContext => visitExprTupleType(ctx)
//      case ctx: ExprTupleContext => visitExprTuple(ctx)
//      case ctx: ExprLambdaContext => visitExprLambda(ctx)
//      // case ctx: ExprSeqContext => visitExprSeq(ctx)
//      case ctx: ExprBlockContext => visitExprBlock(ctx)
//      case ctx: ExprIfContext => visitExprIf(ctx)
//      case ctx: ExprMatchContext => visitExprMatch(ctx)
//      case ctx: ExprFunctionTypeContext => visitExprFunctionType(ctx)
//      case ctx: ExprFunctionTypeImplicitContext => visitExprFunctionTypeImplicit(ctx)
//      case _ => throw CompileErrorException(self, s"Unsupported expression: ${self.getText}")
//    }
//  }
//
//  override def visitExprAtom(ctx: ExprAtomContext): Expr = ctx.atom match {
//    case context: AtomOperatorContext => visitAtomOperator(context)
//    case context: AtomIdentifierContext => visitAtomIdentifier(context)
//    case context: AtomLiteralContext => visitAtomLiteral(context)
//  }
//
//  override def visitExprParen(ctx: ExprParenContext): Expr = ctx.expr.visit
//
//  override def visitExprTupleType(ctx: ExprTupleTypeContext): Expr = {
//    given ParserRuleContext = ctx
//    val elements = ctx.types.asScala.map(_.visit)
//    Expr.RecordType(elements.zipWithIndex.map { (ty, index) =>
//      val fieldType = s"_${index + 1}"
//      (fieldType, ty)
//    }.toList)
//  }
//
//  override def visitExprTuple(ctx: ExprTupleContext): Expr = {
//    given ParserRuleContext = ctx
//    val elements = ctx.elements.asScala.map(_.visit)
//    Expr.RecordValue(elements.zipWithIndex.map { (element, index) =>
//      val fieldName = s"_${index + 1}"
//      (fieldName, element)
//    }.toList)
//  }
//
//  override def visitExprCall(ctx: ExprCallContext): Expr = {
//    given ParserRuleContext = ctx
//    val func = ctx.func.visit
//    val args = ctx.args.asScala.map(_.visit)
//    args.foldLeft(func) { (acc, arg) => Expr.Application(acc, ApplyMode.Explicit, arg) }
//  }
//
//  override def visitExprProjection(ctx: ExprProjectionContext): Expr = {
//    given ParserRuleContext = ctx
//    val subject: Expr = ctx.subject.visit
//    val field: String = ctx.field.getText
//    Expr.Projection(subject, field)
//  }
//
//  override def visitExprLambda(ctx: ExprLambdaContext): Expr = {
//    given ParserRuleContext = ctx
//    val params = ctx.paramList.getParams
//    val body = ctx.body.visit
//    function(params.map((_, ApplyMode.Explicit)), body)
//  }
//
//  // override def visitExprSeq(ctx: ExprSeqContext): Term = ???
//
//  override def visitExprBlock(ctx: ExprBlockContext): Expr = withInScope {
//    given ParserRuleContext = ctx
//    val statements = ctx.block.stmts.asScala.map(_.visit)
//    Expr.CodeBlock(statements.flatMap {
//      case stmt: Expr => Seq(stmt)
//      case stmts: Seq[Expr] => stmts
//    }.toList)
//  }
//
//  override def visitExprIf(ctx: ExprIfContext): Expr = {
//    given ParserRuleContext = ctx
//    val condition = ctx.cond.visit
//    val thenBranch = ctx.`then`.visit
//    val elseBranch = if ctx.else_ == null then None else Some(ctx.else_.visit)
//    Expr.If(condition, thenBranch, elseBranch)
//  }
//
//  override def visitExprMatch(ctx: ExprMatchContext): Expr = {
//    given ParserRuleContext = ctx
//    val scrutinee = ctx.value.visit
//    val cases = ctx.cases.asScala.map { (caseCtx: MatchCaseContext) =>
//      val pattern = caseCtx.pattern
//      val `type` = if caseCtx.`type` != null then Some(caseCtx.`type`.visit) else None
//      val body = caseCtx.value.visit
//      MatchCase(pattern.visit,`type`, body)
//    }
//    Expr.Match(scrutinee, cases.toList)
//  }
//
//  override def visitExprFunctionType(ctx: ExprFunctionTypeContext): Expr = {
//    given ParserRuleContext = ctx
//    Expr.FunctionType(ctx.lhs.visit, ApplyMode.Explicit, ctx.rhs.visit)
//  }
//
//  override def visitExprFunctionTypeImplicit(ctx: ExprFunctionTypeImplicitContext): Expr = {
//    given ParserRuleContext = ctx
//    Expr.FunctionType(ctx.lhs.visit, ApplyMode.Implicit, ctx.rhs.visit)
//  }
//
//  // Statement
//
//  extension (self: StmtContext) {
//    private def visit: Expr | Seq[Expr] = self match {
//      case ctx: StmtExprContext => visitStmtExpr(ctx)
//      case ctx: StmtLetContext => visitStmtLet(ctx)
//      case ctx: StmtInstanceContext => visitStmtInstance(ctx)
//      case ctx: StmtDefContext => visitStmtDef(ctx)
//      case ctx: StmtImplContext => visitStmtImpl(ctx)
//      case ctx: StmtEnumContext => visitStmtEnum(ctx)
//    }
//  }
//
//  override def visitStmtExpr(ctx: StmtExprContext): Expr = ctx.expr.visit
//
//  override def visitStmtLet(ctx: StmtLetContext): Expr = {
//    given ParserRuleContext = ctx
//    val ty = if ctx.`type` == null then None else Some(ctx.`type`.visit)
//    Expr.Let(ctx.name.getText, ty, ctx.value.visit)
//  }
//
//  override def visitStmtInstance(ctx: StmtInstanceContext): Expr = {
//    given ParserRuleContext = ctx
//    Expr.Let("%TYPE_HASH%", Some(ctx.`type`.visit), ctx.value.visit)
//  }
//
//  override def visitStmtDef(ctx: StmtDefContext): Expr = visitDefinition(ctx.definition)
//
//  override def visitStmtImpl(ctx: StmtImplContext): Seq[Expr] = {
//    given ParserRuleContext = ctx
//    val sharedImplicitParams = ctx.paramList.getParams
//    val functions = for functionDef <- ctx.defs.asScala yield {
//      val implicitParams = functionDef.implicitParamList.getParams
//      val explicitParams = functionDef.explicitParamList.getParams
//      val returnType = functionDef.returnType.visit
//      val body = functionDef.body.visit
//      val params =
//        sharedImplicitParams.map((_, ApplyMode.Implicit)) ++
//          implicitParams.map((_, ApplyMode.Implicit)) ++
//          explicitParams.map((_, ApplyMode.Explicit))
//      function(params, body, Some(returnType))
//    }
//    return functions.toSeq
//  }
//
//  /**
//   *
//   * {{{
//   *     enum Option[T: 'Type] {
//   *         case None
//   *         case Some(T)
//   *     }
//   * }}}
//   *    ====[Converts To]====
//   * {{{
//   *     def $Option::None[T: 'Type] = record {
//   *         _$Option::None: Unit
//   *     }
//   *     def $Option::Some[T: 'Type] = record {
//   *         _$Option::Some: Unit,
//   *         _1: T,
//   *     }
//   *     def Option[T: 'Type]: 'Type = $Option::None[T] | $Option::Some[T]
//   * }}}
//   *
//   *    */
//  override def visitStmtEnum(ctx: StmtEnumContext): Seq[Expr] = {
//    given ParserRuleContext = ctx
//
//    case class EnumVariant(name: String, fields: List[(String, Expr)], definitions: Seq[Expr.Let])
//
//    val enumName = ctx.ident.getText
//    val implicitParams = ctx.implicitParamList.getParams
//    val explicitParams = ctx.explicitParamList.getParams
//    val params = implicitParams.map((_, ApplyMode.Implicit)) ++ explicitParams.map((_, ApplyMode.Explicit))
//    val variants = ctx.variants.asScala.map { variant =>
//      val variantName = "$" + s"$enumName::${variant.ident.getText}"
//      val variantPhantomField = (s"_$variantName", Expr.PrimitiveValue(UnitValue))
//      val variantFields = variant.data match {
//        case null => Seq()
//        case tuple: EnumVariantDataTupleContext => {
//          tuple.elements.asScala.zipWithIndex.map { (element, index) => (s"_${index + 1}", element.visit) }
//        }
//        case record: EnumVariantDataRecordContext => record.fields.asScala.flatMap(_.toFields)
//      }
//      val fields = variantPhantomField +: variantFields
//      val recordType = Expr.RecordType(fields.toList)
//      val variantFunc = function(params, Expr.RecordValue(fields.toList), Some(Expr.Universe(0)))
//
//      var variantDefs = Seq(Expr.Let(variantName, Some(recordType), variantFunc))
//      if ctx.isOpen != null then {
//        val updatedParams = params.map((param, mode) => (param, ApplyMode.Implicit))
//        val func = function(params, Expr.RecordValue(fields.toList), Some(Expr.Universe(0)))
//        variantDefs ++= Seq(Expr.Let(variant.ident.getText, Some(recordType), func))
//      }
//
//      EnumVariant(variant.ident.getText, fields.toList, variantDefs)
//    }
//
//    val enumFuncBody = Expr.SumType(variants.map { variant =>
//      if params.isEmpty then {
//        Expr.Variable(variant.name)
//      } else {
//        funcApplication(Expr.Variable(variant.name), params.map {
//          case (param, mode) => (Expr.Variable(param.name), mode)
//        })
//      }
//    }.toList)
//    val enumFunc = function(params, enumFuncBody, Some(Expr.Universe(0)))
//
//    variants.flatMap(_.definitions).toSeq :+ Expr.Let(enumName, None, enumFunc)
//  }
//
//
//  // Pattern
//
//  extension (self: PatternContext) {
//    private def visit: Pattern = self match {
//      case ctx: PatternLiteralContext => visitPatternLiteral(ctx)
//      case ctx: PatternVariableContext => visitPatternVariable(ctx)
//      case ctx: PatternVariantContext => visitPatternVariant(ctx)
//      case ctx: PatternRecordContext => visitPatternRecord(ctx)
//      case ctx: PatternTupleContext => visitPatternTuple(ctx)
//    }
//  }
//
//  override def visitPatternLiteral(ctx: PatternLiteralContext): Pattern = {
//    given ParserRuleContext = ctx
//    val valueString = ctx.literal.getText
//    val value = ctx.literal match {
//      case _: LiteralBoolContext => LiteralValue.BoolValue(valueString.toBoolean)
//      case _: LiteralCharContext => LiteralValue.CharValue(valueString.charAt(1))
//      case _: LiteralFloatContext => LiteralValue.FloatValue(valueString.toFloat)
//      case _: LiteralIntContext => LiteralValue.IntValue(valueString.toInt)
//      case _: LiteralRegularStringContext => LiteralValue.StringValue(valueString.substring(1, valueString.length - 1))
//      case _: LiteralRawStringContext => {
//        LiteralValue.StringValue(valueString.stripPrefix("#").stripPrefix("{").stripSuffix("}"))
//      }
//    }
//    Pattern.Literal(value)
//  }
//
//  override def visitPatternVariable(ctx: PatternVariableContext): Pattern = Pattern.Variable(ctx.ident.getText)(ctx)
//
//  override def visitPatternVariant(ctx: PatternVariantContext): Pattern = {
//    given ParserRuleContext = ctx
//    val variantIdent = ctx.ident.getText
//    val params = ctx.elements.asScala.map(_.visit)
//    Pattern.Variant(variantIdent, params.toList)
//  }
//
//  override def visitPatternRecord(ctx: PatternRecordContext): Pattern = {
//    given ParserRuleContext = ctx
//    val fields = ctx.fields.asScala.map { field =>
//      val name = field.ident.getText
//      val pattern = field.pattern.visit
//      (name, pattern)
//    }.toList
//    Pattern.Record(fields, Some(ctx.record.visit))
//  }
//
//  override def visitPatternTuple(ctx: PatternTupleContext): Pattern = {
//    given ParserRuleContext = ctx
//    val patterns = ctx.elements.asScala.zipWithIndex.map { (element, index) =>
//      val name = s"_${index + 1}"
//      (name, element.visit)
//    }.toList
//    Pattern.Record(patterns, None)
//  }
//
//  // Definition
//
//  override def visitDefinition(ctx: DefinitionContext): Expr = {
//    given ParserRuleContext = ctx
//    val name = ctx.ident.getText
//    val implicitParams = ctx.implicitParamList.getParams
//    val explicitParams = ctx.explicitParamList.getParams
//
//    if ctx.Identifier == null then { // this is an operator definition
//      val operator = getOperator(name)
//      operator match {
//        case Operator.Unary(_, _) => if explicitParams.length != 1 then {
//          throw CompileErrorException(ctx, "Unary operator must have exactly one explicit parameter")
//        }
//        case Operator.Binary(_, _, _, _, _) => if explicitParams.length != 2 then {
//          throw CompileErrorException(ctx, "Binary operator must have exactly two explicit parameters")
//        }
//      }
//    }
//
//    val returnType = ctx.returnType.visit
//    val body = ctx.body.visit
//    val params = implicitParams.map((_, ApplyMode.Implicit)) ++ explicitParams.map((_, ApplyMode.Explicit))
//    val func = function(params, body, Some(returnType))
//
//    Expr.Let(name, func.asInstanceOf[Expr.Function].getType, func)
//  }
//
//  override def visitBinaryOperator(ctx: BinaryOperatorContext): Unit = {
//    given ParserRuleContext = ctx
//    val associativity = ctx.associativity.getText match {
//      case "left-assoc" => Associativity.Left
//      case "right-assoc" => Associativity.Right
//    }
//    val precedenceDecl = ctx.operatorPrecedence.asScala
//    val tighterThan = precedenceDecl.flatMap(_.tighterThan.asScala).map(_.getText).toSet
//    val looserThan = precedenceDecl.flatMap(_.looserThan.asScala).map(_.getText).toSet
//    val sameAs = precedenceDecl.flatMap(_.sameAs.asScala).map(_.getText).toSet
//
//    // Register the operator
//    registerSymbol(ctx.symbol.getText, Operator.Binary(
//      ctx.symbol.getText,
//      associativity,
//      tighterThan.map(getBinaryOperator),
//      looserThan.map(getBinaryOperator),
//      sameAs.map(getBinaryOperator),
//    ))
//  }
//
//  override def visitUnaryOperator(ctx: UnaryOperatorContext): Unit = {
//    given ParserRuleContext = ctx
//    val kind = ctx.kind.getText match {
//      case "prefix" => UnaryType.Prefix
//      case "postfix" => UnaryType.Postfix
//    }
//    registerSymbol(ctx.symbol.getText, Operator.Unary(ctx.symbol.getText, kind))
//  }
//
//  private def function(
//    params: Seq[(BoundVariable, ApplyMode)], body: Expr,
//    returnType: Option[Expr] = None,
//  )(implicit ctx: ParserRuleContext): Expr = {
//    params.foldRight(body) {
//      case ((variable, applyMode), term) => term match {
//        case Expr.Function(param, mode, body, returnType) => {
//          val newReturnType: Option[Expr] = returnType.map(Expr.FunctionType(param.`type`, mode, _))
//          Expr.Function(variable, applyMode, body, newReturnType)
//        }
//        case _ => Expr.Function(variable, applyMode, term, returnType)
//      }
//    }
//  }
//
//  private def funcApplication(func: Expr, args: Seq[(Expr, ApplyMode)])(implicit ctx: ParserRuleContext): Expr = {
//    args.foldLeft(func) { case (acc, (arg, mode)) => Expr.Application(acc, mode, arg) }
//  }
//
//  extension (self: Expr.Function) {
//    // Get the type of the function, like String -> Int, not the return type
//    def getType: Option[Expr] = self.returnType match {
//      case None => None
//      case Some(returnType) => Some(Expr.FunctionType(self.param.`type`, self.applyMode, returnType)(self.span.context))
//    }
//  }
//
//  extension (self: ValueTypePairContext) {
//    private def toFields: Seq[(String, Expr)] = {
//      val `type` = self.`type`.visit
//      self.idents.asScala.map(_.getText).map((_, `type`)).toSeq
//    }
//  }
//
//  // Params
//
//  extension (self: ParamListContext) {
//    private def getParams: List[BoundVariable] = {
//      if (self == null) return Nil
//      self.params.asScala.flatMap { (paramsWithType: ValueTypePairContext) =>
//        val `type` = paramsWithType.`type`.visit
//        paramsWithType.idents.asScala.map { ident => BoundVariable(ident.getText, `type`) }
//      }.toList
//    }
//  }
//
//  // Atom
//
//  override def visitAtomIdentifier(ctx: AtomIdentifierContext): Expr = Expr.Variable(ctx.ident.getText)(ctx)
//
//  override def visitAtomOperator(ctx: AtomOperatorContext): Expr = Expr.Variable(ctx.op.getText)(ctx)
//
//  override def visitAtomLiteral(ctx: AtomLiteralContext): Expr = ctx.literal match {
//    case context: LiteralBoolContext => visitLiteralBool(context)
//    case context: LiteralCharContext => visitLiteralChar(context)
//    case context: LiteralFloatContext => visitLiteralFloat(context)
//    case context: LiteralIntContext => visitLiteralInt(context)
//    case context: LiteralRawStringContext => visitLiteralRawString(context)
//    case context: LiteralRegularStringContext => visitLiteralRegularString(context)
//  }
//
//  // Literal
//
//  override def visitLiteralInt(ctx: LiteralIntContext): Expr = Expr.PrimitiveValue(IntValue(ctx.value.getText.toInt))(ctx)
//
//  override def visitLiteralFloat(ctx: LiteralFloatContext): Expr = Expr.PrimitiveValue(FloatValue(ctx.value.getText.toFloat))(ctx)
//
//  override def visitLiteralBool(ctx: LiteralBoolContext): Expr = Expr.PrimitiveValue(BoolValue(ctx.value.getText.toBoolean))(ctx)
//
//  override def visitLiteralChar(ctx: LiteralCharContext): Expr = Expr.PrimitiveValue(CharValue(ctx.value.getText.charAt(1)))(ctx)
//
//  override def visitLiteralRawString(ctx: LiteralRawStringContext): Expr = {
//    given ParserRuleContext = ctx
//    val text = ctx.value.getText
//    Expr.PrimitiveValue(StringValue(text.stripPrefix("#").stripPrefix("{").stripSuffix("}")))
//  }
//
//  override def visitLiteralRegularString(ctx: LiteralRegularStringContext): Expr = {
//    given ParserRuleContext = ctx
//    val text = ctx.value.getText
//    Expr.PrimitiveValue(StringValue(text.substring(1, text.length - 1)))
//  }
//}
