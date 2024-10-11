package saki.concrete

import org.antlr.v4.runtime.ParserRuleContext
import saki.concrete.SpineParser.{Associativity, Operator, Token, UnaryType}
import saki.concrete.syntax.{Definition, Evaluation, ExprTree, Spanned, Statement, SyntaxTree}
import saki.core.Literal.*
import saki.core.{Pattern, SourceSpan, UnsupportedError, Literal as LiteralValue}
import saki.core.syntax.{ApplyMode, Argument, Clause, Param, Var}
import saki.grammar.SakiBaseVisitor
import saki.grammar.SakiParser.*
import saki.util.*

import scala.collection.Seq
import scala.jdk.CollectionConverters.*

class Visitor extends SakiBaseVisitor[SyntaxTree[?] | Seq[SyntaxTree[?]]] {

  private var symbols: ScopedMap[String, Operator | String] = ScopedMap.empty

  private def binaryOperators: Set[Operator.Binary] = this.symbols.values.collect {
    case operator: Operator.Binary => operator
  }.toSet

  private def getBinaryOperator(symbol: String)(implicit ctx: ParserRuleContext): Operator.Binary = {
    this.symbols.get(symbol) match {
      case Some(operator: Operator.Binary) => operator
      case Some(_: Operator.Unary) => ctx.raiseError("Invalid operator", s"Expected binary operator, found unary operator: ${symbol}")
      case Some(symbol) => ctx.raiseError("Invalid operator", s"Expected binary operator, found non-operator: ${symbol}")
      case None => ctx.raiseError("Invalid operator", s"Undeclared operator: ${symbol}")
    }
  }

  private def getOperator(symbol: String)(implicit ctx: ParserRuleContext): Operator = {
    this.symbols.get(symbol) match {
      case Some(operator: Operator) => operator
      case Some(symbol) => ctx.raiseError("Invalid symbol", s"Expected operator, found non-operator: ${symbol}")
      case None => ctx.raiseError("Invalid operator", s"Undeclared operator: ${symbol}")
    }
  }

  private def registerSymbol(symbol: String, subject: Operator | Option[Nothing] = None)(
    implicit ctx: ParserRuleContext
  ): Unit = subject match {
    case operator: Operator => { // Operator can only be declared once
      if this.symbols.contains(symbol) then {
        ctx.raiseError("Invalid operator declaration", s"Operator redeclaration: ${symbol}")
      }
      this.symbols + (symbol -> operator)
    }
    case _ => this.symbols + (symbol -> symbol)
  }

  private def withInScope[T](block: => T): T = {
    this.symbols = this.symbols.enter
    val result = block
    this.symbols = this.symbols.exit
    result
  }

  override def visitProgram(ctx: ProgramContext): Seq[Definition | Evaluation] = {
    ctx.entities.asScala.flatMap(_.visit)
  }

  // Module Elements

  extension (self: ModuleEntityContext) {
    private def visit: Seq[Definition | Evaluation] = self match {
      case ctx: ModuleEntityImplContext => visitModuleEntityImpl(ctx)
      case ctx: ModuleEntityOpDeclContext => visitModuleEntityOpDecl(ctx)
      case ctx: ModuleEntityDefContext => visitModuleEntityDef(ctx)
      case ctx: ModuleEntityEvalContext => Seq(visitModuleEntityEval(ctx))
    }
  }

  override def visitModuleEntityImpl(ctx: ModuleEntityImplContext): Seq[Definition] = {
    UnsupportedError.unsupported("Module entity implementation is not supported yet", ctx.span)
  }

  override def visitModuleEntityOpDecl(ctx: ModuleEntityOpDeclContext): Seq[Definition] = {
    unimplemented("TODO") // TODO
    Seq.empty
  }

  override def visitModuleEntityDef(ctx: ModuleEntityDefContext): Seq[Definition] = ctx.definition.visit

  override def visitModuleEntityEval(ctx: ModuleEntityEvalContext): Evaluation = Evaluation(ctx.expr.visit)

  // Definition

  extension (self: DefinitionContext) {
    private def visit: Seq[Definition] = self match {
      case ctx: DefGeneralContext => visitDefGeneral(ctx)
      case ctx: DefTypeContext => visitDefType(ctx)
    }
  }

  override def visitDefType(ctx: DefTypeContext): Seq[Definition] = {
    given ParserRuleContext = ctx
    val implicitParams = ctx.implicitParamList.toImplicitParams
    val explicitParams = ctx.explicitParamList.toExplicitParams
    val ident = ctx.ident.getText
    visitDefinitionBody(ident, implicitParams, explicitParams, ExprTree.Universe(), ctx.body)
  }

  override def visitDefGeneral(ctx: DefGeneralContext): Seq[Definition] = {

    val implicitParams = ctx.implicitParamList.toImplicitParams
    val explicitParams = ctx.explicitParamList.toExplicitParams

    val ident = ctx.ident match {
      case ident: DefIdentOperatorContext => ident.operator.getText
      case ident: DefIdentStringContext => ident.string.getText
    }

    val returnType = ctx.returnType.visit

    visitDefinitionBody(ident, implicitParams, explicitParams, returnType, ctx.body)
  }

  // Definition Body

  def visitDefinitionBody(
    ident: String,
    implicitParams: Seq[Param[ExprTree]],
    explicitParams: Seq[Param[ExprTree]],
    returnType: ExprTree,
    body: DefinitionBodyContext,
  ): Seq[Definition] = body match {
    case expr: DefBodyExprContext => {
      val params: Seq[Param[ExprTree]] = implicitParams ++ explicitParams
      Seq(Definition.Function(ident, params, returnType, expr.blockExpr.visit)(body))
    }
    case inductive: DefBodyInductiveContext => {
      val params: Seq[Param[ExprTree]] = implicitParams ++ explicitParams
      val constructors = inductive.constructors.asScala.map {
        case _: InductiveConsTypeContext => {
          UnsupportedError.unsupported("Inductive constructor with type is not supported yet", inductive.span)
        }
        case cons: InductiveConsTupleContext => {
          val params = cons.elements.asScala.zipWithIndex.map { (element, index) =>
            val ident = Option(element.ident).map(_.getText).getOrElse(s"_$index")
            val `type` = element.`type`.visit
            Param(Var.Local(ident), `type`)
          }
          // TODO: flat
          Definition.Constructor(s"$ident::${cons.ident.getText}", params)(cons)
        }
      }
      Seq(Definition.Inductive(ident, params, constructors)(body))
    }
  }


  // block

  override def visitBlock(ctx: BlockContext): Seq[Statement] = ctx.statements.asScala.map(_.visit)

  def visitBlockExpr(ctx: BlockExprContext): ExprTree = ctx.visit

  extension (self: BlockExprContext) {
    private def visit: ExprTree = self match {
      case ctx: BlockExprExprContext => ctx.expr.visit
      case ctx: BlockExprBlockContext => {
        ExprTree.CodeBlock(ctx.block.statements.asScala.map(_.visit))(self)
      }
    }
  }

  // Expr

  def visitExpr(ctx: ExprContext): ExprTree = ctx.visit

  extension (self: ExprContext) {
    private def visit: ExprTree = self match {
      case ctx: ExprAtomContext => visitExprAtom(ctx)
      case ctx: ExprCallContext => visitExprCall(ctx)
      case ctx: ExprImplicitCallContext => visitExprImplicitCall(ctx)
      case ctx: ExprParenContext => visitExprParen(ctx)
      case ctx: ExprTupleTypeContext => visitExprTupleType(ctx)
      case ctx: ExprTupleContext => visitExprTuple(ctx)
      case ctx: ExprConstructorContext => visitExprConstructor(ctx)
      case ctx: ExprLambdaContext => visitExprLambda(ctx)
      case ctx: ExprCallWithLambdaContext => visitExprCallWithLambda(ctx)
      case ctx: ExprEliminationContext => visitExprElimination(ctx)
      case ctx: ExprSpineContext => visitExprSpine(ctx)
      case ctx: ExprIfContext => visitExprIf(ctx)
      case ctx: ExprMatchContext => visitExprMatch(ctx)
      case ctx: ExprArrowTypeContext => visitExprArrowType(ctx)
      case ctx: ExprPiTypeContext => visitExprPiType(ctx)
      case ctx: ExprImplicitPiTypeContext => visitExprImplicitPiType(ctx)
      case ctx: ExprSigmaTypeContext => visitExprSigmaType(ctx)
      case ctx: ExprRecordTypeContext => visitExprRecordType(ctx)
      case ctx: ExprRecordContext => visitExprRecord(ctx)
      case _ => UnsupportedError.unsupported("Unsupported expression", self.span)
    }
  }

  override def visitExprAtom(ctx: ExprAtomContext): ExprTree = ctx.atom match {
    case context: AtomOperatorContext => visitAtomOperator(context)
    case context: AtomIdentifierContext => visitAtomIdentifier(context)
    case context: AtomLiteralContext => visitAtomLiteral(context)
    case context: AtomSelfContext => UnsupportedError.unsupported("`self` is not supported yet", context.span)
  }

  override def visitExprCall(ctx: ExprCallContext): ExprTree.FunctionCall = {
    given ParserRuleContext = ctx
    val func = ctx.func.visit
    val args = ctx.argList.args.asScala.map { arg => Argument(arg.visit, ApplyMode.Explicit) }
    ExprTree.FunctionCall(func, args)
  }

  override def visitExprImplicitCall(ctx: ExprImplicitCallContext): ExprTree.FunctionCall = {
    given ParserRuleContext = ctx
    val func = ctx.func.visit
    val args = ctx.argList.args.asScala.map { arg => Argument(arg.visit, ApplyMode.Implicit) }
    ExprTree.FunctionCall(func, args)
  }

  override def visitExprParen(ctx: ExprParenContext): ExprTree = ctx.value.visit

  override def visitExprTuple(ctx: ExprTupleContext): ExprTree = {
    UnsupportedError.unsupported("Tuple is not supported yet", ctx.span)
  }

  override def visitExprTupleType(ctx: ExprTupleTypeContext): ExprTree = {
    UnsupportedError.unsupported("Tuple type is not supported yet", ctx.span)
  }

  override def visitExprConstructor(ctx: ExprConstructorContext): ExprTree.Constructor = {
    given ParserRuleContext = ctx
    val inductive = ctx.inductive.getText
    val explicitArgs = Option(ctx.explicitArgList).map(_.args.asScala.map(_.visit)).getOrElse(Seq.empty)
    val implicitArgs = Option(ctx.implicitArgList).map(_.args.asScala.map(_.visit)).getOrElse(Seq.empty)
    val constructor = ctx.constructor.getText
    val args = explicitArgs.map(Argument(_)) ++ implicitArgs.map(Argument(_, ApplyMode.Implicit))
    ExprTree.Constructor(inductive, args, constructor)
  }

  override def visitExprLambda(ctx: ExprLambdaContext): ExprTree = {
    given ParserRuleContext = ctx
    val params = ctx.lambdaParamList.params.asScala.flatMap { param =>
      val `type` = param.`type`.visit
      param.idents.asScala.map { ident =>
        val identStr = ident.getText
        Param(Var.Local(identStr), Option(`type`))
      }
    }
    val body = ctx.body.visit
    val returnType = Option(ctx.returnType).map(_.visit)
    lambda(params, body, returnType)
  }

  override def visitExprCallWithLambda(ctx: ExprCallWithLambdaContext): ExprTree = {
    given ParserRuleContext = ctx
    val func = ctx.func.visit
    val returnType = Option(ctx.returnType).map(_.visit)
    val lambdaParams = ctx.lambdaParamList.params.asScala.map { param =>
      val ident = param.ident.getText
      val ty = Option(param.`type`).map(_.visit)
      Param(Var.Local(ident), ty)
    }
    val lambdaBody = ExprTree.CodeBlock(ctx.body.statements.asScala.map(_.visit))
    ExprTree.FunctionCall(func, Seq(Argument(lambda(lambdaParams, lambdaBody, returnType), ApplyMode.Explicit)))
  }

  override def visitExprElimination(ctx: ExprEliminationContext): ExprTree.Elimination = {
    given ParserRuleContext = ctx
    val subject = ctx.subject.visit
    val member = ctx.member.getText
    if ctx.implicitArgList != null then {
      UnsupportedError.unsupported("Implicit arguments in elimination is not supported yet", ctx.span)
    }
    ExprTree.Elimination(subject, member)
  }

  override def visitExprSpine(ctx: ExprSpineContext): ExprTree = {
    // In-order traversal of the expression spine
    def traversal(ctx: ExprContext): Seq[ExprContext] = ctx match {
      case spine: ExprSpineContext => traversal(spine.lhs) ++ traversal(spine.rhs)
      case expr => Seq(expr)
    }

    // Construct the expression spine
    val tokens: Seq[Token] = traversal(ctx).map {
      case atom: ExprAtomContext => atom.value match {
        case operator: AtomOperatorContext => Token.Op(this.getOperator(operator.op.getText)(operator))
        case other => Token.Atom[ExprTree](visitExprAtom(atom))
      }
      case other => Token.Atom[ExprTree](other.visit)
    }

    val spineExprs = SpineParser.parseExpressions(tokens, this.binaryOperators)

    given ParserRuleContext = ctx

    def visitSpineParserExpr(expr: SpineParser.Expr): ExprTree = expr match {
      case SpineParser.Expr.Atom(atom) => atom.asInstanceOf[ExprTree]
      case SpineParser.Expr.UnaryExpr(op, expr) => {
        ExprTree.FunctionCall(ExprTree.Variable(op.symbol), Seq(Argument(visitSpineParserExpr(expr))))
      }
      case SpineParser.Expr.BinaryExpr(op, lhs, rhs) => {
        ExprTree.FunctionCall(ExprTree.Variable(op.symbol), Seq(Argument(visitSpineParserExpr(lhs)), Argument(visitSpineParserExpr(rhs))))
      }
    }

    val spine = spineExprs.map(visitSpineParserExpr)
    ExprTree.FunctionCall(spine.head, spine.tail.map(Argument(_, ApplyMode.Explicit)))
  }

  override def visitExprIf(ctx: ExprIfContext): ExprTree = {
    given ParserRuleContext = ctx
    val condition = ctx.cond.visit
    val thenBranch = ctx.`then`.visit
    val elseBranch = Option(ctx.else_).map(_.visit)
    ExprTree.If(condition, thenBranch, elseBranch)
  }

  override def visitExprMatch(ctx: ExprMatchContext): ExprTree = {
    given ParserRuleContext = ctx
    val scrutinee = ctx.value.visit
    val cases = ctx.cases.asScala.flatMap { caseCtx =>
      val body = caseCtx.body.visit
      caseCtx.clauses.asScala.map {
        case clause: MatchClauseSingleContext => {
          val pattern = clause.pattern.visit.get
          if clause.`type` != null then {
            UnsupportedError.unsupported("Type annotation in match clause is not supported yet", clause.span)
          }
          Clause(Seq(pattern), body)
        }
        case clause: MatchClauseTupleContext => {
          val patterns = clause.patternList.patterns.asScala.map(_.visit.get)
          if clause.`type` != null then {
            UnsupportedError.unsupported("Type annotation in match clause is not supported yet", clause.span)
          }
          Clause(patterns, body)
        }
      }
    }
    ExprTree.Match(Seq(scrutinee), cases.toList)
  }

  override def visitExprArrowType(ctx: ExprArrowTypeContext): ExprTree.Pi = {
    given ParserRuleContext = ctx
    val domain = ctx.domain.visit
    val codomain = ctx.codomain.visit
    ExprTree.Pi(Param(Var.Local("_"), domain), codomain)
  }

  override def visitExprImplicitArrowType(ctx: ExprImplicitArrowTypeContext): ExprTree.Pi = {
    given ParserRuleContext = ctx
    val domain = ctx.domain.visit
    val codomain = ctx.codomain.visit
    ExprTree.Pi(Param(Var.Local("_"), domain, ApplyMode.Implicit), codomain)
  }

  override def visitExprPiType(ctx: ExprPiTypeContext): ExprTree.Pi = {
    given ParserRuleContext = ctx
    val param = ctx.param.getText
    val domain = ctx.domain.visit
    val codomain = ctx.codomain.visit
    ExprTree.Pi(Param(Var.Local(param), domain), codomain)
  }

  override def visitExprImplicitPiType(ctx: ExprImplicitPiTypeContext): ExprTree.Pi = {
    given ParserRuleContext = ctx
    val param = ctx.param.getText
    val domain = ctx.domain.visit
    val codomain = ctx.codomain.visit
    ExprTree.Pi(Param(Var.Local(param), domain, ApplyMode.Implicit), codomain)
  }

  override def visitExprSigmaType(ctx: ExprSigmaTypeContext): ExprTree.Sigma = {
    given ParserRuleContext = ctx
    val param = ctx.param.getText
    val domain = ctx.domain.visit
    val codomain = ctx.codomain.visit
    ExprTree.Sigma(Param(Var.Local(param), domain), codomain)
  }

  override def visitExprRecordType(ctx: ExprRecordTypeContext): ExprTree.RecordType = {
    given ParserRuleContext = ctx
    val fields = ctx.fields.asScala.flatMap { field =>
      val ty = field.`type`.visit
      field.idents.asScala.map { ident => (ident.getText, ty) }
    }
    ExprTree.RecordType(fields.toSeq)
  }

  override def visitExprRecord(ctx: ExprRecordContext): ExprTree.RecordValue = {
    given ParserRuleContext = ctx
    val recordType = ctx.recordType.visit
    val fields = ctx.fields.asScala.map { field =>
      val name = field.ident.getText
      val value = field.value.visit
      (name, value)
    }
    ExprTree.RecordValue(fields.toSeq, recordType)
  }

  // Statement

  def visitStatement(ctx: StatementContext): Statement = ctx.visit

  extension (self: StatementContext) {
    private def visit: Statement = self match {
      case ctx: StatementExprContext => visitStatementExpr(ctx)
      case ctx: StatementLetContext => visitStatementLet(ctx)
      case ctx: StatementInstanceContext => visitStatementInstance(ctx)
    }
  }

  override def visitStatementExpr(ctx: StatementExprContext): Statement = {
    given ParserRuleContext = ctx
    Statement.Expression(ctx.expr.visit)
  }

  override def visitStatementLet(ctx: StatementLetContext): Statement = {
    given ParserRuleContext = ctx
    val ty = if ctx.`type` == null then None else Some(ctx.`type`.visit)
    Statement.Let(ctx.name.getText, ty, ctx.value.visit)
  }

  override def visitStatementInstance(ctx: StatementInstanceContext): Statement = {
    UnsupportedError.unsupported("Instance statement is not supported yet", ctx.span)
  }

  // Pattern

  extension (self: PatternContext) {
    private def visit: Spanned[Pattern[ExprTree]] = self match {
      case ctx: PatternLiteralContext => visitPatternLiteral(ctx)
      case ctx: PatternVariableContext => visitPatternVariable(ctx)
      case ctx: PatternConstructorContext => visitPatternConstructor(ctx)
      case ctx: PatternRecordContext => visitPatternRecord(ctx)
      case ctx: PatternTupleContext => visitPatternTuple(ctx)
    }
  }

  override def visitPatternLiteral(ctx: PatternLiteralContext): Spanned[Pattern[ExprTree]] = {
    given ParserRuleContext = ctx
    given SourceSpan = ctx.span
    val valueString = ctx.literal.getText
    val value = ctx.literal match {
      case _: LiteralBoolContext => LiteralValue.BoolValue(valueString.toBoolean)
      case _: LiteralCharContext => LiteralValue.CharValue(valueString.charAt(1))
      case _: LiteralFloatContext => LiteralValue.FloatValue(valueString.toFloat)
      case _: LiteralIntContext => LiteralValue.IntValue(valueString.toInt)
      case _: LiteralRegularStringContext => LiteralValue.StringValue(valueString.substring(1, valueString.length - 1))
      case _: LiteralRawStringContext => {
        LiteralValue.StringValue(valueString.stripPrefix("#").stripPrefix("{").stripSuffix("}"))
      }
    }
    Spanned(Pattern.Primitive(value))
  }

  override def visitPatternVariable(ctx: PatternVariableContext): Spanned[Pattern[ExprTree]] = {
    given ParserRuleContext = ctx
    Spanned(Pattern.Bind[ExprTree](Var.Local(ctx.ident.getText))(ctx.span))
  }

  override def visitPatternConstructor(ctx: PatternConstructorContext): Spanned[Pattern[ExprTree]] = {
    given ParserRuleContext = ctx
    given SourceSpan = ctx.span
    val inductiveIdent = ctx.inductive.getText
    val constructorIdent = s"${inductiveIdent}::${ctx.constructor.getText}"
    if ctx.indExplicitArgList != null || ctx.indImplicitArgList != null then {
      // TODO: support constructor with arguments
      UnsupportedError.unsupported("Pattern constructor with arguments", ctx.span)
    }
    val patterns = Option(ctx.consPatternList).map(_.patterns.asScala.map(_.visit.get)).getOrElse(Seq.empty)
    Spanned(Pattern.Cons(Var.Defined(constructorIdent), patterns.toSeq))
  }

  override def visitPatternRecord(ctx: PatternRecordContext): Spanned[Pattern[ExprTree]] = {
    given ParserRuleContext = ctx
    given SourceSpan = ctx.span
    val fields = ctx.fields.asScala.map { field =>
      val name = field.ident.getText
      val pattern = field.pattern.visit.get
      (name, pattern)
    }
    Spanned(Pattern.Record(fields.toSeq))
  }

  override def visitPatternTuple(ctx: PatternTupleContext): Spanned[Pattern[ExprTree]] = {
    UnsupportedError.unsupported("Tuple is not supported yet", ctx.span)
  }

  // Operator

  override def visitBinaryOperator(ctx: BinaryOperatorContext): Seq[Definition] = {
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
    registerSymbol(ctx.symbol.getText, Operator.Binary(
      ctx.symbol.getText,
      associativity,
      tighterThan.map(getBinaryOperator),
      looserThan.map(getBinaryOperator),
      sameAs.map(getBinaryOperator),
    ))

    Seq.empty
  }

  override def visitUnaryOperator(ctx: UnaryOperatorContext): Seq[Definition] = {
    given ParserRuleContext = ctx
    val kind = ctx.kind.getText match {
      case "prefix" => UnaryType.Prefix
      case "postfix" => UnaryType.Postfix
    }
    registerSymbol(ctx.symbol.getText, Operator.Unary(ctx.symbol.getText, kind))
    Seq.empty
  }

  private def lambda(
    params: Seq[Param[Option[ExprTree]]], body: ExprTree,
    returnType: Option[ExprTree] = None,
  )(implicit ctx: ParserRuleContext): ExprTree = {
    given ParserRuleContext = ctx
    // Fold over parameters to construct both the Lambda expression and the Pi-type return type
    params.foldRight((body, returnType)) { case (param, (accBody, returnType)) =>
      // Handling optional types by short-circuiting if any component is None
      // Ensures that the Pi type is constructed only when both the parameter type
      // and the codomain (return type) are well-defined.
      val piReturnType = for {
        paramType <- param.`type`  // Ensure the parameter type is defined
        codomain <- returnType  // Ensure the accumulated return type is defined
      } yield ExprTree.Pi(Param(param.ident, paramType), codomain)
      // Construct the Lambda expression with the updated body and return type (which could be None)
      val lambdaExpr = ExprTree.Lambda(param, accBody, returnType)
      // Accumulate the updated Lambda and Pi-type return type
      (lambdaExpr, piReturnType)
    }._1  // Return only the Lambda expression, ignoring the accumulated return type
  }

  // Params

  extension (self: ParamListContext) {

    private def toParams(applyMode: ApplyMode): Seq[Param[ExprTree]] = {
      if (self == null) return Seq.empty
      self.params.asScala.flatMap { param =>
        param.idents.asScala.map(_.getText).map { ident =>
          val `type` = param.`type`.visit
          Param(Var.Local(ident), `type`, applyMode)
        }
      }
    }

    private def toExplicitParams: Seq[Param[ExprTree]] = toParams(ApplyMode.Explicit)

    private def toImplicitParams: Seq[Param[ExprTree]] = toParams(ApplyMode.Implicit)
  }

  // Atom

  override def visitAtomIdentifier(ctx: AtomIdentifierContext): ExprTree = ExprTree.Variable(ctx.ident.getText)(ctx)

  override def visitAtomOperator(ctx: AtomOperatorContext): ExprTree = ExprTree.Variable(ctx.op.getText)(ctx)

  override def visitAtomLiteral(ctx: AtomLiteralContext): ExprTree = ctx.literal match {
    case context: LiteralBoolContext => visitLiteralBool(context)
    case context: LiteralCharContext => visitLiteralChar(context)
    case context: LiteralFloatContext => visitLiteralFloat(context)
    case context: LiteralIntContext => visitLiteralInt(context)
    case context: LiteralRawStringContext => visitLiteralRawString(context)
    case context: LiteralRegularStringContext => visitLiteralRegularString(context)
  }

  // Literal

  override def visitLiteralInt(ctx: LiteralIntContext): ExprTree = ExprTree.PrimitiveValue(IntValue(ctx.value.getText.toInt))(ctx)

  override def visitLiteralFloat(ctx: LiteralFloatContext): ExprTree = ExprTree.PrimitiveValue(FloatValue(ctx.value.getText.toFloat))(ctx)

  override def visitLiteralBool(ctx: LiteralBoolContext): ExprTree = ExprTree.PrimitiveValue(BoolValue(ctx.value.getText.toBoolean))(ctx)

  override def visitLiteralChar(ctx: LiteralCharContext): ExprTree = ExprTree.PrimitiveValue(CharValue(ctx.value.getText.charAt(1)))(ctx)

  override def visitLiteralRawString(ctx: LiteralRawStringContext): ExprTree = {
    given ParserRuleContext = ctx
    val text = ctx.value.getText
    ExprTree.PrimitiveValue(StringValue(text.stripPrefix("#").stripPrefix("{").stripSuffix("}")))
  }

  override def visitLiteralRegularString(ctx: LiteralRegularStringContext): ExprTree = {
    given ParserRuleContext = ctx
    val text = ctx.value.getText
    ExprTree.PrimitiveValue(StringValue(text.substring(1, text.length - 1)))
  }
}
