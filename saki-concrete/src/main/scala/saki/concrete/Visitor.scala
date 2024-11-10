package saki.concrete

import org.antlr.v4.runtime.ParserRuleContext
import saki.concrete.SpineParser.{Associativity, Operator, Token, UnaryType}
import saki.concrete.syntax.{Definition, Evaluation, ExprTree, Spanned, Statement, SyntaxTree}
import saki.concrete.SyntaxError.*
import saki.core.Literal.*
import saki.core.{Pattern, Literal as LiteralValue}
import saki.core.syntax.{ApplyMode, Argument, Clause, Param, Var}
import saki.grammar.SakiBaseVisitor
import saki.grammar.SakiParser.*
import saki.util.*
import saki.error.CoreErrorKind.*

import scala.collection.Seq
import scala.jdk.CollectionConverters.*

class Visitor extends SakiBaseVisitor[SyntaxTree[?] | Seq[SyntaxTree[?]]] {

  private var symbols: Map[String, Operator | String] = {
    ScopedMap(Prelude.operators.map(op => op.symbol -> op).toMap[String, Operator | String])
  }

  private def binaryOperators: Set[Operator.Binary] = this.symbols.values.collect {
    case operator: Operator.Binary => operator
  }.toSet

  private def getBinaryOperator(symbol: String)(implicit ctx: ParserRuleContext): Operator.Binary = {
    this.symbols.get(symbol) match {
      case Some(operator: Operator.Binary) => operator
      case Some(_: Operator.Unary) => ctx.raiseError(InvalidOperator.apply) {
        s"Expected binary operator, found unary operator: ${symbol}"
      }
      case Some(symbol) => ctx.raiseError(InvalidOperator.apply) {
        s"Expected binary operator, found non-operator: ${symbol}"
      }
      case None => ctx.raiseError(InvalidOperator.apply) {
        s"Undeclared operator: ${symbol}"
      }
    }
  }

  private def getOperator(symbol: String)(implicit ctx: ParserRuleContext): Operator = {
    this.symbols.get(symbol) match {
      case Some(operator: Operator) => operator
      case Some(symbol) => ctx.raiseError(InvalidSymbol.apply) {
        s"Expected operator, found non-operator: ${symbol}"
      }
      case None => ctx.raiseError(InvalidOperator.apply) {
        s"Undeclared operator: ${symbol}"
      }
    }
  }

  private def registerSymbol(symbol: String, subject: Operator | Option[Nothing] = None)(
    implicit ctx: ParserRuleContext
  ): Unit = subject match {
    case operator: Operator => { // Operator can only be declared once
      if this.symbols.contains(symbol) then {
        ctx.raiseError(InvalidDeclaration.apply) {
          s"Operator redeclaration: ${symbol}"
        }
      }
      this.symbols += (symbol -> operator)
    }
    case _ => this.symbols += (symbol -> symbol)
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
    UnsupportedFeature.raise(ctx.span) { "Module entity implementation is not supported yet" }
  }

  override def visitModuleEntityOpDecl(ctx: ModuleEntityOpDeclContext): Seq[Definition] = {
    ctx.operatorDeclaration match {
      case ctx: UnaryOperatorContext => visitUnaryOperator(ctx)
      case ctx: BinaryOperatorContext => visitBinaryOperator(ctx)
    }
    Seq.empty
  }

  override def visitModuleEntityDef(ctx: ModuleEntityDefContext): Seq[Definition] = ctx.definition.visit

  override def visitModuleEntityEval(ctx: ModuleEntityEvalContext): Evaluation = Evaluation(ctx.blockExpr.visit)

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

    if ctx.returnType == null then {
      ctx.raiseError(SyntaxError.MissingReturnType.apply) {
        "Return type of a definition must be explicitly specified"
      }
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
        case _: InductiveConsTypeContext => UnsupportedFeature.raise(inductive.span) {
          "Inductive constructor with type is not supported yet"
        }
        case cons: InductiveConsTupleContext => {
          val params = cons.elements.asScala.zipWithIndex.map { (element, index) =>
            val ident = Option(element.ident).map(_.getText).getOrElse(s"_$index")
            val `type` = element.`type`.visit
            Param(Var.Local(ident), `type`)
          }
          // TODO: flat constructor: add a global definition with the constructor name
          Definition.Constructor(s"${cons.ident.getText}", params)(cons)
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
      case ctx: ExprUnionTypeContext => visitExprUnionType(ctx)
      case ctx: ExprIntersectionTypeContext => visitExprIntersectionType(ctx)
      case ctx: ExprTupleTypeContext => visitExprTupleType(ctx)
      case ctx: ExprTupleContext => visitExprTuple(ctx)
      case ctx: ExprConstructorContext => visitExprConstructor(ctx)
      case ctx: ExprLambdaContext => visitExprLambda(ctx)
      case ctx: ExprCallWithLambdaContext => visitExprCallWithLambda(ctx)
      case ctx: ExprEliminationContext => visitExprElimination(ctx)
      case ctx: ExprSpineContext => visitExprSpine(ctx)
      case ctx: ExprSpineInfixOpContext => visitExprSpineInfixOp(ctx)
      case ctx: ExprSpinePrefixOpContext => visitExprSpinePrefixOp(ctx)
      case ctx: ExprSpinePostfixOpContext => visitExprSpinePostfixOp(ctx)
      case ctx: ExprIfContext => visitExprIf(ctx)
      case ctx: ExprMatchContext => visitExprMatch(ctx)
      case ctx: ExprArrowTypeContext => visitExprArrowType(ctx)
      case ctx: ExprPiTypeContext => visitExprPiType(ctx)
      case ctx: ExprImplicitPiTypeContext => visitExprImplicitPiType(ctx)
      case ctx: ExprSigmaTypeContext => visitExprSigmaType(ctx)
      case ctx: ExprRecordTypeContext => visitExprRecordType(ctx)
      case ctx: ExprRecordContext => visitExprRecord(ctx)
      case _ => UnsupportedFeature.raise(self.span) { "Unsupported expression" }
    }
  }

  extension (self: AtomContext) {
    private def visit: ExprTree = self match {
      case ctx: AtomOperatorContext => visitAtomOperator(ctx)
      case ctx: AtomIdentifierContext => visitAtomIdentifier(ctx)
      case ctx: AtomLiteralContext => visitAtomLiteral(ctx)
      case ctx: AtomSelfContext => UnsupportedFeature.raise(ctx.span) {
        "`self` is not supported yet"
      }
    }
  }

  override def visitExprAtom(ctx: ExprAtomContext): ExprTree = ctx.atom.visit

  override def visitExprCall(ctx: ExprCallContext): ExprTree.Apply = {
    given ParserRuleContext = ctx
    val func = ctx.func.visit
    val args = ctx.argList.args.asScala.map { arg => Argument(arg.visit, ApplyMode.Explicit) }
    ExprTree.Apply(func, args)
  }

  override def visitExprImplicitCall(ctx: ExprImplicitCallContext): ExprTree.Apply = {
    given ParserRuleContext = ctx
    val func = ctx.func.visit
    val args = ctx.argList.args.asScala.map { arg => Argument(arg.visit, ApplyMode.Implicit) }
    ExprTree.Apply(func, args)
  }

  override def visitExprParen(ctx: ExprParenContext): ExprTree = ctx.value.visit

  override def visitExprUnionType(ctx: ExprUnionTypeContext): ExprTree = {
    ExprTree.Union(ctx.types.asScala.map(_.visit))(ctx)
  }
  
  override def visitExprIntersectionType(ctx: ExprIntersectionTypeContext): ExprTree = {
    ExprTree.Intersection(ctx.types.asScala.map(_.visit))(ctx)
  }

  override def visitExprTuple(ctx: ExprTupleContext): ExprTree = {
    UnsupportedFeature.raise(ctx.span) { "Tuple is not supported yet" }
  }

  override def visitExprTupleType(ctx: ExprTupleTypeContext): ExprTree = {
    UnsupportedFeature.raise(ctx.span) { "Tuple type is not supported yet" }
  }

  override def visitExprConstructor(ctx: ExprConstructorContext): ExprTree.Constructor = {
    given ParserRuleContext = ctx
    val inductive = ctx.inductive.visit
    val constructor = ctx.constructor.getText
    ExprTree.Constructor(inductive, constructor)
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
    val params = if ctx.lambdaParamList == null then Seq.empty else {
      ctx.lambdaParamList.params.asScala
    }
    val lambdaParams = params.map { param =>
      val ident = param.ident.getText
      val ty = Option(param.`type`).map(_.visit)
      Param(Var.Local(ident), ty)
    }
    val lambdaBody = ExprTree.CodeBlock(ctx.body.statements.asScala.map(_.visit))
    ExprTree.Apply(func, Seq(Argument(lambda(lambdaParams, lambdaBody, returnType), ApplyMode.Explicit)))
  }

  override def visitExprElimination(ctx: ExprEliminationContext): ExprTree = {
    given ParserRuleContext = ctx
    val subject = ctx.subject.visit
    val member = ctx.member.getText
    if ctx.implicitArgList == null then {
      ExprTree.Elimination(subject, member)
    } else {
      // Implicit arguments should be applied to member, returns an application
      val implicitArgs = ctx.implicitArgList.args.asScala.map { arg => Argument(arg.visit, ApplyMode.Implicit) }
      ExprTree.Apply(ExprTree.Apply(ExprTree.Variable(member), implicitArgs), Seq(Argument(subject)))
    }
  }

  override def visitExprSpine(ctx: ExprSpineContext): ExprTree = visitSpine(ctx)
  override def visitExprSpineInfixOp(ctx: ExprSpineInfixOpContext): ExprTree = visitSpine(ctx)
  override def visitExprSpinePrefixOp(ctx: ExprSpinePrefixOpContext): ExprTree = visitSpine(ctx)
  override def visitExprSpinePostfixOp(ctx: ExprSpinePostfixOpContext): ExprTree = visitSpine(ctx)

  private type SpineContext = ExprSpineContext
    | ExprSpineInfixOpContext
    | ExprSpinePrefixOpContext
    | ExprSpinePostfixOpContext

  private def visitSpine(ctx: SpineContext): ExprTree = {
    def operator(op: String)(implicit ctx: ExprContext): Operator = getOperator(op)(ctx)

    // In-order traversal of the expression spine
    def traversal(ctx: ExprContext): Seq[ExprContext | AtomContext | Operator] = {
      given ExprContext = ctx
      ctx match {
        case spine: ExprSpineContext => traversal(spine.lhs) ++ Seq(spine.rhs)
        case spine: ExprSpineInfixOpContext => traversal(spine.lhs) ++ Seq(operator(spine.op.getText)) ++ traversal(spine.rhs)
        case spine: ExprSpinePrefixOpContext => Seq(operator(spine.op.getText)) ++ traversal(spine.rhs)
        case spine: ExprSpinePostfixOpContext => traversal(spine.lhs) ++ Seq(operator(spine.op.getText))
        case expr => Seq(expr)
      }
    }

    // Construct the expression spine
    val tokens: Seq[Token] = traversal(ctx).map {
      case op: Operator => Token.Op(op)
      case expr: ExprContext => Token.Atom[ExprTree](expr.visit)
      case atom: AtomContext => Token.Atom[ExprTree](atom.visit)
    }

    val spineExprs = try {
      SpineParser.parseExpressions(tokens, this.binaryOperators)
    } catch {
      case err: Throwable => ctx.raiseError(SyntaxError.SpineParsingError.apply) {
        s"Failed to parse expression spine: ${err.getMessage}"
      }
    }

    given ParserRuleContext = ctx

    def visitSpineParserExpr(expr: SpineParser.Expr): ExprTree = expr match {
      case SpineParser.Expr.Atom(atom) => atom.asInstanceOf[ExprTree]
      case SpineParser.Expr.UnaryExpr(op, expr) => {
        ExprTree.Apply(ExprTree.Variable(op.symbol), Seq(Argument(visitSpineParserExpr(expr))))
      }
      case SpineParser.Expr.BinaryExpr(op, lhs, rhs) => {
        ExprTree.Apply(ExprTree.Variable(op.symbol), Seq(Argument(visitSpineParserExpr(lhs)), Argument(visitSpineParserExpr(rhs))))
      }
    }

    val spine = spineExprs.map(visitSpineParserExpr)
    ExprTree.Apply(spine.head, spine.tail.map(Argument(_, ApplyMode.Explicit)))
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
          val pattern: Pattern[ExprTree] = clause.pattern.visit.get
          val typedPattern = if clause.`type` == null then pattern else {
            Pattern.Typed(pattern, clause.`type`.visit)(ctx.span)
          }
          Clause(Seq(typedPattern), body)
        }
        case clause: MatchClauseTupleContext => {
          val patterns = clause.patternList.patterns.asScala.map(_.visit.get)
          if clause.`type` != null then UnsupportedFeature.raise(clause.span) {
            "Type annotation in match clause is not supported yet"
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
      case ctx: StatementBlockContext => visitStatementBlock(ctx)
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

  override def visitStatementBlock(ctx: StatementBlockContext): Statement = {
    given ParserRuleContext = ctx
    Statement.Expression(visitBlockExpr(ctx.blockExpr))
  }

  override def visitStatementInstance(ctx: StatementInstanceContext): Statement = {
    UnsupportedFeature.raise(ctx.span) { "Instance statement is not supported yet" }
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
      case _: LiteralCharContext => LiteralValue.RuneValue(valueString.charAt(1))
      case _: LiteralFloatContext => LiteralValue.FloatValue(valueString.toDouble)
      case _: LiteralIntContext => LiteralValue.IntValue(BigInt(valueString))
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
    val inductive = ctx.inductive.visit
    val patterns = Option(ctx.consPatternList).map(_.patterns.asScala.map(_.visit.get)).getOrElse(Seq.empty)
    Spanned(Pattern.Variant(inductive, ctx.constructor.getText, patterns.toSeq))
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
    UnsupportedFeature.raise(ctx.span) { "Tuple pattern is not supported yet" }
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
      sameAs.map(getBinaryOperator),
      looserThan.map(getBinaryOperator),
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

  override def visitLiteralChar(ctx: LiteralCharContext): ExprTree = ExprTree.PrimitiveValue(RuneValue(ctx.value.getText.charAt(1)))(ctx)

  override def visitLiteralRawString(ctx: LiteralRawStringContext): ExprTree = {
    given ParserRuleContext = ctx
    val text = ctx.value.getText
    ExprTree.PrimitiveValue(StringValue(text.stripPrefix("#").stripPrefix("{").stripSuffix("}")))
  }

  override def visitLiteralRegularString(ctx: LiteralRegularStringContext): ExprTree = {
    given ParserRuleContext = ctx
    val text = ctx.value.getText
    ExprTree.PrimitiveValue(StringValue(text.substring(1, text.length - 1).translateEscapes))
  }
}
