package saki.core.elaborate

import saki.core.Param
import saki.core.context.{Environment, Typed}
import saki.core.domain.{Type, Value}
import saki.core.syntax.{*, given}
import saki.util.{unreachable, SourceSpan}
import saki.error.CoreErrorKind.*

import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

object Synthesis:

  def elaborate(expr: Expr, expectedType: Term)(
    implicit env: Environment.Typed[Value]
  ): Term = expr match {
    case Expr.Lambda(lambdaParam, body, returnType) => {
      // Check that expectedType is a Pi type
      expectedType.normalize match {
        case Term.Pi(piParam, codomain) => Term.Lambda(
          param = lambdaParam.map(_.elaborate(piParam.`type`)),
          body = env.withLocal(lambdaParam.ident, piParam.`type`.eval, piParam.`type`.infer) {
            // Check that the body type is the same as the codomain of the Pi type
            returnType.map(_.synth.term.eval unify codomain.eval) match {
              case Some(false) => TypeNotMatch.raise(returnType.get.span) {
                s"Expected type: $codomain, found: ${returnType.get.synth.term}"
              }
              case _ => body.elaborate(codomain)
            }
          }
        )
        case ty => TypeNotMatch.raise(expr.span) {
          s"Expected a function type or a Pi type, found: $ty"
        }
      }
    }
    case Expr.Hole(_) => UnexpectedValue.raise(expr.span) {
      "Holes are not allowed in this context"
    }
    case _ => {
      val synthResult = expr.synth
      if !(synthResult.`type` unify expectedType.eval) then TypeNotMatch.raise(expr.span) {
        s"Expected type: $expectedType, found: ${synthResult.`type`}"
      }
      synthResult.term
    }
  }

  case class Synth(term: Term, `type`: Type) {
    def unpack: (Term, Type) = (term, `type`)
    def normalize(implicit env: Environment.Typed[Value]): Synth = {
      Synth(term.normalize, `type`)
    }
  }

  def synth(expr: Expr)(implicit env: Environment.Typed[Value]): Synth = expr match {

    case Expr.Hole(_) => ??? // TODO: Implement hole synthesis

    case Expr.Unresolved(name) => {
      given SourceSpan = expr.span
      env.getDefinitionByName(name) match {
        case Some(definition) => Expr.Variable(definition.ident).synth(env)
        case None => env.getTyped(name) match {
          case Some(Typed(value, ty)) => Synth(value.readBack, ty)
          case None => UnresolvedReference.raise(expr.span) {
            s"Unresolved reference: $name"
          }
        }
      }
    }

    case Expr.Universe() => Synth(Term.Universe, Value.Universe)

    case Expr.Primitive(value) => Synth(Term.Primitive(value), Value.PrimitiveType(value.ty))

    case Expr.PrimitiveType(ty) => Synth(Term.PrimitiveType(ty), Value.Universe)

    case Expr.TypeOf(value) => value.synth(env).unpack match {
      case (_, ty: Value) => Synth(ty.readBack, Value.Universe)
    }

    case Expr.Variable(ref) => ref match {
      // Converting a definition reference to a lambda, enabling curry-style function application
      case definitionVar: Var.Defined[Term@unchecked, ?] => env.getDefinition(definitionVar) match {
        case Some(definition) => {
          // definition.ident.definition :=! definition
          synthDeclarationRef(definition)
        }
        case None => env.declarations.get(definitionVar) match {
          case Some(declaration) => synthDeclarationRef(declaration)
          case None => UnresolvedReference.raise(expr.span) {
            s"Unresolved reference: ${definitionVar.name}"
          }
        }
      }
      case variable: Var.Local => env.locals.get(variable) match {
        case Some(ty) => Synth(ty.value.readBack, ty.`type`)
        case None => UnboundVariable.raise(expr.span) {
          s"Unbound variable: ${variable.name}"
        }
      }
    }

    case Expr.Elimination(obj, member) => obj.synth(env).normalize.unpack match {
      // This is a project operation
      // `obj.field`
      case (term, recordType: Value.RecordType) => term match {
        case Term.Record(fields) => fields.get(member) match {
          case Some(value) => Synth(value, recordType.fields(member))
          case None => RecordMissingField.raise(expr.span) {
            s"Field not found: $member"
          }
        }
        case _ => Synth(Term.Projection(term, member), recordType.fields(member))
      }
      // This is a method call
      // `obj.method`
      case _ => {
        given SourceSpan = expr.span
        val method: (Function[Term] | Overloaded[Term]) = env.getDefinitionByName(member) match {
          case Some(definition: (Function[Term] | Overloaded[Term])) => definition
          case _ => MethodNotFound.raise(expr.span) {
            s"Method not found: $member"
          }
        }
        Expr.Apply(Expr.Variable(method.ident), Argument(obj)).synth(env)
      }
    }

    case Expr.Apply(fnExpr, argExpr) => {

      val (fn, fnType) = fnExpr.synth.unpack
      val paramIdent = env.uniqueVariable
      val param = Value.variable(paramIdent)

      fnType match {

        case Value.Pi(paramType, codomain) => {
          env.withLocal(paramIdent, param, paramType) { implicit env =>
            val (argTerm, argType) = argExpr.value.synth(env).unpack
            if !(paramType <:< argType) then TypeNotMatch.raise(argExpr.value.span) {
              s"Expected type: $paramType, found: $argType"
            }
            Synth(fn.apply(argTerm), codomain(argTerm.eval))
          }
        }

        case overloaded: Value.OverloadedPi => {

          val (argTerm, argType) = argExpr.value.synth(env).unpack
          val eigenState = overloaded.applyArgument(
            argTerm.eval, argType, Value.OverloadedPi.apply,
            unwrapStates = {
              case Value.OverloadedPi(states) => states
              case value => TypeNotMatch.raise {
                s"Expected an overloaded pi, but got: ${value.readBack}"
              }
            }
          )

          env.withLocal(paramIdent, param, argType) { implicit env =>
            Synth(fn.apply(argTerm), eigenState)
          }
        }

        case _ => TypeNotMatch.raise(fnExpr.span) {
          s"Expected a function type, found: $fnType"
        }
      }
    }

    case Expr.Constructor(inductiveExpr, consIdent) => {
      val inductiveType: Value.InductiveType = inductiveExpr.synth(env).term.eval match {
        case inductiveType: Value.InductiveType => inductiveType
        case _ => TypeNotMatch.raise(inductiveExpr.span) {
          s"Expected inductive type, found: ${inductiveExpr.synth.term}"
        }
      }
      val inductive: Var.Defined[Term, Inductive] = inductiveType.inductive
      val constructor: Constructor[Term] = inductive.definition.get.getConstructor(consIdent) match {
        case Some(definition) => definition
        case _ => ConstructorNotFound.raise(expr.span) {
          s"Constructor not found $consIdent in ${inductive.name}"
        }
      }
      if inductive != constructor.owner then TypeNotMatch.raise(expr.span) {
        s"Expected inductive type ${inductive.name}, found: ${constructor.owner.name}"
      }
      // Build lambda
      val variant = Term.InductiveVariant(inductiveType.readBack, constructor, constructor.paramToVars)
      env.withLocals(inductiveType.argsMap) { implicit env =>
        Synth(
          term = constructor.params.buildLambda(variant).normalize,
          `type` = constructor.params.buildPiType(inductiveType.readBack).eval,
        )
      }
    }

    case Expr.Match(scrutinees, clauses) => {
      val scrutineesSynth: Seq[Synth] = scrutinees.map(_.synth)
      val clausesSynth: Seq[(Clause[Term], Term)] = clauses.map { clause =>
        synthClause(clause, scrutineesSynth)
      }
      val clauseBodyTypes: Seq[Term] = clausesSynth.map(_._2)
      if !clauseBodyTypes.tail.forall(_.eval unify clauseBodyTypes.head.eval) then {
        TypeNotMatch.raise(expr.span) {
          s"Expected all clause bodies to have the same type, found: ${clauseBodyTypes.map(_.eval)}"
        }
      }
      Synth(
        term = Term.Match(scrutineesSynth.map(_.term), clausesSynth.map(_._1)),
        `type` = clauseBodyTypes.head.eval
      )
    }

    case Expr.Pi(param, result) => synthDependentType(param, result, Term.Pi.apply)

    case Expr.Sigma(param, result) => synthDependentType(param, result, Term.Sigma.apply)

    case Expr.Lambda(param, body, returnType) => {
      val paramIdent = param.ident
      val (paramType, _) = param.`type`.synth.unpack
      val paramTypeValue = paramType.eval
      val paramVariable = Value.variable(paramIdent)

      env.withLocal(paramIdent, paramVariable, paramTypeValue) { implicit env =>

        val (bodyTerm: Term, bodyType: Type) = body.synth(env).unpack

        val returnTypeValue: Value = returnType match {
          case Some(returnTypeExpr) => {
            val (returnType, _) = returnTypeExpr.synth.unpack
            val returnTypeValue = returnType.eval
            if !(returnTypeValue <:< bodyType) then {
              TypeNotMatch.raise(returnTypeExpr.span) {
                s"Expected type: $returnType, found: $bodyType"
              }
            }
            returnTypeValue
          }
          case None => bodyType
        }

        // Closure for the Pi type
        def piTypeClosure(arg: Value): Value = {
          val argVar: Typed[Value] = Typed[Value](arg, paramTypeValue)
          env.withLocal(param.ident, argVar) {
            implicit env => returnTypeValue.readBack(env).eval(env)
          }
        }

        Synth(
          term = Term.Lambda(Param(paramIdent, paramType), bodyTerm),
          `type` = Value.Pi(paramTypeValue, piTypeClosure),
        )

      }
    }

    case Expr.Record(fields, expectedType) => {
      val fieldSynths: Map[String, Synth] = fields.map((name, expr) => (name -> expr.synth))
      val recordType = Value.RecordType(fieldSynths.map((name, synth) => (name, synth.`type`)))
      val recordFields = fieldSynths.map((name, synth) => (name, synth.term))
      expectedType.foreach { expectedTypeExpr =>
        val (expectedType, _) = expectedTypeExpr.synth.unpack
        if !(recordType <:< expectedType.eval) then {
          TypeNotMatch.raise(expectedTypeExpr.span) {
            s"Expected type: $expectedType, found: $recordType"
          }
        }
      }
      Synth(Term.Record(recordFields), recordType)
    }

    case Expr.RecordType(fields) => {
      val fieldTypes: Map[String, Term] = fields.map((name, ty) => (name -> ty.synth.term))
      Synth(Term.RecordType(fieldTypes), Value.Universe)
    }
    
  }

  private def synthDependentType(param: Param[Expr], result: Expr, constructor: (Param[Term], Term) => Term)(
    implicit env: Environment.Typed[Value]
  ): Synth = {
    val (paramType, _) = param.`type`.synth.unpack
    val (codomain, _) = env.withLocal(param.ident, Value.variable(param.ident), paramType.eval) {
      result.synth(_).unpack
    }
    Synth(
      term = constructor(Param(param.ident, paramType), codomain),
      `type` = Value.Universe,
    )
  }

  def synthClause(clause: Clause[Expr], scrutinees: Seq[Synth])(
    implicit env: Environment.Typed[Value]
  ): (Clause[Term], Term) = {
    val patterns = clause.patterns.map(pattern => pattern.map(_.synth.term))
    val map = patterns.zip(scrutinees).foldLeft(Map.empty: Map[Var.Local, Term]) {
      case (subst, (pattern, param)) => {
        subst ++ pattern.buildMatchBindings(param.`type`).map((variable, ty) => variable -> ty.readBack)
      }
    }.map { case (k, v) => k -> Typed[Value](Value.variable(k), v.eval) }
    val (body, ty) = env.withLocals[Synth](map) { clause.body.synth }.unpack
    (Clause(patterns, body), ty.readBack)
  }

  extension (definition: Definition[Expr]) {
    def synth(implicit env: Environment.Typed[Value]): Definition[Term] = synthDefinition(definition)
  }
  
  def synthDefinition(definition: Definition[Expr])(
    implicit env: Environment.Typed[Value]
  ): Definition[Term] = definition match {
    case DefinedFunction(ident, paramExprs, resultTypeExpr, isRecursive, pristineBody) => {
      val (params, envParams) = synthParams(paramExprs)
      val (resultType, _) = resultTypeExpr.synth(envParams).unpack
      // Try to obtain the declaration of the function from the environment
      val defVar: Var.Defined[Term, Function] = env.declarations.get(Var.Defined(ident.name)) match {
        case Some(decl) => decl.ident.asInstanceOf[Var.Defined[Term, Function]]
        case _ => Var.Defined[Term, Function](ident.name)
      }
      val function = DefinedFunction[Term](defVar, params, resultType, isRecursive)
      function.body := envParams.withCurrentDefinition(function.ident) {
        implicit env => pristineBody.get.elaborate(resultType)(env)
      }
      function
    }

    case inductiveExpr: Inductive[Expr] => {
      val (params, envParams) = synthParams(inductiveExpr.params)(env)
      val constructors = ArrayBuffer.empty[Constructor[Term]]
      // Try to obtain the declaration of the inductive from the environment
      val defVar: Var.Defined[Term, Inductive] = env.declarations.get(Var.Defined(inductiveExpr.ident.name)) match {
        case Some(decl) => decl.ident.asInstanceOf[Var.Defined[Term, Inductive]]
        case _ => Var.Defined[Term, Inductive](inductiveExpr.ident.name)
      }
      val inductiveDefinition: Inductive[Term] = Inductive(defVar, params, constructors)
      // To support recursive inductive types, we need to add the inductive type to the context
      // before synthesizing the constructors
      envParams.withCurrentDefinition[Inductive[Term]](inductiveDefinition.ident) { implicit env =>
        constructors ++= inductiveExpr.constructors.map { constructor =>
          val constructorParams: ArrayBuffer[Param[Term]] = ArrayBuffer.empty
          val constructorDefinition: Constructor[Term] = {
            Constructor(constructor.ident, inductiveDefinition.ident, constructorParams)
          }
          constructorParams ++= synthParams(constructor.params)._1
          constructorDefinition
        }
        inductiveDefinition
      }
    }
    
    case Overloaded(ident, body) => {
      val overloads = body.map(_.synth.asInstanceOf[Function[Term]])
      Overloaded(ident.asInstanceOf[Var.Defined[Term, Overloaded]], overloads)
    }

    case _ => unreachable
  }

  /**
   * Synthesize the parameters of a function
   * @param paramExprs Sequence of Param[Expr]
   * @param env Environment
   * @return 1. Sequence of Param[Term]
   *         2. Updated environment
   */
  def synthParams(paramExprs: Seq[Param[Expr]])(
    implicit env: Environment.Typed[Value]
  ): (Seq[Param[Term]], Environment.Typed[Value]) = {
    paramExprs.foldLeft((Seq.empty[Param[Term]], env: Environment.Typed[Value])) {
      case ((params, env), paramExpr) => {
        val param = Param(paramExpr.ident, paramExpr.`type`.elaborate(Term.Universe)(env))
        (params :+ param, env.add(param.ident, Value.variable(param.ident), param.`type`.eval(env)))
      }
    }
  }

  def synthDeclarationRef(decl: Declaration[Term])(
    implicit env: Environment.Typed[Value]
  ): Synth = decl match {

    case definition: NaiveDefinition[Term] => Synth(
      term = definition.params.buildLambda(definition.buildInvoke(Term)).normalize,
      `type` = definition.params.buildPiType(definition.resultType).eval,
    )
    
    case declaration: PreDeclaration[Term, ?] => ???

    case overloaded: Overloaded[Term] => {
      val lambdaPaths = overloaded.overloads.map(overloaded => (overloaded.params, overloaded.buildInvoke(Term)))
      val piPaths = overloaded.overloads.map(overloaded => (overloaded.params, overloaded.resultType))
      val lambda = Term.overloaded(Term.OverloadedLambda.apply, lambdaPaths)
      val pi = Term.overloaded(Term.OverloadedPi.apply, piPaths)
      Synth(lambda, pi.eval)
    }

  }

end Synthesis
