package saki.core.elaborate

import saki.core.Param
import saki.core.context.{Environment, Typed}
import saki.core.domain.{NeutralValue, Type, Value}
import saki.core.syntax.{*, given}
import saki.util.{unreachable, SourceSpan}
import saki.error.CoreErrorKind.*

import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

object Synthesis:

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
      case definitionVar: Var.Defined[Term@unchecked, ?] => env.getSymbol(definitionVar) match {
        case Some(symbol) => synthSymbol(symbol)
        case None => UnresolvedReference.raise(expr.span) {
          s"Unresolved reference: ${definitionVar.name}"
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
      case (term, ty) => {
        given SourceSpan = expr.span
        val method: Symbol[Term] = env.getSymbolByName(member) match {
          case Some(definition: Symbol[Term]) => definition
          case _ => MethodNotFound.raise(expr.span) {
            s"Method not found $member for value $term of type $ty"
          }
        }
        Expr.Apply(Expr.Variable(method.ident), Argument(obj)).synth(env)
      }
    }

    case Expr.Apply(fnExpr, argExpr) => {

      val (fn, fnType) = fnExpr.synth.unpack
      val paramIdent = env.uniqueVariable("#")

      fnType match {

        case Value.Pi(paramType, codomain) => {
          val param = Value.variable(paramIdent, paramType)
          env.withLocal(paramIdent, param, paramType) { implicit env =>
            val (argTerm, argType) = argExpr.value.synth(env).unpack
            if !(paramType <:< argType) then TypeNotMatch.raise(argExpr.value.span) {
              s"Expected argument type: ${paramType.readBack}, found argument $argExpr with type ${argType.readBack}"
            }
            Synth(Term.Apply(fn, argTerm), codomain.invokeWithEnv(argTerm.eval))
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

          val param = Value.variable(paramIdent, argType)
          env.withLocal(paramIdent, param, argType) { implicit env =>
            Synth(Term.Apply(fn, argTerm), eigenState)
          }
        }

        case Value.Universe => fn.normalize match {
          case Term.Pi(piParam, codomain) => {
            val (argTerm, argType) = argExpr.value.synth(env).normalize.unpack
            if !(piParam.`type`.eval <:< argType) then TypeNotMatch.raise(argExpr.value.span) {
              s"Expected type: ${piParam.`type`}, found: $argType"
            }
            val codomainValue = env.withLocal(piParam.ident, argTerm.eval, argType) {
              implicit env => codomain.eval(env)
            }
            Synth(codomainValue.readBack, Value.Universe)
          }
          case _ => TypeNotMatch.raise(fnExpr.span) {
            s"Expected a function, found: $fn"
          }
        }

        case fn => TypeNotMatch.raise(fnExpr.span) {
          s"Expected a function, found: $fn"
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
      // **Principle of Type Uniformity**
      //
      //  In a pattern-matching expression, we have a scrutinee (i.e., the value being matched)
      //  and multiple clauses (patterns and corresponding expressions). Each clause represents
      //  a potential match, and the principle of type uniformity requires that:
      //
      //    1. All clauses must yield the same type, regardless of which pattern successfully matches.
      //
      //    2. The resulting type of the entire match expression must be well-formed and identical
      //       for each possible path through the pattern.
      //
      // e.g. The type of following match expression should be `P(n)`, but not `P(Nat::Zero) | P(Nat::Succ(n'))`
      // ```
      //  match n {
      //    case Nat::Zero => ? : P(Nat::Zero)
      //    case Nat::Succ(n') => ? : P(Nat::Succ(n'))
      //  }
      // ```
      val scrutineesSynth: Seq[Synth] = scrutinees.map(_.synth)
      val clausesSynth: Seq[ClauseSynth] = clauses.map { clause =>
        synthClause(clause, scrutineesSynth)
      }
      val clauseBodyTypeTerms: Seq[Term] = clausesSynth.map(_.substitutedType)
      if clauseBodyTypeTerms.isEmpty then {
        SizeNotMatch.raise(expr.span) { "Expected at least one clause" }
      }
      val clauseBodyTypes: Seq[Value] = clauseBodyTypeTerms.map(_.eval)
      val leastUpperBoundType: Type = clauseBodyTypes.reduce((a, b) => a <:> b)
      Synth(
        term = Term.Match(scrutineesSynth.map(_.term), clausesSynth.map(_._1)),
        `type` = leastUpperBoundType
      )
    }

    case Expr.Pi(param, result) => synthDependentType(param, result, Term.Pi.apply)

    case Expr.Sigma(param, result) => synthDependentType(param, result, Term.Sigma.apply)

    case Expr.Lambda(param, body, returnType) => {
      val paramIdent = param.ident
      val (paramType, _) = param.`type`.synth.unpack
      val paramTypeValue = paramType.eval
      val paramVariable = Value.variable(paramIdent, paramTypeValue)

      env.withLocal(paramIdent, paramVariable, paramTypeValue) { implicit env =>

        val (bodyTerm: Term, bodyType: Type) = body.synth(env).unpack

        val returnTypeValue: Value = returnType match {
          case Some(returnTypeExpr) => {
            val (returnType, _) = returnTypeExpr.synth.unpack
            val returnTypeValue = returnType.eval
            if !(returnTypeValue <:< bodyType) then {
              TypeNotMatch.raise(returnTypeExpr.span) {
                s"Expected type: ${returnType.eval.readBack}, found: $bodyType"
              }
            }
            returnTypeValue
          }
          case None => bodyType
        }

        Synth(
          term = Term.Lambda(Param(paramIdent, paramType), bodyTerm),
          `type` = Value.Pi(
            paramTypeValue,
            ParameterizedClosure(Param(paramIdent, paramTypeValue), env) {
              implicit env => returnTypeValue.readBack.eval
            }
          )
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
    val (paramTypeTerm, _) = param.`type`.synth.unpack
    val paramType = paramTypeTerm.eval
    val (codomain, _) = env.withLocal(param.ident, Value.variable(param.ident, paramType), paramType) {
      result.synth(_).unpack
    }
    Synth(
      term = constructor(Param(param.ident, paramTypeTerm), codomain),
      `type` = Value.Universe,
    )
  }

  case class ClauseSynth(clause: Clause[Term], substitutedType: Term, normalizedType: Type) {
    def unpack: (Clause[Term], Term, Type) = (clause, substitutedType, normalizedType)
    def unapply: (Clause[Term], Term, Type) = unpack
  }

  def synthClause(clause: Clause[Expr], scrutinees: Seq[Synth])(
    implicit env: Environment.Typed[Value]
  ): ClauseSynth = {
    val patterns = clause.patterns.map(pattern => pattern.map(_.synth.term))
    // TODO: Check whether bindings of all patterns are consistent
    //  e.g. This is not allowed:
    //  match x {
    //    case Foo::Bar(a) | Foo::Baz(b) => foo(a, b)
    //  }
    val patternsBinding: Seq[Map[Var.Local, Typed[Value]]] = patterns.zip(scrutinees).map {
      (pattern, scrutinee) => pattern.buildTypeMapping(scrutinee.`type`)
    }.map { bindings =>
      bindings.map { case (k, v) => k -> Typed[Value](Value.variable(k, v), v) }
    }
    val bindings = patternsBinding.foldLeft(Map.empty[Var.Local, Typed[Value]]) {
      (acc, bindings) => acc ++ bindings
    }
    val (body, ty) = env.withLocals[Synth](bindings) { clause.body.synth }.unpack
    val substitutedType = patterns.zip(scrutinees).zip(patternsBinding).foldLeft(ty.readBack) {
      case (ty, ((pattern, scrutinee), binding)) => env.withLocals(binding) {
        ty.substitute(pattern.toTerm, scrutinee.term)
      }
    }
    ClauseSynth(Clause(patterns, body), substitutedType, ty)
  }

  extension (definition: Definition[Expr]) {
    def synth(implicit env: Environment.Typed[Value]): Definition[Term] = synthDefinition(definition)
  }
  
  def synthDefinition(definition: Definition[Expr])(
    implicit env: Environment.Typed[Value]
  ): Definition[Term] = definition match {

    case DefinedFunction(ident, paramExprs, resultTypeExpr, dependencies, pristineBody) => {
      val (params, envParams) = synthParams(paramExprs)
      val (resultType, _) = resultTypeExpr.synth(envParams).unpack
      // Try to obtain the declaration of the function from the environment
      val defVar: Var.Defined[Term, Function] = env.declarations.get(Var.Defined(ident.name)) match {
        case Some(decl) => Var.Defined[Term, Function](decl.ident.name)
        case _ => Var.Defined[Term, Function](ident.name)
      }
      val updatedDependencies: Set[Var.Defined[Term, Function]] = dependencies.map { dependency =>
        env.getSymbolByName(dependency.name) match {
          case Some(symbol) => symbol.ident.asInstanceOf[Var.Defined[Term, Function]]
          case None => Var.Defined[Term, Function](dependency.name)
        }
      }
      val function = DefinedFunction[Term](defVar, params, resultType, updatedDependencies)
      function.body := envParams.defineFunction(function.ident) {
        implicit env => pristineBody.get.elaborate(resultType).normalize
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
      envParams.defineFunction[Inductive[Term]](inductiveDefinition.ident) { implicit env =>
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

  def synthDeclaration(definition: Definition[Expr])(
    implicit env: Environment.Typed[Value]
  ): Declaration[Term, ?] = definition match {

    case definition @ DefinedFunction(_, paramExprs, resultTypeExpr, _, _) => {
      val (params, envParams) = synthParams(paramExprs)
      val (resultType, _) = resultTypeExpr.synth(envParams).unpack
      NaiveDeclaration[Term, Function](definition.toIdent[Term], params, resultType, SymbolKind.Function)
    }

    case inductive @ Inductive(_, paramExprs, _) => {
      val (params, _) = synthParams(paramExprs)
      NaiveDeclaration[Term, Inductive](inductive.toIdent[Term], params, Term.Universe, SymbolKind.Inductive)
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
    implicit env: Environment.Typed[Value] // ^1
  ): (Seq[Param[Term]], Environment.Typed[Value]) = {
    paramExprs.foldLeft((Seq.empty[Param[Term]], env: Environment.Typed[Value])) {
      case ((params, env /* ^2 */), paramExpr) => {
        // Notice, here `env` should be explicitly passed!
        // Since the priority of implicit given `env` (^1) is higher
        // than the `env` (^2) in the inner scope
        val paramTypeTerm = paramExpr.`type`.synth(env).term
        val paramType = paramTypeTerm.eval(env)
        val param = Param(paramExpr.ident, paramTypeTerm)
        (params :+ param, env.add(param.ident, Value.variable(param.ident, paramType), paramType))
      }
    }
  }

  def synthSymbol(decl: Symbol[Term])(
    implicit env: Environment.Typed[Value]
  ): Synth = decl match {

    case definition: NaiveDefinition[Term] => Synth(
      term = definition.params.buildLambda(definition.buildInvoke(Term)),
      `type` = definition.params.buildPiType(definition.resultType).eval,
    )
    
    case overloaded: Overloaded[Term] => {
      val lambdaPaths = overloaded.overloads.map(overloaded => (overloaded.params, overloaded.buildInvoke(Term)))
      val piPaths = overloaded.overloads.map(overloaded => (overloaded.params, overloaded.resultType))
      val lambda = Term.overloaded(Term.OverloadedLambda.apply, lambdaPaths)
      val pi = Term.overloaded(Term.OverloadedPi.apply, piPaths)
      Synth(lambda, pi.eval)
    }

    case declaration: NaiveDeclaration[Term, ?] => Synth(
      term = declaration.params.buildLambda(declaration.buildInvoke(Term)),
      `type` = declaration.params.buildPiType(declaration.resultType).eval,
    )

    case overloaded: OverloadedDeclaration[Term] => {
      val lambdaPaths = overloaded.overloads.map(overloaded => (overloaded.params, overloaded.buildInvoke(Term)))
      val piPaths = overloaded.overloads.map(overloaded => (overloaded.params, overloaded.resultType))
      val lambda = Term.overloaded(Term.OverloadedLambda.apply, lambdaPaths)
      val pi = Term.overloaded(Term.OverloadedPi.apply, piPaths)
      Synth(lambda, pi.eval)
    }

  }

end Synthesis
