package saki.core

object Elaborate {

  case class Context(
    definitions: Map[Var.Defined[?], Definition],
    locals: Map[Var.Local, Type],
  ) {
    private[Elaborate] def withLocal[R](local: Var.Local, `type`: Type)(action: Context => R): R = {
      action(copy(locals = locals.updated(local, `type`)))
    }

    def getDefinition(name: String): Option[Definition] = definitions.collectFirst {
      case (varDef, definition) if varDef.name == name => definition
    }
  }

  def elaborate(expr: Expr, expectedType: Type)(implicit ctx: Context): Term = expr match {
    case Expr.Lambda(lambdaParam, body) => {
      // Check that expectedType is a Pi type
      expectedType.normalize(Map.empty) match {
        case piType: Term.Pi => Term.Lambda(
          param = lambdaParam,
          body = ctx.withLocal(lambdaParam, piType.param.`type`) { elaborate(body, piType.codomain) }
        )
        case ty => TypeError.mismatch("Π (x : A) -> B", ty.toString, expr.span)
      }
    }
    case Expr.Hole(_) =>  TypeError.error("Holes are not allowed in this context", expr.span)
    case _ => {
      val synthResult = expr.synth
      if !(synthResult.`type` unify expectedType) then {
        TypeError.mismatch(expectedType.toString, synthResult.`type`.toString, expr.span)
      }
      synthResult.term
    }
  }

  case class Synth(term: Term, `type`: Type) {
    def unpack: (Term, Type) = (term, `type`)
    def normalize: Synth = copy(term = term.normalize(Map.empty), `type` = `type`.normalize(Map.empty))
  }

  def synth(expr: Expr)(implicit ctx: Context): Synth = (expr match {

    case Expr.Primitive(value) => Synth(Term.Primitive(value), Term.PrimitiveType(value.ty))

    case Expr.PrimitiveType(ty) => Synth(Term.PrimitiveType(ty), Term.Universe(0))

    case Expr.Resolved(ref) => ref match {
      case definitionVar: Var.Defined[?] => definitionVar.definition match {
        case None => Synth(
          term = definitionVar.signature.params.buildLambda(definitionVar.call),
          `type` = definitionVar.signature.params.buildPiType(definitionVar.signature.resultType)
        )
        case Some(definition: Definition) => Synth(
          // TODO: should we remove `rename` here? (does it make sense?)
          term = definition.params.buildLambda(definition.ident.call).rename(Map.empty),
          `type` = definition.params.buildPiType(definition.resultType),
        )
      }
      case variable: Var.Local => ctx.locals.get(variable) match {
        case Some(ty) => Synth(Term.Ref(variable), ty)
        case None => TypeError.error(s"Unbound variable: ${variable.name}", expr.span)
      }
    }

    case Expr.Elimination(obj, member) => obj.synth.normalize.unpack match {
      // This is a project operation
      // `obj.field`
      // TODO: maybe we don't need to normalize here?
      case (term, recordType: Term.RecordType) => term match {
        case Term.Record(fields) => fields.get(member) match {
          case Some(value) => Synth(value, recordType.fields(member))
          case None => TypeError.error(s"Field not found: $member", expr.span)
        }
        case _ => Synth(Term.Projection(term, member), recordType.fields(member))
      }
      // This is a method call
      // `obj.method`
      case (term, _) => {
        val method: Definition.Function = ctx.getDefinition(member) match {
          case Some(definition: Definition.Function) => definition
          case _ => TypeError.error(s"Method not found: $member", expr.span)
        }
        Synth(Term.FunctionCall(method.ident, Seq(term)), method.resultType)
      }
    }

    case Expr.Pi(param, result) => synthDependentType(param, result)

    case Expr.Sigma(param, result) => synthDependentType(param, result)

    case _ => TypeError.error("Failed to synthesis expression", expr.span)
    
  }).normalize

  private def synthDependentType(param: Param[Expr], result: Expr)(implicit ctx: Context): Synth = {
    val (paramType, paramTypeType) = param.`type`.synth.unpack
    val (codomain, codomainType) = ctx.withLocal(param.ident, paramType) { result.synth(_).unpack }
    Synth(
      term = Term.Sigma(Param(param.ident, paramType), codomain),
      `type` = Term.Universe(Math.max(paramTypeType.universeLevel, codomainType.universeLevel))
    )
  }
}

extension (self: Expr) {
  def synth(implicit ctx: Elaborate.Context): Elaborate.Synth = Elaborate.synth(self)
  def infer(implicit ctx: Elaborate.Context, expected: Type): Term = Elaborate.elaborate(self, expected)
}

extension (self: Term) {
  def universeLevel: Int = self match {
    case Term.Universe(level) => level
    case _ => throw new Exception("Not a universe")
  }
}
