package saki.core.elaborate

import saki.core.syntax.*

object Normalize {

  type Context = Map[Var.Local, Term]
  type RenameMap = Map[Var.Local, Var.Local]

  extension (ctx: Context) {
    private def withParam[R](param: Var.Local)(action: Context => R): R = {
      action(ctx + (param -> Term.Variable(param)))
    }
  }
  
  def normalizeTerm(term: Term, ctx: Context): Term = {
    given RenameMap = Map.empty
    given Context = ctx
    term match {

      case Term.Primitive(_) | Term.PrimitiveType(_) | Term.Universe => term

      case Term.Variable(variable) => ctx.get(variable).map(_.rename).getOrElse(term)

      case Term.Pi(param, codomain) => Term.Pi(param, codomain.normalize)

      case Term.Sigma(param, codomain) => Term.Sigma(param, codomain.normalize)

      case Term.ApplyOnce(fn, arg) => {
        fn.normalize match {
          case Term.Lambda(param, body) => ctx.withParam(param) {
            // TODO: need subst?
            newCtx => body.normalize(newCtx)
          }
          case fnNorm => Term.ApplyOnce(fnNorm, arg.normalize)
        }
      }

      case Term.FunctionCall(fnRef, args) => {
        val fn = fnRef.definition.get
        val argsNorm = args.map(_.normalize(ctx))
        given Context = fn.arguments.zip(argsNorm).foldLeft(ctx) {
          case (acc, (param, arg)) => acc + (param -> arg)
        }
        fn.body.toOption match {
          case Some(term) => term.normalize
          case None => Term.FunctionCall(fnRef, argsNorm)
        }
      }

      case Term.InductiveCall(inductive, args) => Term.InductiveCall(inductive, args.map(_.normalize))

      case Term.ConstructorCall(cons, args, inductiveArgs) => {
        Term.ConstructorCall(cons, args.map(_.normalize), inductiveArgs.map(_.normalize))
      }

      case Term.Match(scrutinees, clauses) => {
        val scrutineesNorm = scrutinees.map(_.normalize)
        clauses.tryMatch(scrutineesNorm).map(_.normalize).getOrElse {
          Term.Match(scrutineesNorm, clauses)
          // PatternError.noMatch(scrutineesNorm.toString, clauses.last.patterns.head.span)
        }
      }

      case Term.Record(fields) => Term.Record(fields.view.mapValues(_.normalize).toMap)

      case Term.RecordType(fields) => Term.RecordType(fields.view.mapValues(_.normalize).toMap)

      case Term.Lambda(param, body) => Term.Lambda(param, body.normalize)

      case Term.Projection(record, field) => {
        record.normalize match {
          case Term.Record(fields) => fields(field).normalize
          case recordNorm => Term.Projection(recordNorm, field)
        }
      }
      // end of term match
    }
    // end of normalizeTerm
  }

  def renameTerm(term: Term)(implicit map: RenameMap): Term = term match {
    case Term.Primitive(_) | Term.PrimitiveType(_) | Term.Universe => term

    case Term.Variable(variable) => Term.Variable(map.getOrElse(variable, variable))

    case Term.Lambda(param, body) => {
      val withParam: RenameMap = map + (param -> Var.Local(param.name))
      Term.Lambda(param, body.rename(withParam))
    }

    case Term.Pi(param, codomain) => {
      val withParam: RenameMap = map + (param.ident -> Var.Local(param.name))
      Term.Pi(param, codomain.rename(withParam))
    }

    case Term.Sigma(param, codomain) => {
      val withParam: RenameMap = map + (param.ident -> Var.Local(param.name))
      Term.Sigma(param, codomain.rename(withParam))
    }

    case Term.Record(fields) => Term.Record(fields.view.mapValues(_.rename).toMap)

    case Term.RecordType(fields) => Term.RecordType(fields.view.mapValues(_.rename).toMap)

    case Term.ApplyOnce(fn, arg) => Term.ApplyOnce(fn.rename, arg.rename)

    case Term.InductiveCall(inductive, args) => Term.InductiveCall(inductive, args.map(_.rename))

    case Term.FunctionCall(fn, args) => Term.FunctionCall(fn, args.map(_.rename))

    case Term.ConstructorCall(cons, args, inductiveArgs) => {
      Term.ConstructorCall(cons, args.map(_.rename), inductiveArgs.map(_.rename))
    }

    case Term.Match(scrutinees, clauses) => {
      Term.Match(scrutinees.map(_.rename), clauses.map(clause => clause.map(_.rename)))
    }

    case Term.Projection(record, field) => Term.Projection(record.rename, field)
  }

}
