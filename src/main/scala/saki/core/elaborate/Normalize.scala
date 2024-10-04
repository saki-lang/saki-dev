package saki.core.elaborate

import saki.core.syntax.*

import scala.collection.Seq

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

      case Term.Apply(fn, arg) => {
        fn.normalize match {
          case Term.Lambda(param, body) => ctx.withParam(param) {
            // TODO: need subst?
            newCtx => body.normalize(newCtx)
          }
          case fnNorm => Term.Apply(fnNorm, arg.normalize)
        }
      }

      case Term.FunctionInvoke(fnRef, args) => {
        val fn = fnRef.definition.get
        val argsNorm: Seq[Term] = args.map(_.normalize(ctx))
        fn.body.toOption match {
          // TODO: if all arguments are pure values, we can evaluate the function
          case Some(term) if !fn.isNeutral => {
            given Context = fn.arguments(argsNorm).foldLeft(ctx) {
              case (acc, (param, arg)) => acc + (param -> arg)
            }
            term.normalize
          }
          case _ => Term.FunctionInvoke(fnRef, argsNorm)
        }
      }

      case Term.InductiveType(inductive, args) => Term.InductiveType(inductive, args.map(_.normalize))

      case Term.InductiveVariant(cons, args, inductiveArgs) => {
        Term.InductiveVariant(cons, args.map(_.normalize), inductiveArgs.map(_.normalize))
      }

      case Term.Match(scrutinees, clauses) => {
        val scrutineesNorm = scrutinees.map(_.normalize)
        clauses.map {
          clause => clause.mapPatterns(_.map(_.normalize))
        }.tryMatch(scrutineesNorm).map(_.normalize).getOrElse {
          Term.Match(scrutineesNorm, clauses)
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

    case Term.Apply(fn, arg) => Term.Apply(fn.rename, arg.rename)

    case Term.InductiveType(inductive, args) => Term.InductiveType(inductive, args.map(_.rename))

    case Term.FunctionInvoke(fn, args) => Term.FunctionInvoke(fn, args.map(_.rename))

    case Term.InductiveVariant(cons, args, inductiveArgs) => {
      Term.InductiveVariant(cons, args.map(_.rename), inductiveArgs.map(_.rename))
    }

    case Term.Match(scrutinees, clauses) => {
      Term.Match(scrutinees.map(_.rename), clauses.map(clause => clause.map(_.rename)))
    }

    case Term.Projection(record, field) => Term.Projection(record.rename, field)
  }

}
