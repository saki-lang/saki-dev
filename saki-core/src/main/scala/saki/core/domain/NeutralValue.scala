package saki.core.domain

import saki.core.context.{Environment, Typed}
import saki.core.syntax.{buildMatchBindings, Clause, Function, Term, Var}

import scala.annotation.targetName
import scala.collection.Seq

enum NeutralValue {

  case Variable(ident: Var.Local, `type`: Type)

  case Apply(fn: NeutralValue, arg: Value)

  case Projection(record: Value, field: String)

  // global function call
  case FunctionInvoke(
    fn: Var.Defined[Term, Function],
    args: Seq[Value],
  )

  case Match(
    scrutinees: Seq[Value],
    clauses: Seq[Clause[Value]]
  )

  def infer(implicit env: Environment.Typed[Value]): Type = this.readBack.infer

  def readBack(implicit env: Environment.Typed[Value]): Term = this match {
    case Variable(ident, _) => Term.Variable(ident)
    case Apply(fn, arg) => Term.Apply(fn.readBack, arg.readBack)
    case Projection(record, field) => Term.Projection(record.readBack, field)
    case FunctionInvoke(fnRef, args) => Term.FunctionInvoke(fnRef, args.map(_.readBack))
    case Match(scrutinees, clauses) => {
      // TODO: May be we don't need to build the match bindings here
      val termClauses = clauses.map { clause =>
        // Bind the pattern variables to the scrutinee values
        val bindings: Seq[(Var.Local, Typed[Value])] = scrutinees.zip(clause.patterns).flatMap {
          (scrutinee, pattern) =>
            val scrutineeType = scrutinee.infer
            pattern.buildMatchBindings(scrutineeType)
        }.map {
          case (param, ty) => (param, Typed[Value](Value.variable(param, ty), ty))
        }
        val bodyTerm = env.withLocals(bindings.toMap) { implicit env => clause.body.readBack }
        Clause(clause.patterns.map(_.map(_.readBack)), bodyTerm)
      }
      Term.Match(scrutinees.map(_.readBack), termClauses)
    }
  }

  def containsMatching(implicit env: Environment.Typed[Value]): Boolean = this match {
    case Variable(_, _) => false
    case Apply(fn, arg) => fn.containsMatching || arg.containsMatching
    case Projection(record, _) => record.containsMatching
    case FunctionInvoke(_, args) => args.exists(_.containsMatching)
    case Match(_, _) => true
  }

  def isFinal(variables: Set[Var.Local] = Set.empty)(
    implicit env: Environment.Typed[Value]
  ): Boolean = this match {
    case Variable(ident, _) => variables.contains(ident)
    case Apply(fn, arg) => fn.isFinal(variables) && arg.isFinal(variables)
    case Projection(record, _) => record.isFinal(variables)
    case FunctionInvoke(_, args) => args.forall(_.isFinal(variables))
    case Match(scrutinees, clauses) => {
      scrutinees.forall(_.isFinal(variables)) && clauses.forall { clause =>
        clause.patterns.forall(_.forall(_.isFinal(variables))) && clause.body.isFinal(variables)
      }
    }
  }

  infix def unify(that: NeutralValue)(
    implicit env: Environment.Typed[Value]
  ): Boolean = (this, that) match {

    case (Variable(ident1, ty1), Variable(ident2, ty2)) => ident1 == ident2 && ty1 == ty2
    case (Apply(fn1, arg1), Apply(fn2, arg2)) => (fn1 unify fn2) && (arg1 unify arg2)

    case (Projection(record1, field1), Projection(record2, field2)) => {
      (record1 unify record2) && (field1 == field2)
    }

    case (FunctionInvoke(fn1, args1), FunctionInvoke(fn2, args2)) => {
      fn1 == fn2 && args1.zip(args2).forall((arg1, arg2) => arg1 unify arg2)
    }

    case (Match(scrutinees1, clauses1), Match(scrutinees2, clauses2)) => {
      scrutinees1.zip(scrutinees2).forall((scrutinee1, scrutinee2) => scrutinee1 unify scrutinee2) &&
        clauses1.forall { clause1 =>
          clauses2.find(_.patterns == clause1.patterns) match {
            case Some(clause2) => clause1.body unify clause2.body
            case None => false
          }
        }
    }

    case _ => false
  }

  @targetName("subtype")
  infix def <:<(that: NeutralValue)(
    implicit env: Environment.Typed[Value]
  ): Boolean = (this, that) match {

    // Variable subtyping: variables must match
    case (Variable(ident1, ty1), Variable(ident2, ty2)) => ident1 == ident2 && ty1 <:< ty2

    // Application subtyping: function applications must have subtyping in both function and argument
    case (Apply(fn1, arg1), Apply(fn2, arg2)) => fn1 <:< fn2 && arg1 <:< arg2

    // Projection subtyping: projections must have the same field and record subtype
    case (Projection(record1, field1), Projection(record2, field2)) => {
      field1 == field2 && record1 <:< record2
    }

    // FunctionInvoke subtyping: invoked functions must be the same, and all arguments must be subtypes
    case (FunctionInvoke(fn1, args1), FunctionInvoke(fn2, args2)) => {
      fn1 == fn2 && args1.zip(args2).forall { case (arg1, arg2) => arg1 <:< arg2 }
    }

    // Match subtyping: scrutinees and clauses must match
    case (Match(scrutinees1, clauses1), Match(scrutinees2, clauses2)) => {
      scrutinees1.zip(scrutinees2).forall { case (scrutinee1, scrutinee2) => scrutinee1 <:< scrutinee2 } &&
        clauses1.forall { clause1 =>
          clauses2.find(_.patterns == clause1.patterns) match {
            case Some(clause2) => clause1.body <:< clause2.body
            case None => false
          }
        }
    }

    // Default case: no subtyping relationship
    case _ => false
  }

}