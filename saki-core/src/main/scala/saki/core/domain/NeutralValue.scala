package saki.core.domain

import saki.core.context.{Environment, Typed}
import saki.core.syntax.{buildTypeMapping, getOverload, Clause, Function, Inductive, NaiveDeclaration, OverloadedSymbol, Var}
import saki.core.term
import saki.core.term.Term
import saki.error.CoreErrorKind.*

import scala.annotation.targetName
import scala.collection.Seq

enum NeutralValue {

  case Variable(ident: Var.Local, `type`: Type)

  case TypeBarrier(value: Value, `type`: Type)

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

  def infer(implicit env: Environment.Typed[Value]): Type = this match {

    case Variable(_, ty) => ty

    case TypeBarrier(_, ty) => ty

    case Apply(fn, arg) => fn.infer match {
      case Value.Pi(paramType, codomain) => {
        val argType = arg.infer
        if !(paramType <:< argType) then TypeNotMatch.raise {
          s"Expected argument type: ${paramType.reflect}, but got: ${argType.reflect}"
        }
        // To obtain the concrete return type, feed the concrete argument to the codomain closure
        codomain(arg.eval)
      }

      case Value.OverloadedPi(states) => {
        val argType = arg.infer
        // Find the states that the argument type is a subtype of the parameter type
        val candidateStates = states.filter {
          (paramType, _) => paramType <:< argType
        }
        if candidateStates.isEmpty then OverloadingNotMatch.raise {
          s"No matched overloading in overloaded Pi type of argument with type: ${argType.reflect}"
        }
        // Find the states that there is no other state that is closer to the argument type
        val validStates = candidateStates.filter {
          (paramType, _) => !candidateStates.exists { (paramType2, _) =>
            paramType2 != paramType && paramType2 <:< paramType
          }
        }
        if validStates.size > 1 then OverloadingAmbiguous.raise {
          s"Ambiguous overloading in overloaded Pi type of argument with type: ${argType.reflect}"
        }
        val (_, codomain) = validStates.head
        codomain(arg.eval)
      }

      case _ => TypeNotMatch.raise {
        s"Cannot apply an argument to a non-function value: $fn"
      }
    }

    case NeutralValue.Projection(record, field) => record.infer match {
      case Value.RecordType(fieldTypes) => fieldTypes(field)
      case ty => TypeNotMatch.raise {
        s"Expected record type, but got: ${ty.reflect}"
      }
    }

    case NeutralValue.FunctionInvoke(fn, args) => env.getSymbol(fn).get match {
      case fn: (Function[Term] | NaiveDeclaration[Term, ?]) => {
        val updatedEnv = fn.params.zip(args).foldLeft(env) {
          case (env, (param, arg)) => {
            val paramType = param.`type`.eval(env)
            val argType = arg.infer(env)
            if !(paramType <:< argType) then TypeNotMatch.raise {
              s"Expected argument type: ${paramType.reflect}, but got: ${argType.reflect}"
            }
            env.add(param.ident, arg.eval(env), paramType)
          }
        }
        fn.resultType(Term).eval(updatedEnv)
      }
      case overloaded: OverloadedSymbol[Term, ?, Function[Term]] @unchecked => {
        val func = overloaded.getOverload(args)
        val paramMap = func.params.map { param =>
          val paramType = param.`type`.eval
          (param.ident, Typed[Value](Value.variable(param.ident, paramType), paramType))
        }.toMap
        env.withLocals(paramMap) { implicit env => func.resultType(Term).eval(env) }
      }
      case _: Inductive[Term] => Value.Universe
      case _ => TypeNotMatch.raise {
        s"Expected function or inductive type, but got: ${fn}"
      }
    }

    case NeutralValue.Match(scrutinees, clauses) => {
      val scrutineesType = scrutinees.map(_.infer)
      val clausesType: Seq[Value] = clauses.map { clause =>
        val bindings: Seq[(Var.Local, Typed[Value])] = scrutineesType.zip(clause.patterns).flatMap {
          (scrutinee, pattern) => pattern.buildTypeMapping(scrutinee)
        }.map {
          case (param, ty) => (param, Typed[Value](Value.variable(param, ty), ty))
        }
        env.withLocals(bindings.toMap) { implicit env => clause.body.infer(env) }
      }
      clausesType.reduce((a, b) => a \/ b)
    }
  }

  def reflect(implicit env: Environment.Typed[Value]): Term = this match {
    case Variable(ident, _) => term.Variable(ident)
    case Apply(fn, arg) => term.Apply(fn.reflect, arg.reflect)
    case TypeBarrier(value, ty) => term.TypeBarrier(value.reflect, ty.reflect)
    case Projection(record, field) => term.Projection(record.reflect, field)
    case FunctionInvoke(fnRef, args) => term.FunctionInvoke(fnRef, args.map(_.reflect))
    case Match(scrutinees, clauses) => {
      // TODO: May be we don't need to build the match bindings here
      val termClauses = clauses.map { clause =>
        // Bind the pattern variables to the scrutinee values
        val bindings: Seq[(Var.Local, Typed[Value])] = scrutinees.zip(clause.patterns).flatMap {
          (scrutinee, pattern) =>
            val scrutineeType = scrutinee.infer
            pattern.buildTypeMapping(scrutineeType)
        }.map {
          case (param, ty) => (param, Typed[Value](Value.variable(param, ty), ty))
        }
        val bodyTerm = env.withLocals(bindings.toMap) { implicit env => clause.body.reflect }
        Clause(clause.patterns.map(_.map(_.reflect)), bodyTerm)
      }
      term.Match(scrutinees.map(_.reflect), termClauses)
    }
  }

  def containsMatching(implicit env: Environment.Typed[Value]): Boolean = this match {
    case Variable(_, _) => false
    case TypeBarrier(value, _) => value.containsMatching
    case Apply(fn, arg) => fn.containsMatching || arg.containsMatching
    case Projection(record, _) => record.containsMatching
    case FunctionInvoke(_, args) => args.exists(_.containsMatching)
    case Match(_, _) => true
  }

  def isFinal(variables: Set[Var.Local] = Set.empty)(
    implicit env: Environment.Typed[Value]
  ): Boolean = this match {
    case Variable(ident, _) => variables.contains(ident)
    case TypeBarrier(value, _) => value.isFinal(variables)
    case Apply(fn, arg) => fn.isFinal(variables) && arg.isFinal(variables)
    case Projection(record, _) => record.isFinal(variables)
    case FunctionInvoke(_, args) => args.forall(_.isFinal(variables))
    case Match(scrutinees, clauses) => {
      scrutinees.forall(_.isFinal(variables)) && clauses.forall { clause =>
        clause.patterns.forall(_.forall(_.isFinal(variables))) && clause.body.isFinal(variables)
      }
    }
  }

  infix def unify(that: NeutralValue)(implicit env: Environment.Typed[Value]): Boolean = (this, that) match {

    case (lhs, rhs) if lhs == rhs => true

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
