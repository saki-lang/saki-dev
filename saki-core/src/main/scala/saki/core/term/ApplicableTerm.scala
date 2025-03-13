package saki.core.term

import saki.core.context.Environment
import saki.core.domain.{Type, Value}
import saki.core.syntax.{CodomainClosure, Param, ParamList}
import saki.error.CoreErrorKind.{OverloadingAmbiguous, TypeNotMatch}
import saki.util.unreachable

import scala.collection.Seq

private trait ApplicableTerm {
  def param: Param[Term]
  def body: Term

  def eval(constructor: (Type, CodomainClosure) => Value, evalMode: EvalMode)(
    implicit env: Environment.Typed[Value]
  ): Value = {
    val (paramType, closure) = Term.evalParameterized(param, body, evalMode)
    constructor(paramType, closure)
  }

  def toOverloaded: OverloadedTerm[?] & Term = this match {
    case lambda: Lambda => OverloadedLambda(Map(lambda.param -> lambda.body))
    case pi: Pi => OverloadedPi(Map(pi.param -> pi.codomain))
    case _ => unreachable
  }
}

private trait ApplicableTypeTerm extends ApplicableTerm {
  def codomain: Term
  override def body: Term = codomain
}

extension (params: ParamList[Term]) {
  def buildPiType(body: Term): Term = params.foldRight(body) {
    case (param, body) => Pi(param, body)
  }

  def buildLambda(body: Term): Term = params.foldRight(body) {
    case (param, body) => Lambda(param, body)
  }
}

private trait OverloadedTerm[S <: Term & OverloadedTerm[S]] {

  def states: Map[Param[Term], Term]

  def eval(constructor: Map[Type, CodomainClosure] => Value, evalMode: EvalMode)(
    implicit env: Environment.Typed[Value]
  ): Value = {
    // Normalize the states (unify the parameters with identical type but different names)
    val closureStates: Seq[(Type, CodomainClosure)] = states.toSeq.map {
      (param, term) => Term.evalParameterized(param, term, evalMode)
    }

    // Merge the states with identical parameter types
    //  1. Group the states by parameter type
    val grouped: Map[Type, Iterable[CodomainClosure]] = closureStates.groupBy(_._1.readBack).map {
      (_, states) => states.map(states.head._1 -> _._2)
    }.flatten.groupMap(_._1)(_._2)

    //  2. Merge the states with identical parameter types
    val merged: Map[Type, CodomainClosure] = grouped.map { (paramType, closures) =>
      assert(closures.nonEmpty)
      if closures.size == 1 then paramType -> closures.head else {
        val merged = closures.map { closure =>
          val (_, bodyTerm) = Value.readBackClosure(paramType, closure)
          bodyTerm match {
            // case 1: The term is a lambda-like term, convert it to an overloaded lambda
            case lambdaLikeTerm: ApplicableTerm => lambdaLikeTerm.toOverloaded
            // case 2: The term is an overloaded lambda, keep it as is
            case overloaded: OverloadedTerm[?] => overloaded
            // case 3: The term is not a lambda-like term, indicating an ambiguous overload
            case _ => OverloadingAmbiguous.raise(s"Ambiguous overloading for function: ${paramType}")
          }
        }.reduce((merged, overloaded) => merged.merge(overloaded))
        Term.evalParameterized(paramType, merged, evalMode)
      }
    }
    constructor(merged)
  }

  def copy(states: Map[Param[Term], Term]): S

  @SuppressWarnings(Array("unchecked"))
  private def merge(other: OverloadedTerm[?]): S = {
    assert(this.getClass == other.getClass)
    val mergedStates = (this.states.keySet ++ other.states.keySet).map { param =>
      val term: Term = (this.states.get(param), other.states.get(param)) match {
        case (Some(term1: Term), Some(term2: Term)) => (term1, term2) match {
          // States are overloaded lambdas, merge them recursively
          case (overloaded1: OverloadedTerm[?], overloaded2: OverloadedTerm[?]) => {
            // Recursively merge the states
            try overloaded1.asInstanceOf[S].merge(overloaded2.asInstanceOf[S]) catch {
              // If the merge fails, the states are not compatible (e.g. trying to merge a Pi and a Lambda)
              case _: ClassCastException => TypeNotMatch.raise(s"Cannot merge states of different types: ${term1}, ${term2}")
            }
          }
          // States are not overloaded lambdas, indicating a mismatch
          case _ => TypeNotMatch.raise(s"Cannot merge states of different types: ${term1}, ${term2}")
        }
        case (Some(term: Term), _) => term
        case (_, Some(term: Term)) => term
        case (None, None) => unreachable
      }
      param -> term
    }.toMap[Param[Term], Term]
    this.copy(mergedStates)
  }
}

