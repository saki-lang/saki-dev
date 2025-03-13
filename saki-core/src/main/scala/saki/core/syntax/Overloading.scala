package saki.core.syntax

import saki.core.RuntimeEntity
import saki.core.context.Environment
import saki.core.domain.{Type, Value}
import saki.core.term
import saki.core.term.Term
import saki.error.CoreErrorKind.{NoSuchOverloading, OverloadingAmbiguous}

import scala.collection.Seq

extension (self: OverloadedSymbol[Term, ?, Function[Term]]) {
  /**
   * Get the most suitable state of the overloaded function that matches the argument types.
   *
   * @param env The environment
   * @return The most suitable eigenstate of the overloaded function body
   */
  def getOverload[T <: RuntimeEntity[Type]](args: Seq[T])(implicit env: Environment.Typed[Value]): NaiveSymbol[Term] = {

    val fn = env.getSymbol(self.ident).get.asInstanceOf[OverloadedSymbol[Term, ?, ? <: NaiveSymbol[Term]]]

    // Filter out the candidate overloads that fit the argument types
    val candidateOverloads = fn.overloads.filter { overload =>
      val params = overload.params
      if params.size != args.size then false
      else params.zip(args).forall {
        (param, arg) => param.`type`.eval <:< arg.infer
      }
    }

    // If no candidate overload fits the argument types, throw an error
    if candidateOverloads.isEmpty then {
      NoSuchOverloading.raise {
        s"No overloading of function ${fn.ident.name} found for arguments of types: " +
          args.map(_.infer.readBack).mkString(", ")
      }
    }

    // Otherwise, find the most suitable one
    // The most suitable one is the one that has the most specific parameter types

    // Iterate through each parameter position, eliminating branches that are not
    // the most specific at that position
    var remainingOverloads = candidateOverloads
    val numParams = remainingOverloads.head.params.length

    for (i <- 0 until numParams if remainingOverloads.size > 1) {
      // Cache the current parameter type at position i for each overload
      val currentOtherParamTypes: Seq[Type] = remainingOverloads.map(_.params(i).`type`.eval)
      // Filter branches to keep those with the most specific parameter type at position i
      remainingOverloads = remainingOverloads.filter { overload =>
        val currentSelfParamType = overload.params(i).`type`.eval
        // Check whether the current parameter type is more specific than or equal to all other parameter types
        // at this position across the remaining branches.
        currentOtherParamTypes.forall { otherParamType =>
          currentSelfParamType <:< otherParamType || !(otherParamType <:< currentSelfParamType)
        }
      }
    }

    if (remainingOverloads.size != 1) {
      OverloadingAmbiguous.raise {
        s"Ambiguous overloading for function ${fn.ident.name} with arguments of types: " +
          args.map(_.infer.readBack).mkString(", ")
      }
    }

    remainingOverloads.head
  }
}