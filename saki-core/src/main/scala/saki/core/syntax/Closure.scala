package saki.core.syntax

import saki.core.context.{Environment, Typed}
import saki.core.domain.{NeutralValue, Value}
import saki.core.domain

type CodomainClosure = (Value | NeutralValue.Variable) => Value

case class ParameterizedClosure(param: Param[Value], env: Environment.Typed[Value])(
  action: Environment.Typed[Value] => Value
) extends ((Value | NeutralValue.Variable) => Value) {
  override def apply(argument: Value | NeutralValue.Variable): Value = {
    val (closureEnv, argValue) = argument match {
      case NeutralValue.Variable(ident, ty) => {
        val variable = Value.variable(ident, ty)
        (env.add(ident, variable, ty), variable)
      }
      case value: Value => (env, value)
    }
    closureEnv.withLocal(param.ident, Typed[Value](argValue, param.`type`)) {
      implicit env => action(env)
    }
  }
}
