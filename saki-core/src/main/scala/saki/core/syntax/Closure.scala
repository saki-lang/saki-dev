package saki.core.syntax

import saki.core.context.{Environment, Typed}
import saki.core.domain
import saki.core.domain.Value

type CodomainClosure = Value => Value

case class ParameterizedClosure(param: Param[Value], env: Environment.Typed[Value])(
  action: Environment.Typed[Value] => Value
) extends (Value => Value) {
  override def apply(argument: Value): Value = {
    env.withLocal(param.ident, Typed[Value](argument, param.`type`)) {
      implicit env => action(env)
    }
  }
}
