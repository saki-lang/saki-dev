package saki.tool

import saki.concrete.Visitor
import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.elaborate.Resolve
import saki.core.syntax.Expr
import saki.core.syntax.Module.EvalResult
import saki.prelude.Prelude

private class ReplCore {
  val visitor: Visitor = Visitor()
  var environment: Environment.Typed[Value] = Prelude.environment
  var resolveContext: Resolve.Context = Resolve.Context(Prelude.symbols)

  def evaluate(expr: Expr): EvalResult = {
    val (term, ty) = expr.resolve(Resolve.Context(environment))._1.synth(environment).unpack
    EvalResult(term.normalize(environment), ty.readBack(environment))
  }
}
