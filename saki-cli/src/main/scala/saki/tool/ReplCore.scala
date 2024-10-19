package saki.tool

import saki.cli.ReadEvalPrintLoop
import saki.concrete.{ErrorListener, SyntaxError, Visitor}
import saki.concrete.syntax.{Definition, Evaluation, Statement}
import saki.core.context.{Environment, Typed}
import saki.core.domain.Value
import saki.core.elaborate.Resolve
import saki.core.elaborate.Resolve.{preResolve, resolve}
import saki.core.elaborate.Synthesis.synth
import saki.core.syntax.{Expr, Var}
import saki.core.syntax.Module.EvalResult
import saki.error.{CoreErrorKind, Error}
import saki.prelude.Prelude

private class ReplCore {
  val visitor: Visitor = Visitor()
  implicit var environment: Environment.Typed[Value] = Prelude.environment
  implicit var resolveContext: Resolve.Context = Resolve.Context(Prelude.symbols)

  def evaluate(expr: Expr): EvalResult = {
    val (term, ty) = expr.resolve(resolveContext)._1.synth(environment).unpack
    EvalResult(term.normalize(environment), ty.readBack(environment))
  }

  def iterate(source: String): Unit = {

    if source.trim == "env" then {
      environment.locals.foreach { case (local, Typed(value, ty)) =>
        println(s"  $local: ${ty.readBack(environment)} = ${value.readBack(environment)}")
      }
      println("")
      return
    }

    val listener = ErrorListener(source)
    try {
      val parser = parseSource(source, listener)
      val entities = visitor.visitProgram(parser.program())
      val definitions = entities.collect { case defn: Definition => defn }.map(_.emit)
      val evaluations = entities.collect { case eval: Evaluation => eval }

      catchError(source) { _ =>

        resolveContext = definitions.foldLeft(resolveContext) {
          (ctx, definition) => definition.preResolve(ctx)
        }

        val (newContext, newEnv) = definitions.foldLeft((resolveContext, environment)) {
          case ((resolvingContext, env), definition) => {
            val (resolved, newCtx) = definition.resolve(resolvingContext)
            val definitionSynth = resolved.synth(env)
            (newCtx, env.add(definitionSynth))
          }
        }

        resolveContext = newContext
        environment = newEnv

        evaluations.foreach { evaluation => println("  " + evaluate(evaluation.expr.emit)) }
      }

    } catch {
      // Not seems to be a definition, evaluate it as an expression
      case _: SyntaxError => {
        try {
          val listener = ErrorListener(source)
          val parser = parseSource(source, listener)
          val expr = visitor.visitStatement(parser.statement())

          given Resolve.Context = resolveContext
          given Environment.Typed[Value] = environment

          expr match {
            case Statement.Let(name, expectedTypeExpr, valueExpr) => {
              val (term, ty) = valueExpr.emit.resolve._1.synth.unpack
              expectedTypeExpr match {
                case Some(expectedTypeExpr) => {
                  val expectedType = expectedTypeExpr.emit.resolve._1.synth.term.normalize
                  if !(expectedType.eval <:< ty) then {
                    throw CoreErrorKind.TypeNotMatch.raise(valueExpr.span) {
                      s"Expected type ${expectedType}, but got a ${ty.readBack(environment)}"
                    }
                  }
                }
                case None => ()
              }
              val variable: Var.Local = Var.Local(name)
              environment = environment.add(Var.Local(name), term.eval, ty)
              resolveContext += variable
              println(s"  $name = ${term.normalize(environment)} : ${ty.readBack(environment)}\n")
            }
            case Statement.Expression(expr) => {
              println(s"  ${evaluate(expr.emit)}\n")
            }
          }
        } catch {
          case error: Error => printError(source, error)
          case error: Throwable => throw error // println(s"Error: ${error.getMessage}")
        }
      }
      case error: Error => printError(source, error)
      case error: Throwable => println(s"Error: ${error.getMessage}")
    }
  }

  private def printError(source: String, error: Error): Unit = {
    error.infoSpans.headOption match {
      case Some((span, info)) => {
        val spanLength = span.end - span.start + 1
        ReadEvalPrintLoop.printError(source, "", error.message, info, span.start, spanLength)
      }
      case None => println(s"Error: ${error.message}")
    }
  }
}
