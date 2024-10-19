package saki.tool

import saki.cli.{ReadEvalPrintLoop, SakiToolTrait}
import saki.concrete.{ErrorListener, SyntaxError}
import saki.concrete.syntax.{Definition, Evaluation, Statement}
import saki.core.context.{Environment, Typed}
import saki.core.domain.Value
import saki.core.elaborate.Resolve.{preResolve, resolve}
import saki.core.elaborate.Resolve
import saki.core.elaborate.Synthesis.synth
import saki.core.syntax.Var
import saki.error.{CoreErrorKind, Error}
import saki.prelude.Prelude

import java.io.File
import scala.io.Source

object SakiTool extends SakiToolTrait {

  lazy val repl = new ReplCore()

  override def run(file: File): Unit = {
    val source = Source.fromFile(file)
    try compileModule(source.mkString, Some(file.getAbsolutePath)) catch {
      case _: Error => ()
      case exception => throw exception
    }
    source.close()
  }

  override def iterate(source: String): Unit = {
    val listener = ErrorListener(source)
    try {
      val parser = parseSource(source, listener)
      val entities = repl.visitor.visitProgram(parser.program())
      val definitions = entities.collect { case defn: Definition => defn }.map(_.emit)
      val evaluations = entities.collect { case eval: Evaluation => eval }

      catchError(source) { _ =>

        definitions.foldLeft(repl.resolveContext) { (ctx, definition) => definition.preResolve(ctx) }

        val (context, finalEnv) = definitions.foldLeft((repl.resolveContext, Prelude.environment)) {
          case ((resolvingContext, env), definition) => {
            val (resolved, newCtx) = definition.resolve(resolvingContext)
            val definitionSynth = resolved.synth(env)
            (newCtx, env.add(definitionSynth))
          }
        }

        repl.resolveContext = context
        repl.environment = finalEnv

        evaluations.foreach { evaluation => println("  " + repl.evaluate(evaluation.expr.emit)) }
      }

    } catch {
      // Not seems to be a definition, evaluate it as an expression
      case _: SyntaxError => {
        try {
          val listener = ErrorListener(source)
          val parser = parseSource(source, listener)
          val expr = repl.visitor.visitStatement(parser.statement())
          given Resolve.Context = repl.resolveContext
          given Environment.Typed[Value] = repl.environment
          expr match {
            case Statement.Let(name, expectedTypeExpr, valueExpr) => {
              val (term, ty) = valueExpr.emit.resolve._1.synth.unpack
              expectedTypeExpr match {
                case Some(expectedTypeExpr) => {
                  val expectedType = expectedTypeExpr.emit.resolve._1.synth.term.normalize
                  if (ty != expectedType) {
                    throw CoreErrorKind.TypeNotMatch.raise {
                      s"Expected type ${expectedType}, but got ${ty.readBack(repl.environment)}"
                    }
                  }
                }
                case None => ()
              }
              val variable: Var.Local = Var.Local(name)
              repl.environment = repl.environment.add(Var.Local(name) , term.eval, ty)
              repl.resolveContext += variable
              println(s"  $name = ${term.normalize(repl.environment)} : ${ty.readBack(repl.environment)}\n")
            }
            case Statement.Expression(expr) => {
              println(s"  ${repl.evaluate(expr.emit)}\n")
            }
          }
        } catch {
          case error: Error => printError(source, error)
          case error: Throwable => throw error// println(s"Error: ${error.getMessage}")
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
