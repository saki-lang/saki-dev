package saki.core.pattern

import saki.core.*
import util.SourceSpan

enum Pattern(val span: SourceSpan) {
  
  case Unresolved(
    name: String,
    patterns: Seq[Unresolved],
  )(implicit span: SourceSpan) extends Pattern(span)

  case Primitive(
    value: Literal,
  )(implicit span: SourceSpan) extends Pattern(span)

  case Bind(
    binding: Var.Local,
  )(implicit span: SourceSpan) extends Pattern(span)

  case Cons(
    cons: Var.Defined[Definition.Constructor],
    patterns: Seq[Pattern],
  )(implicit span: SourceSpan) extends Pattern(span)

  given SourceSpan = span

  override def toString: String = {
    this match {
      case Unresolved(name, patterns) => s"$name ${patterns.mkString(" ")}"
      case Primitive(value) => value.toString
      case Bind(binding) => binding.name
      case Cons(cons, patterns) => s"${cons.name} ${patterns.mkString(" ")}"
    }
  }

  def buildSubstMap(term: Term): Option[Map[Var.Local, Term]] = {
    PatternMatching.buildSubstMap(this, term)
  }

  def resolve(implicit ctx: ResolvingContext): (Pattern, ResolvingContext) = this match {
    case Unresolved(name, patterns) => {
      ctx.get(name) match {
        
        // If the variable is already defined, then it should be a constructor.
        // TODO: literals and records
        case Some(variable: Var.Defined[?]) if patterns.nonEmpty => {
          val (resolvedPatterns, updatedContext) = patterns.foldLeft((List.empty[Pattern], ctx)) {
            case ((resolvedPatterns, context), pattern) => {
              val (resolved, newCtx) = pattern.resolve(context)
              (resolvedPatterns :+ resolved, newCtx)
            }
          }
          val pattern = Pattern.Cons(
            cons = variable.asInstanceOf[Var.Defined[Definition.Constructor]],
            patterns = resolvedPatterns
          )
          (pattern, updatedContext)
        }
        
        // Otherwise, it should be a new introduced variable.
        case _ => {
          val variable: Var.Local = Var.Local(name)
          (Pattern.Bind(variable), ctx.updated(name, variable))
        }
      }
    }
    
    // It is already resolved, just return it.
    case _ => (this, ctx)
  }
}
