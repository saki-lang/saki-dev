package saki.core.syntax

import saki.core.SourceSpan
import saki.core.typing.{Elaborate, Resolve}

import scala.collection.Seq

enum Expr(val span: SourceSpan) {

  case Unresolved(
    name: String
  )(implicit span: SourceSpan) extends Expr(span)

  case Resolved(
    ref: Var
  )(implicit span: SourceSpan) extends Expr(span)

  /**
   * Universe level.
   * `Type` | `Type^1` | `Type^2` | ...
   */
  case Universe()(implicit span: SourceSpan) extends Expr(span)

  case Primitive(
    value: Literal
  )(implicit span: SourceSpan) extends Expr(span)

  case PrimitiveType(
    `type`: LiteralType
  )(implicit span: SourceSpan) extends Expr(span)

  /**
   * Application of a function to an argument.
   * `fn arg` | `fn(arg)`
   */
  case Apply(
    fn: Expr,
    arg: Argument[Expr]
  )(implicit span: SourceSpan) extends Expr(span)

  /**
   * Projection of a field from a record.
   * Or call to a "method" on an object. (syntax sugar)
   * `obj.field`
   */
  case Elimination(
    obj: Expr,
    member: String
  )(implicit span: SourceSpan) extends Expr(span)

  case Match(
    scrutinee: Expr,
    clauses: Seq[Clause[Expr]]
  )(implicit span: SourceSpan) extends Expr(span)

  case Hole(
    accessible: Seq[Var.Local]
  )(implicit span: SourceSpan) extends Expr(span)

  /**
   * Dependent Pi type.
   * `Π (x : A) -> B` | `∀ (x : A) -> B`
   */
  case Pi(
    param: Param[Expr],
    result: Expr
  )(implicit span: SourceSpan) extends Expr(span)

  /**
   * Dependent Sigma type.
   * `Σ (x : A) -> B` | `∃ (x : A) -> B`
   */
  case Sigma(
    param: Param[Expr],
    result: Expr
  )(implicit span: SourceSpan) extends Expr(span)

  /**
   * Lambda abstraction.
   * `λ (x : A) => B` | `|x: A| => B`
   */
  case Lambda(
    param: Param[Option[Expr]],
    body: Expr,
    returnType: Option[Expr] = None
  )(implicit span: SourceSpan) extends Expr(span)

  /**
   * Record instance.
   * `^{ field1 = expr1, field2 = expr2, ... }`
   */
  case Record( // TODO: record type
    fields: Map[String, Expr]
  )(implicit span: SourceSpan) extends Expr(span)

  /**
   * Record type.
   * `record { field1: Type1, field2: Type2, ... }`
   */
  case RecordType(
    fields: Map[String, Expr]
  )(implicit span: SourceSpan) extends Expr(span)

  def synth(implicit ctx: Elaborate.Context): Elaborate.Synth = Elaborate.synth(this)

  def elaborate(expected: Type)(implicit ctx: Elaborate.Context): Term = Elaborate.elaborate(this, expected)

  def resolve(implicit ctx: Resolve.Context): Expr = Resolve.resolveExpr(this)

  override def toString: String = {
    this match {
      case Universe() => "Type"
      case Primitive(value) => value.toString
      case PrimitiveType(ty) => ty.toString
      case Resolved(ref) => ref.toString
      case Unresolved(name) => name
      case Hole(_) => "_"
      case Pi(param, result) => s"Π(${param.ident} : ${param.`type`}) -> $result"
      case Sigma(param, result) => s"Σ(${param.ident} : ${param.`type`}) -> $result"
      case Apply(fn, arg) => s"$fn $arg"
      case Elimination(obj, member) => s"$obj.$member"
      case Lambda(param, body, _) => s"λ(${param.ident} : ${param.`type`}) => $body"
      case Record(fields) => s"^{ ${fields.map { case (k, v) => s"$k = $v" }.mkString(", ")} }"
      case RecordType(fields) => s"record { ${fields.map { case (k, v) => s"$k: $v" }.mkString(", ")} }"
      case Match(scrutinee, clauses) => s"match $scrutinee { ${clauses.map(_.toString).mkString(" | ")} }"
    }
  }
}

object Expr {
  def unit(implicit span: SourceSpan): Expr = Primitive(Literal.UnitValue)(span)
}
