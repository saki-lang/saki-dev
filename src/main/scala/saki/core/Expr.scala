package saki.core

import util.SourceSpan

enum Expr(span: SourceSpan) {

  case Unresolved(span: SourceSpan, name: String) extends Expr(span)

  case Resolved(span: SourceSpan, ref: Var) extends Expr(span)

  case Primitive(span: SourceSpan, value: Any) extends Expr(span)

  case PrimitiveType(span: SourceSpan, name: String) extends Expr(span)

  /**
   * Application of a function to an argument.
   * `fn arg` | `fn(arg)`
   */
  case Apply(span: SourceSpan, fn: Expr, arg: Expr) extends Expr(span)

  /**
   * Projection of a field from a record.
   * Or call to a "method" on an object. (syntax sugar)
   * `obj.field`
   */
  case Elimination(span: SourceSpan, obj: Expr, member: String) extends Expr(span)

  case Hole(span: SourceSpan, accessible: Seq[Var.Local]) extends Expr(span)

  /**
   * Dependent Pi type.
   * `Π (x : A) -> B` | `∀ (x : A) -> B`
   */
  case Pi(span: SourceSpan, param: Param[Expr], result: Expr) extends Expr(span)

  /**
   * Dependent Sigma type.
   * `Σ (x : A) -> B` | `∃ (x : A) -> B`
   */
  case Sigma(span: SourceSpan, param: Param[Expr], result: Expr) extends Expr(span)

  /**
   * Lambda abstraction.
   * `λ (x : A) => B` | `|x: A| => B`
   */
  case Lambda(span: SourceSpan, param: Param[Expr], body: Expr) extends Expr(span)

  /**
   * Record instance.
   * `^{ field1 = expr1, field2 = expr2, ... }`
   */
  case Record(span: SourceSpan, fields: Map[String, Expr]) extends Expr(span)

  /**
   * Record type.
   * `record { field1: Type1, field2: Type2, ... }`
   */
  case RecordType(span: SourceSpan, fields: Map[String, Expr]) extends Expr(span)

  /**
   * Universe level.
   * `Type` | `Type^1` | `Type^2` | ...
   */
  case Universe(span: SourceSpan, level: Int) extends Expr(span)
}
