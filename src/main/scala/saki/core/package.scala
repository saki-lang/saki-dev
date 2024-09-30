package saki

package object core {

  type Literal = syntax.Literal

  type LiteralType = syntax.LiteralType

  type Expr = syntax.Expr

  type Term = syntax.Term

  type Type = syntax.Type

  type Param[T] = syntax.Param[T]
  val Param: syntax.Param.type = syntax.Param

  type Pattern = syntax.Pattern

  type Clause[T] = syntax.Clause[T]

  type Definition = syntax.Definition

  type PristineDefinition = syntax.PristineDefinition
  val PristineDefinition: syntax.PristineDefinition.type = syntax.PristineDefinition

  type LocalVar = syntax.Var.Local

}
