package saki.core

case class Param[Type](ident: Var.Local, `type`: Type)

type Telescope[T] = Seq[Param[T]]

trait FnLike[T] {
  def telescope: Telescope[T]
  def resultType: T
  def telescopeVars: Seq[Var.Local] = telescope.map(_.ident)
  def telescopeRefs: Seq[Term.Ref] = telescopeVars.map(Term.Ref.apply)
}

enum Signature(telescope: Telescope[Term], result: Term) {
  case Function(telescope: Telescope[Term], result: Term) extends Signature(telescope, result)
  case Inductive(telescope: Telescope[Term], result: Term) extends Signature(telescope, result)
  case Constructor(telescope: Telescope[Term], result: Term) extends Signature(telescope, result)
  case Contract(telescope: Telescope[Term], result: Term) extends Signature(telescope, result)
}

enum Definition(ident: Var.Defined[? <: Definition]) extends FnLike[Term] {
  case Function(
    ident: Var.Defined[Function], telescope: Telescope[Term],
    override val resultType: Term,
    body: Either[Term, Pattern.ClauseSet[Term]],
  ) extends Definition(ident)

  case Inductive(
    ident: Var.Defined[Inductive], telescope: Telescope[Term], constructors: Seq[Constructor],
  ) extends Definition(ident)

  case Constructor(
    ident: Var.Defined[Constructor], owner: Var.Defined[Inductive], telescope: Telescope[Term],
  ) extends Definition(ident)

  case Contract(
    ident: Var.Defined[Contract], telescope: Telescope[Term],
    override val resultType: Term,
  ) extends Definition(ident)

  def resultType: Term = this match {
    case Function(_, _, resultType, _) => resultType
    case Inductive(_, _, _) => Term.unitType
    case Constructor(_, owner, _) => Term.InductiveCall(owner, owner.definition.telescopeRefs)
    case Contract(_, _, resultType) => resultType
  }
}

object Definition {
  sealed trait ConstructorExtension {
    def owner: Var.Defined[Definition.Inductive]
    def result: Term = ???
  }

}