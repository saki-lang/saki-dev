package saki.core

enum Var {
  case Defined[Def <: Definition](override val name: String, definition: Def, signature: Signature)
  case Local(override val name: String)

  def name: String = this match {
    case Defined(name, _, _) => name
    case Local(name) => name
  }
}

enum Term {

  case Primitive(value: Literal)
  case PrimitiveType(`type`: LiteralType)
  case Ref(variable: Var.Local)
  case FunctionCall(fn: Var.Defined[Definition.Function], args: Seq[Term])
  case InductiveCall(inductive: Var.Defined[Definition.Inductive], args: Seq[Term])
  case ConstructorCall(cons: Var.Defined[Definition.Constructor], args: Seq[Term], inductiveArgs: Seq[Term])
  case Pi(param: Var.Local, codomain: Term)
  case Sigma(param: Var.Local, codomain: Term)
  case Record(fields: Map[String, Term])
  case RecordType(fields: Map[String, Type])
  case ApplyOnce(fn: Term, arg: Term)
  case Lambda(param: Var.Local, body: Term)
  case Projection(record: Term, field: String)
  case Universe(level: Int)

  def subst(variable: Var.Local, term: Term): Term = this.subst(Map(variable -> term))

  def subst(implicit ctx: Normalize.Context): Term = this.normalize

  def apply(args: Term*): Term = {
    args.foldLeft(this) {
      case (Lambda(param, body), arg) => body.subst(param, arg)
      case (fn, arg) => ApplyOnce(fn, arg)
    }
  }

  def proj(field: String): Term = this match {
    case Record(fields) => fields(field)
    case _ => Projection(this, field)
  }
}

type Type = Term

extension (telescope: Seq[Var.Local]) {
  def mkLambda(body: Term): Term = telescope.foldRight(body) {
    case (param, body) => Term.Lambda(param, body)
  }

  def mkPi(body: Term): Term = telescope.foldRight(body) {
    case (param, body) => Term.Pi(param, body)
  }
}

extension (self: Term.Pi) {
  def codomainApplied(term: Term): Term = self.codomain.subst(self.param, term)
}

object Term {
  def unit: Term = Primitive(Literal.UnitValue)
  def unitType: Term = PrimitiveType(LiteralType.UnitType)
}
