package saki.core

enum Var {

  case Defined[Def <: Definition](
    override val name: String,
    signature: Signature,
    definition: Option[Def] = None
  )

  case Local(override val name: String)

  def name: String = this match {
    case Defined(name, _, _) => name
    case Local(name) => name
  }
}

extension (self: Var.Defined[?]) {
  def call: Term = self.definition match {
    case Some(_: Definition.Function) =>
      Term.FunctionCall(self.asInstanceOf[Var.Defined[Definition.Function]],  self.signature.arguments.refs)
    case Some(_: Definition.Inductive) =>
      Term.InductiveCall(self.asInstanceOf[Var.Defined[Definition.Inductive]], self.signature.arguments.refs)
    case Some(_: Definition.Constructor) => {
      val cons = self.asInstanceOf[Var.Defined[Definition.Constructor]]
      Term.ConstructorCall(
        cons = cons,
        consArgs = self.signature.arguments.refs,
        inductiveArgs = cons.owner.signature.arguments.refs
      )
    }
    case Some(_: Definition.Contract) => ???
    case None => throw new Exception(s"Unresolved reference: ${self.name}")
  }
}

extension (variable: Var.Defined[Definition.Constructor]) {
  def owner: Var.Defined[Definition.Inductive] = variable.definition.get.owner
}

enum Term {

  case Primitive(value: Literal)
  case PrimitiveType(`type`: LiteralType)
  case Ref(variable: Var.Local)
  case FunctionCall(fn: Var.Defined[Definition.Function], args: Seq[Term])
  case InductiveCall(inductive: Var.Defined[Definition.Inductive], args: Seq[Term])
  case ConstructorCall(cons: Var.Defined[Definition.Constructor], consArgs: Seq[Term], inductiveArgs: Seq[Term])
  case Pi(param: Param[Term], codomain: Term)
  case Sigma(param: Param[Term], codomain: Term)
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

  override def toString: String = this match {
    case Primitive(value) => value.toString
    case PrimitiveType(ty) => ty.toString
    case Ref(variable) => variable.name
    case FunctionCall(fn, args) => s"${fn.name}(${args.mkString(", ")})"
    case InductiveCall(inductive, args) => s"${inductive.name}(${args.mkString(", ")})"
    case ConstructorCall(cons, args, inductiveArgs) =>
      s"${cons.owner}::(${inductiveArgs.mkString(", ")})${cons.name}(${args.mkString(", ")})"
    case Pi(param, codomain) => s"Π(${param.name} : ${param.`type`}) -> $codomain"
    case Sigma(param, codomain) => s"Σ(${param.name} : ${param.`type`}) -> $codomain"
    case Record(fields) => s"{${fields.map { case (k, v) => s"$k = $v" }.mkString(", ")}}"
    case RecordType(fields) => s"record {${fields.map { case (k, v) => s"$k: $v" }.mkString(", ")}}"
    case ApplyOnce(fn, arg) => s"$fn($arg)"
    case Lambda(param, body) => s"λ(${param.name}) => $body"
    case Projection(record, field) => s"$record.$field"
    case Universe(level) => s"Type^$level"
  }
}

type Type = Term

trait Typed {
  def `type`: Type
}

extension (params: Seq[Param[Term]]) {
  def buildPiType(body: Term): Term = params.foldRight(body) {
    case (param, body) => Term.Pi(param, body)
  }

  def buildLambda(body: Term): Term = params.map(_.ident).foldRight(body) {
    case (param, body) => Term.Lambda(param, body)
  }
}

extension (self: Term.Pi) {
  def codomainApplied(term: Term): Term = self.codomain.subst(self.param.ident, term)
}

object Term {
  def unit: Term = Primitive(Literal.UnitValue)
  def unitType: Term = PrimitiveType(LiteralType.UnitType)
}
