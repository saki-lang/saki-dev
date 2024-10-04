package saki.core.syntax

import saki.core.domain.{Environment, NeutralValue, Type, Value}
import saki.core.elaborate.Normalize.{Context, RenameMap}
import saki.core.elaborate.{Normalize, Unify}
import saki.core.{Entity, EntityFactory, RuntimeEntity, TypeError}
import saki.util.LateInit

import scala.collection.Seq

enum Var {

  case Defined[T <: Entity, Def[A <: Entity] <: Definition[A]](
    override val name: String,
    val definition: LateInit[Def[T]] = LateInit[Def[T]](),
  )

  case Local(override val name: String)

  def name: String = this match {
    case Defined(name, _) => name
    case Local(name) => name
  }

  override def toString: String = this.name
}

extension [T <: Entity, Def[E <: Entity] <: Definition[E]](self: Var.Defined[T, Def]) {

  def signature(implicit factory: EntityFactory[T]): Signature[T] = self.definition.get.signature

  def call(implicit factory: EntityFactory[T]): T = self.definition.toOption match {

    case Some(_: Function[T]) => {
      val signature: Signature[T] = self.definition.get.signature
      factory.functionInvoke(self.asInstanceOf[Var.Defined[T, Function]], signature.paramToVars)
    }

    case Some(_: Inductive[T]) => {
      val signature: Signature[T] = self.definition.get.signature
      factory.inductiveType(self.asInstanceOf[Var.Defined[T, Inductive]], signature.paramToVars)
    }

    case Some(_: Constructor[T]) => {
      val cons = self.asInstanceOf[Var.Defined[T, Constructor]]
      factory.inductiveVariant(
        cons = cons,
        consArgs = self.signature.paramToVars,
        inductiveArgs = cons.owner.signature.paramToVars
      )
    }

    case None => TypeError(s"Unresolved reference: ${self.name}").raise
  }
}

extension [T <: Entity](variable: Var.Defined[T, Constructor]) {
  def owner: Var.Defined[T, Inductive] = variable.definition.get.owner
}

enum Term extends RuntimeEntity[Type] {

  case Universe
  case Primitive(value: Literal)
  case PrimitiveType(`type`: LiteralType)
  case Variable(variable: Var.Local)
  case FunctionInvoke(fn: Var.Defined[Term, Function], args: Seq[Term])
  case InductiveType(inductive: Var.Defined[Term, Inductive], args: Seq[Term])
  case InductiveVariant(cons: Var.Defined[Term, Constructor], consArgs: Seq[Term], inductiveArgs: Seq[Term])
  case Match(scrutinees: Seq[Term], clauses: Seq[Clause[Term]])
  case Pi(param: Param[Term], codomain: Term)
  case Sigma(param: Param[Term], codomain: Term)
  case Record(fields: Map[String, Term])
  case RecordType(fields: Map[String, Term])
  case Apply(fn: Term, arg: Term)
  case Lambda(param: Var.Local, body: Term)
  case Projection(record: Term, field: String)

  def subst(variable: Var.Local, term: Term): Term = this.subst(Map(variable -> term))

  def subst(implicit ctx: Normalize.Context): Term = this.normalize

  def apply(args: Term*): Term = {
    args.foldLeft(this) {
      case (Lambda(param, body), arg) => body.subst(param, arg)
      case (fn, arg) => Apply(fn, arg)
    }
  }

  override def toString: String = this match {
    case Universe => s"#Universe"
    case Primitive(value) => value.toString
    case PrimitiveType(ty) => ty.toString
    case Variable(variable) => variable.name
    case FunctionInvoke(fn, args) => s"${fn.name}(${args.mkString(", ")})"
    case InductiveType(inductive, args) => s"${inductive.name}(${args.mkString(", ")})"
    case InductiveVariant(cons, args, inductiveArgs) => {
      if (inductiveArgs.isEmpty) {
        s"${cons.name}(${args.mkString(", ")})"
      } else {
        s"${cons.name}(${inductiveArgs.mkString(", ")})(${args.mkString(", ")})"
      }
    }
    case Match(scrutinee, clauses) => s"match $scrutinee {${clauses.mkString(" | ")}}"
    case Pi(param, codomain) => s"Π(${param.name} : ${param.`type`}) -> $codomain"
    case Sigma(param, codomain) => s"Σ(${param.name} : ${param.`type`}) -> $codomain"
    case Record(fields) => s"{${fields.map { case (k, v) => s"$k = $v" }.mkString(", ")}}"
    case RecordType(fields) => s"record {${fields.map { case (k, v) => s"$k: $v" }.mkString(", ")}}"
    case Apply(fn, arg) => s"$fn($arg)"
    case Lambda(param, body) => s"λ(${param.name}) => $body"
    case Projection(record, field) => s"$record.$field"
  }

  def unify(that: Term): Boolean = Unify.unify(this, that)

  /**
   * Unify by eta conversion
   * (λx. M) N ~> M[x := N]
   */
  def etaUnify(lambda: Term.Lambda): Boolean = Unify.unify(lambda.body, this.apply(Term.Variable(lambda.param)))

  def normalize(implicit ctx: Context): Term = Normalize.normalizeTerm(this, ctx)

  def rename(implicit map: RenameMap): Term = Normalize.renameTerm(this)
  
  override def infer(implicit env: Environment): Value = ???

  def eval(implicit env: Environment): Value = this match {

    case Universe => Value.Universe

    case Primitive(value) => Value.Primitive(value)

    case PrimitiveType(ty) => Value.PrimitiveType(ty)

    case Variable(variable) => env(variable).value

    case FunctionInvoke(fnRef, argTerms) => {
      // Use the evaluated function in the environment if possible to avoid re-evaluation
      val fn = env.lookup(fnRef).getOrElse {
        throw IllegalStateException(s"Unbound function: ${fnRef.name}")
      }.asInstanceOf[Function[Value | Term]]
      env.currentDefinition match {
        case Some(current) if current.name == fnRef.name => {
          // Recursive call, keep it a neutral value
          Value.functionInvokeGeneral(fn.ident, argTerms.map(_.eval))
        }
        case None => {
          val body = fn.body.get match {
            case body: Value => body.readBack
            case body: Term => body
          }
          val argsValue: Seq[Value] = argTerms.map(_.eval)
          val argVarList: Seq[(Var.Local, Environment.TypedValue)] = fn.arguments(argsValue).map {
            (param, arg) => (param, Environment.TypedValue(arg, arg.infer))
          }
          env.withVars(argVarList.toMap) { body.eval }
        }
      }
    }

    case InductiveType(indRef, argTerms) => {
      val argsValue: Seq[Value] = argTerms.map(_.eval)
      Value.inductiveTypeGeneral(indRef, argsValue)
    }

    case InductiveVariant(consRef, consArgs, inductiveArgs) => {
      val consArgsValue: Seq[Value] = consArgs.map(_.eval)
      val inductiveArgsValue: Seq[Value] = inductiveArgs.map(_.eval)
      Value.inductiveVariantGeneral(consRef, consArgsValue, inductiveArgsValue)
    }
    
    case Match(scrutinees, clauses) => ???
  }
}

extension (params: Seq[Param[Term]]) {
  def buildPiType(body: Term): Term = params.foldRight(body) {
    case (param, body) => Term.Pi(param, body)
  }

  def buildLambda(body: Term): Term = params.map(_.ident).foldRight(body) {
    case (param, body) => Term.Lambda(param, body)
  }
}

given EntityFactory[Term] = Term

object Term extends EntityFactory[Term] {

  override def unit: Term = Primitive(Literal.UnitValue)

  override def unitType: Term = PrimitiveType(LiteralType.UnitType)

  override def universe: Term = Universe

  override def variable(ident: Var.Local): Term = Variable(ident)

  override def inductiveType(
    inductive: Var.Defined[Term, Inductive], args: Seq[Term]
  ): Term = InductiveType(inductive, args)

  override def functionInvoke(
    function: Var.Defined[Term, Function], args: Seq[Term]
  ): Term = FunctionInvoke(function, args)

  override def inductiveVariant(
    cons: Var.Defined[Term, Constructor], consArgs: Seq[Term], inductiveArgs: Seq[Term]
  ): Term = InductiveVariant(cons, consArgs, inductiveArgs)
}
