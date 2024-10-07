package saki.core.syntax

import saki.core.context.{CurrentDefinition, CurrentDefinitionContext, DefinitionContext, Environment, LocalContext, Typed, TypedEnvironment, TypedLocalContext, TypedLocalMutableContext}
import saki.core.domain.{NeutralValue, Type, Value}
import saki.core.elaborate.Unify
import saki.core.{Entity, EntityFactory, RuntimeEntity, RuntimeEntityFactory, TypeError}
import saki.util.LateInit

import scala.collection.Seq

enum Term extends RuntimeEntity[Type] {

  case Universe
  case Primitive(value: Literal)
  case PrimitiveType(`type`: LiteralType)
  case Variable(variable: Var.Local)
  case FunctionInvoke(fn: Var.Defined[Term, Function], args: Seq[Term])
  case InductiveType(inductive: Var.Defined[Term, Inductive], args: Seq[Term])
  case InductiveVariant(cons: Var.Defined[Term, Constructor], consArgs: Seq[Term], inductiveArgs: Seq[Term])
  case Match(scrutinees: Seq[Term], clauses: Seq[Clause[Term]])
  case Pi(param: Param[Term], codomain: Term) extends Term with PiLikeTerm
  case Sigma(param: Param[Term], codomain: Term) extends Term with PiLikeTerm
  case Record(fields: Map[String, Term])
  case RecordType(fields: Map[String, Term])
  case Apply(fn: Term, arg: Term)
  case Lambda(param: Param[Term], body: Term) extends Term with LambdaLikeTerm
  case Projection(record: Term, field: String)

  def subst(variable: Var.Local, term: Term): Term = ???

  def subst(
    implicit env: Environment.Typed[Value]
  ): Term = this.normalize

  def apply(args: Term*): Term = {
    args.foldLeft(this) {
      case (Lambda(param, body), arg) => body.subst(param.ident, arg)
      case (fn, arg) => Apply(fn, arg)
    }
  }

  override def toString: String = this match {
    case Universe => s"#Universe"
    case Primitive(value) => value.toString
    case PrimitiveType(ty) => ty.toString
    case Variable(variable) => variable.name
    case FunctionInvoke(fn, args) => s"${fn.name}(${args.mkString(", ")})"
    case InductiveType(inductive, args) => {
      val argsStr = if args.nonEmpty then s"(${args.mkString(", ")})" else ""
      s"${inductive.name}$argsStr"
    }
    case InductiveVariant(cons, args, inductiveArgs) => {
      val inductiveArgsStr = if inductiveArgs.nonEmpty then s"(${inductiveArgs.mkString(", ")})" else ""
      val argsStr = if args.nonEmpty then s"(${args.mkString(", ")})" else ""
      s"${cons.name}$inductiveArgsStr$argsStr"
    }
    case Match(scrutinees, clauses) => {
      val scrutineesStr = if scrutinees.size > 1 then {
        s"(${scrutinees.mkString(", ")})"
      } else scrutinees.head.toString
      s"match $scrutineesStr { ${clauses.mkString(" | ")} }"
    }
    case Pi(param, codomain) => s"Π(${param.name} : ${param.`type`}) -> $codomain"
    case Sigma(param, codomain) => s"Σ(${param.name} : ${param.`type`}) -> $codomain"
    case Record(fields) => s"{ ${fields.map { case (k, v) => s"$k = $v" }.mkString(", ")} }"
    case RecordType(fields) => s"record { ${fields.map { case (k, v) => s"$k: $v" }.mkString(", ")} }"
    case Apply(fn, arg) => s"$fn($arg)"
    case Lambda(param, body) => s"λ(${param.name}) => $body"
    case Projection(record, field) => s"$record.$field"
  }

  def unify(that: Term): Boolean = Unify.unify(this, that)

  /**
   * Unify by eta conversion
   * (λx. M) N ~> M[x := N]
   */
  def etaUnify(lambda: Term.Lambda): Boolean = {
    Unify.unify(lambda.body, this.apply(Term.Variable(lambda.param.ident)))
  }

  def normalize(
    implicit env: Environment.Typed[Value]
  ): Term = this.eval.readBack

  /**
   * Infer the type of the term
   * @param env The environment
   * @return The inferred type
   */
  override def infer(
    implicit env: Environment.Typed[Value]
  ): Value = this match {
    case Universe => Value.Universe
    case Primitive(value) => Value.PrimitiveType(value.ty)
    case PrimitiveType(_) => Value.Universe
    case Variable(variable) => env.getTyped(variable).get.`type`
    case FunctionInvoke(fnRef, _) => {
      env.definitions(fnRef).asInstanceOf[Function[Term]].resultType.eval
    }
    case InductiveType(indRef, _) => {
      env.definitions(indRef).asInstanceOf[Inductive[Term]].resultType.eval
    }
    case InductiveVariant(consRef, _, _) => {
      env.definitions(consRef).asInstanceOf[Constructor[Term]].resultType.eval
    }
    case Match(_, clauses) => {
      val clausesType: Seq[Value] = clauses.map(_.body.infer)
      if clausesType.tail.forall(_ unify clausesType.head) then clausesType.head
      else throw TypeError("Clauses have different types", None)
      clausesType.head
    }
    case Pi(_, _) => Value.Universe
    case Sigma(_, _) => Value.Universe
    case Record(fields) => Value.RecordType(fields.map {
      (name, term) => (name, term.infer)
    })
    case RecordType(_) => Value.Universe
    case Record(fields) => Value.Record(fields.map {
      (name, value) => (name, value.infer)
    })
    case Apply(fn, arg) => fn.infer match {
      case Value.Pi(paramType, codomain) => {
        val argType = arg.infer
        if !(argType unify paramType) then {
          throw TypeError(s"Type mismatch: $paramType != $argType", None)
        }
        // To obtain the concrete return type, feed the concrete argument to the codomain closure
        codomain(arg.eval)
      }
      case _ => throw TypeError("Cannot apply non-function", None)
    }
    
  }

  def eval(implicit env: Environment.Typed[Value]): Value = this match {

    case Universe => Value.Universe

    case Primitive(value) => Value.Primitive(value)

    case PrimitiveType(ty) => Value.PrimitiveType(ty)

    case Variable(variable) => env.getValue(variable).get

    case FunctionInvoke(fnRef, argTerms) => {
      val fn = env.definitions(fnRef).asInstanceOf[Function[Term]]
      env.currentDefinition match {
        case Some(current) if current.name == fnRef.name => {
          // Recursive call, keep it a neutral value
          Value.functionInvoke(fn.ident, argTerms.map(_.eval))
        }
        case None | Some(_) => {
          val argsValue: Seq[Value] = argTerms.map(_.eval)
          // TODO: this need to be optimized
          if !fn.isNeutral || argsValue.forall(_.readBack.isFinal(Set.empty)) then {
            val argVarList: Seq[(Var.Local, Typed[Value])] = fn.arguments(argsValue).map {
              (param, arg) => (param, Typed[Value](arg, arg.infer))
            }
            val body: Term = fn.body.get
            env.withLocals(argVarList.toMap) { implicit env => body.eval }
          } else {
            Value.functionInvoke(fn.ident, argsValue)
          }
        }
      }
    }

    case InductiveType(indRef, argTerms) => {
      val argsValue: Seq[Value] = argTerms.map(_.eval)
      Value.inductiveType(indRef, argsValue)
    }

    case InductiveVariant(consRef, consArgs, inductiveArgs) => {
      val consArgsValue: Seq[Value] = consArgs.map(_.eval)
      val inductiveArgsValue: Seq[Value] = inductiveArgs.map(_.eval)
      Value.inductiveVariant(consRef, consArgsValue, inductiveArgsValue)
    }
    
    case Match(scrutinees, clauses) => {
      val scrutineesNorm = scrutinees.map(_.eval)
      clauses.map {
        clause => clause.map(_.normalize)
      }.tryMatch(scrutineesNorm).getOrElse {
        Value.Neutral(NeutralValue.Match(scrutineesNorm, clauses))
      }
    }

    case piType: Pi => piType.eval(Value.Pi.apply)

    case sigmaType: Sigma => sigmaType.eval(Value.Sigma.apply)

    case Record(fields) => {
      val fieldsValue: Map[String, Value] = fields.map {
        (name, term) => (name, term.eval)
      }
      Value.Record(fieldsValue)
    }

    case RecordType(fields) => {
      val fieldsType: Map[String, Type] = fields.map {
        (name, term) => (name, term.infer)
      }
      Value.RecordType(fieldsType)
    }

    case Apply(fn, arg) => fn.eval match {
      case Value.Lambda(_, bodyClosure) => bodyClosure(arg.eval)
      // If the evaluation of the function stuck, the whole application is stuck
      // Thus, no need for considering the situation that function is a global call
      // because it is tried to be evaluated before but failed
      case Value.Neutral(neutral) => Value.Neutral(NeutralValue.Apply(neutral, arg.eval))
      case _ => throw TypeError(s"Cannot apply non-function: $fn", None)
    }

    case lambda: Lambda => lambda.eval(Value.Lambda.apply)

    case Projection(record, field) => record.eval match {
      case Value.Record(fields) => fields.getOrElse(field, {
        throw TypeError(s"Missing field: $field", None)
      })
      case neutral: Value.Neutral => Value.Neutral(NeutralValue.Projection(neutral, field))
      case _ => throw TypeError(s"Cannot project from non-record: $record", None)
    }
  }

  def isFinal(implicit localVars: Set[Var.Local]): Boolean = this match {
    case Variable(variable) => localVars.contains(variable)
    case Universe | Primitive(_) | PrimitiveType(_) => true
    case FunctionInvoke(_, args) => args.forall(_.isFinal)
    case InductiveType(_, args) => args.forall(_.isFinal)
    case InductiveVariant(_, consArgs, inductiveArgs) => consArgs.forall(_.isFinal) && inductiveArgs.forall(_.isFinal)
    case Match(scrutinees, clauses) => scrutinees.forall(_.isFinal) && clauses.forall(_.forall(_.isFinal))
    case Pi(param, codomain) => param.`type`.isFinal && codomain.isFinal
    case Sigma(param, codomain) => param.`type`.isFinal && codomain.isFinal
    case Record(fields) => fields.values.forall(_.isFinal)
    case RecordType(fields) => fields.values.forall(_.isFinal)
    case Apply(fn, arg) => fn.isFinal && arg.isFinal
    case Lambda(param, body) => body.isFinal(localVars + param.ident)
    case Projection(record, _) => record.isFinal
  }
}

extension (params: ParamList[Term]) {
  def buildPiType(body: Term): Term = params.foldRight(body) {
    case (param, body) => Term.Pi(param, body)
  }

  def buildLambda(body: Term): Term = params.foldRight(body) {
    case (param, body) => Term.Lambda(param, body)
  }
}

given RuntimeEntityFactory[Term] = Term

object Term extends RuntimeEntityFactory[Term] {

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

private sealed trait LambdaLikeTerm {
  def param: Param[Term]
  def body: Term

  def eval(constructor: (Type, Value => Value) => Value)(
    implicit env: Environment.Typed[Value]
  ): Value = {
    val newParam = env.uniqueVariable
    def closure(arg: Value): Value = {
      val argVar = Typed[Value](arg, arg.infer)
      env.withLocal(newParam, argVar) { implicit env => body.eval }
    }
    constructor(param.`type`.eval, closure)
  }
}

private sealed trait PiLikeTerm extends LambdaLikeTerm {
  def codomain: Term
  override def body: Term = codomain
}
