package saki.core

import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.syntax.{Constructor, Function, Inductive, Var}
import saki.core.term
import saki.core.term.Term

import scala.collection.Seq

trait Entity

trait RuntimeEntity[IT <: Entity] extends Entity {
  def infer(implicit env: Environment.Typed[Value]): IT
  def eval(implicit env: Environment.Typed[Value]): Value
}

trait EntityFactory[T <: Entity, D <: Entity] {

  def unit: T

  def unitType: T

  def universe: T

  def variable(ident: Var.Local, ty: T): T

  def typeBarrier(value: T, ty: T): T

  def inductiveType(inductive: Var.Defined[D, Inductive], args: Seq[T]): T

  def functionInvoke(function: Var.Defined[D, Function], args: Seq[T]): T

  def inductiveVariant(inductive: T, constructor: Constructor[D], args: Seq[T]): T
  
}

trait RuntimeEntityFactory[T <: Entity] extends EntityFactory[T, Term]

