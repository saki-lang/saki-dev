package saki.core

import saki.core.context.Environment
import saki.core.domain.Value
import saki.core.syntax.{Constructor, Function, Inductive, Overloaded, Var}

import scala.collection.Seq

trait Entity

trait RuntimeEntity[IT <: Entity] extends Entity {
  def infer(
    implicit env: Environment.Typed[Value]
  ): IT
}

trait EntityFactory[T <: Entity, D <: Entity] {

  def unit: T

  def unitType: T

  def universe: T

  def variable(ident: Var.Local): T

  def inductiveType(inductive: Var.Defined[D, Inductive], args: Seq[T]): T

  def functionInvoke(function: Var.Defined[D, Function], args: Seq[T]): T

  def inductiveVariant[Ind <: T](inductive: Ind, constructor: Var.Defined[D, Constructor], args: Seq[T]): T
  
}

trait RuntimeEntityFactory[T <: Entity] extends EntityFactory[T, Term]

