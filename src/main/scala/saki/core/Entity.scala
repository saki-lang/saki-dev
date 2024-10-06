package saki.core

import saki.core.context.{CurrentDefinitionContext, Environment, TypedEnvironment, TypedLocalMutableContext}
import saki.core.domain.Value
import saki.core.syntax.{Constructor, Function, Inductive, Var}

import scala.collection.Seq

trait Entity

trait RuntimeEntity[IT <: Entity] extends Entity {
  def infer(
    implicit env: Environment.Typed[Value]
  ): IT
}

trait EntityFactory[T <: Entity] {

  def unit: T

  def unitType: T

  def universe: T

  def variable(ident: Var.Local): T

  def inductiveType(inductive: Var.Defined[?, Inductive], args: Seq[T]): T

  def functionInvoke(function: Var.Defined[?, Function], args: Seq[T]): T

  def inductiveVariant(cons: Var.Defined[?, Constructor], consArgs: Seq[T], inductiveArgs: Seq[T]): T
  
}

