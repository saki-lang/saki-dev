package saki.core

import saki.core.domain.Environment
import saki.core.syntax.{Constructor, Function, Inductive, Var}

import scala.collection.Seq

trait Entity

trait RuntimeEntity[IT <: Entity] extends Entity {
  def infer(implicit env: Environment): IT
}

trait EntityFactory[T <: Entity] {

  def unit: T

  def unitType: T

  def universe: T

  def variable(ident: Var.Local): T

  def inductiveType(inductive: Var.Defined[T, Inductive], args: Seq[T]): T

  def functionInvoke(function: Var.Defined[T, Function], args: Seq[T]): T

  def inductiveVariant(cons: Var.Defined[T, Constructor], consArgs: Seq[T], inductiveArgs: Seq[T]): T
  
}

