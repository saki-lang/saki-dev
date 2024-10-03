package saki.core

import saki.core.syntax.{Function, Constructor, Inductive, Var}

import scala.collection.Seq

trait Entity

trait EntityFactory[T <: Entity] {

  def unit: T

  def unitType: T

  def universe: T

  def variable(ident: Var.Local): T

  def inductiveCall(inductive: Var.Defined[T, Inductive], args: Seq[T]): T

  def functionCall(function: Var.Defined[T, Function], args: Seq[T]): T

  def constructorCall(cons: Var.Defined[T, Constructor], consArgs: Seq[T], inductiveArgs: Seq[T]): T
  
}

