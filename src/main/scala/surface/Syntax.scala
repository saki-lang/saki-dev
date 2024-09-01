package surface

import scala.annotation.targetName

sealed trait Expr {
  def asType: Type = {
    this.asInstanceOf[Type]
  }
}

case class Var(name: String) extends Expr

case class Let(name: String, value: Expr, ty: Option[Expr], body: Expr) extends Expr

sealed trait Type extends Expr {
  def universe: Int

  @targetName("lessThanEq")
  infix def <=(other: Type): Boolean
}

case class AtomType(override val universe: Int) extends Type {
  @targetName("lessThanEq")
  override infix def <=(other: Type): Boolean = other match {
    case SumType(variants) => variants.forall(this <= _.asType)
    case _ => false
  }
}

case class FuncType(paramName: String, paramType: Expr, returnType: Expr) extends Type {
  override def universe: Int = {
    val argUniv = paramType.asType.universe
    val returnUniv = returnType.asType.universe
    Math.max(argUniv, returnUniv)
  }

  @targetName("lessThanEq")
  override infix def <=(other: Type): Boolean = other match {
    case FuncType(otherParamName, otherParamType, otherReturnType) =>
      otherParamType.asType <= paramType.asType && returnType.asType <= otherReturnType.asType
    case SumType(variants) => variants.forall(this <= _.asType)
    case _ => false
  }
}

case class RecordType(fields: Map[String, Expr]) extends Type {
  override def universe: Int = fields.values.map(_.asType.universe).max

  @targetName("lessThanEq")
  override infix def <=(other: Type): Boolean = other match {
    case RecordType(otherFields) =>
      fields.forall { case (name, fieldType) =>
        otherFields.get(name) match {
          case Some(otherFieldType) => fieldType.asType <= otherFieldType.asType
          case None => false
        }
      }
    case SumType(variants) => variants.forall(this <= _.asType)
    case _ => false
  }
}

case class SumType(variants: Set[Expr]) extends Type {
  override def universe: Int = variants.map(_.asType.universe).max

  @targetName("lessThanEq")
  override infix def <=(other: Type): Boolean = other match {
    case SumType(otherVariants) => variants.forall { variant =>
      otherVariants.exists(variant.asType <= _.asType) // TODO: double check this
    }
    case ty => variants.forall(_.asType <= ty)
  }
}
