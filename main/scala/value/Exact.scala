package value

import context.{IllegalValueException, TypeException}
import expression.Literal

case class Exact(private val _value: Int) extends Numeric with Ordered[Value] {
  def value: Int = _value
  def +(other: Value): Addable =
    other match {
      case x: Exact => Exact(this.value + x.value)
      case x: Inexact => Inexact(this.value + x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def unary_- : Numeric = Exact(-this.value)

  override def compare(other: Value): Int =
    other match {
      case x: Exact => this.value.compare(x.value)
      case x: Inexact => this.value.compare(x.value.toInt)
      case _ => throw new TypeException("Arguments must be comparable")
    }

  override def equals(other: Any): Boolean =
    other match {
      case x: Exact => x.isInstanceOf[Exact] && x.value == this.value
      case x: Inexact => x.isInstanceOf[Inexact] && x.value == this.value
      case _ => false
    }

  override def toString: String = this.value.toString

  override def -(other: Numeric): Numeric =
    other match {
      case x: Exact => Exact(this.value - x.value)
      case x: Inexact => Inexact(this.value - x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  override def *(other: Numeric): Numeric =
    other match {
      case x: Exact => Exact(this.value * x.value)
      case x: Inexact => Inexact(this.value * x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  override def /(other: Numeric): Numeric =
    other match {
      case x: Exact => if (x.value == 0) throw new IllegalValueException("Divide by 0!") else Exact(this.value / x.value)
      case x: Inexact => if (x.value == 0) throw new IllegalValueException("Divide by 0!") else Inexact(this.value / x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  override def hashCode(): Int = this.value.hashCode()
}