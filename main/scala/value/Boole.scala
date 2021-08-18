package value

import expression.Literal

case class Boole(private val _value: Boolean) extends Literal {
  def value: Boolean = _value
  override def toString: String = this.value.toString

  def &&(other: Boole): Boole = {
    other match {
     case x: Boole => Boole(x.value && this.value)
     case _ => Boole(false)
    }
  }

  def ||(other: Boole): Boole = {
    other match {
      case x: Boole => Boole(x.value || this.value)
      case _ => Boole(false)
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case x: Boole => x.isInstanceOf[Boole] && x.value == this.value
      case _ => false
    }
  }

  def unary_! : Boole = Boole(!this.value)

  override def hashCode(): Int = this.value.hashCode()
}

object Boole {
  val TRUE: Boole = Boole(true)
  val FALSE: Boole = Boole(false)
}