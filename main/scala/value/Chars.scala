package value
import context.{Environment, TypeException}

case class Chars(private val _value: String) extends Addable with Ordered[Value] {
  def value: String = _value
  override def toString: String = this.value
  override def +(other: Value): Addable = Chars(this.value + other.toString)

  def size: Exact = {
    Exact(value.size)
  }

  def subChars(to: Exact, from: Exact): Chars = {
    Chars(this.value.substring(to.value, from.value))
  }

  override def compare(other: Value): Int = {
    other match {
      case x: Chars =>
        if (x.toString < this.value) 1
        else if (this.value < x.toString) -1
        else 0
      case _ => throw new TypeException()
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case x: Chars => x.isInstanceOf[Chars] && x.value == this.value
      case _ => false
    }
  }

  override def hashCode(): Int = this.value.hashCode()
}
