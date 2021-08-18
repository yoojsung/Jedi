package value

case class Pair(val first: Value, val second: Value) extends Value {
  override def toString: String = "(" + first + ", " + second + ")"
}
