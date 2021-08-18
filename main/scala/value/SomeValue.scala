package value

case class SomeValue(val content: Value) extends OptionValue {
  override def toString: String = "Some(" + content + ")"
}
