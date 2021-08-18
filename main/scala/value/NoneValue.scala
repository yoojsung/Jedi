package value

import expression.Literal

case class NoneValue() extends OptionValue with Literal {
  override def toString: String = "None"
}
