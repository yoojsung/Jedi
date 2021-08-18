package value

trait Numeric extends Addable {
  def -(other: Numeric): Numeric
  def *(other: Numeric): Numeric
  def /(other: Numeric): Numeric
  def unary_- : Numeric
}
