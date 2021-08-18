package expression

import context.{Environment, UndefinedException}
import value.{Thunk, Value}

case class Identifier(val name: String) extends Expression {
  override def toString = this.name

  def execute(env: Environment): Value = env(this) match {
    case t: Thunk => t()
    case v: Value => v
    case _ => throw new UndefinedException(this)
  }

  def hashcode: Int = this.name.hashCode()
}
