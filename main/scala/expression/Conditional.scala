package expression
import context.Environment
import value.{Boole, Value}

case class Conditional(val condition: Expression, val consequent: Expression, val alternative: Expression = null) extends SpecialForm {
  override def execute(env: Environment): Value = {
    condition.execute(env) match {
      case x: Boole if x == Boole.TRUE => consequent.execute(env)
      case x: Boole if x == Boole.FALSE => alternative.execute(env)
      case _ => throw new Exception()
    }
  }
}
