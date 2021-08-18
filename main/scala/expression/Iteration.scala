package expression
import context.Environment
import value.{Boole, Notification, Value}

case class Iteration(private val condition: Expression, private val body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    while (condition.execute(env) == Boole.TRUE) body.execute(env)
    Notification.DONE
  }
}
