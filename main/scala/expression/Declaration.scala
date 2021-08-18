package expression
import context.Environment
import value.{Notification, Value}

case class Declaration(private val identifier: Identifier, private val expression: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    env.put(identifier, expression.execute(env))
    Notification.OK
  }
}
