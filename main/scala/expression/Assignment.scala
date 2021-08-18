package expression
import context.{Environment, TypeException}
import value.{Notification, Value, Variable}

case class Assignment(private val vbl: Identifier, private val update: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    if(!vbl.execute(env).isInstanceOf[Variable]) throw new TypeException()
    else
      vbl.execute(env).asInstanceOf[Variable].content = update.execute(env)
    Notification.DONE
  }
}
