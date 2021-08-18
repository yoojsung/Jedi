package expression
import context.Environment
import value.Value

case class Lambda(private val parameters: List[Identifier], private val body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    Closure(parameters, body, env)
  }
}
