package expression
import context.Environment
import value.Value

case class Block(val expressions: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    val defEnv = new Environment(env)
    for (i <- 0 until expressions.length - 1)
      expressions(i).execute(defEnv)
    expressions.last.execute(defEnv)
  }
}
