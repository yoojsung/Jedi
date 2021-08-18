package value

import context.Environment
import expression.{Closure, Expression}

class Thunk(private val body: Expression, private val defEnv: Environment) extends Closure(Nil, body, defEnv) {
  private var cache : Value = _

  def apply(): Value = {
    if (cache == null) {
      cache = super.apply(Nil)
      cache
    }
    else cache
  }
}

object Thunk {
  def apply(body: Expression, env: Environment): Thunk = new Thunk(body, env)
}

