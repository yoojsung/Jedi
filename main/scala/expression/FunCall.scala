package expression
import context._
import value.{Thunk, Value}

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression  {
  def execute(env: Environment): Value = {
    /*
    val args: List[Value] = operands.map(_.execute(env))
    val closure = operator.execute(env).asInstanceOf[Closure]
    closure.apply(args)
    */
    /*
    val args = operands.map(_.execute(env))
    alu.execute(operator, args)
     */
    /*
    val arguments : List[Value] = operands.map(_.execute(env))
    try {
      alu.execute(operator, arguments)
    } catch {
      case e: UndefinedException => throw e
      case e: Exception => throw e
    }
    */
    //df
    var args: List[Value] = Nil

    if (env.contains(operator)) {
      if (flags.paramPassing == flags.BY_NAME) {
        args = operands.map(o => new Thunk(o, env))
      } else {
        args = operands.map(_.execute(env))
      }
      /*
      val closure = operator.execute(env).asInstanceOf[Closure]
      closure.apply(args)
       */
      try {
        operator.execute(env) match {
          case c: Closure => c.apply(args)
          case _ => throw new JediException()
        }
      } catch {
        case e: JediException => alu.execute(operator.asInstanceOf[Identifier], args)
      }
      // etc.
    } else {
      args = operands.map(_.execute(env))
      alu.execute(operator, args)
    }

  }
}
