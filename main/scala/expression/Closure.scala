package expression

import context.{Environment, JediException}
import value.Value

case class Closure(private val parameters: List[Identifier], private val body: Expression, private val env: Environment) extends Value {
  def apply(args: List[Value]): Value = {
    val tempEnv = new Environment(env)
    tempEnv.bulkPut(parameters, args)
    if (args.length == parameters.length) body.execute(tempEnv)
    else throw new JediException()
  }
}
