package expression
import context.{Environment, TypeException}
import value.{Boole, Value}

case class Conjunction(val operands: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var exit = true
    var res = Boole.TRUE
    for (o <- operands if exit) {
      val temp = o.execute(env)
      if (temp.isInstanceOf[Boole] && temp.asInstanceOf[Boole] == Boole.FALSE) {
        res = Boole.FALSE
        exit = false
      }
      else if (!temp.isInstanceOf[Boole]) throw new  TypeException()
    }
    res
  }
}
