package expression
import context.{Environment, TypeException}
import value.{Boole, Value}

case class Disjunction(val operands: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var exit = true
    var res = Boole.FALSE
    for (o <- operands if exit) {
      val temp = o.execute(env)
      if (temp.isInstanceOf[Boole] && temp.asInstanceOf[Boole] == Boole.TRUE) {
        res = Boole.TRUE
        exit = false
      }
      else if (!temp.isInstanceOf[Boole]) throw new  TypeException()
    }
    res
  }
}
