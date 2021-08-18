package context

import value._
import expression._

object alu {

  def execute(opcode: Identifier, args: List[Value]): Value = opcode.name match {
    case "add" => add(args)            // n-ary
    case "mul" => mul(args)            // n-ary
    case "sub" => sub(args)            // n-ary
    case "div" => div(args)            // n-ary
    case "less" => less(args)          // binary
    case "equals" => same(args)        // binary
    case "more" => more(args)          // binary
    case "unequals" => unequals(args)  // binary
    case "not" => not(args)            // unary
    case "write" => write(args)
    case "cons" => cons(args)
    case "car" => car(args)
    case "cdr" => cdr(args)
    case "list" => list(args)
    case "nil" => nil(args)
    // variables
    case "dereference" => dereference(args)
    case "var" => makeVar(args)
    case "Some" => some(args)
    case "content" => content(args)
    // primitive I/O ops:
    //case "prompt" => prompt(args)
    //case "read" => read(args)
    // store ops
    /*
    case "store" => store(args)
    case "put" => put(args)
    case "rem" => rem(args)
    case "contains" => contains(args)
    case "map" => map(args)
    case "filter" => filter(args)
    case "get" => get(args)
    case "addLast" => addLast(args)
    case "size" => size(args)
    */
    case _ => throw new UndefinedException(opcode)
  }

  private def add(args: List[Value]): Value = {
    def helper(result: Addable, unseen: List[Value]): Addable =
      if (unseen == Nil) result
      else helper(result + unseen.head, unseen.tail)

    if (args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match {
      case n: Addable => helper(args(0).asInstanceOf[Addable], args.tail)
      case _ => throw new TypeException("Inputs to + must be addable")
    }
  }

  private def mul(args: List[Value]): Value = {
    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if (unseen == Nil) result
      else helper(result * unseen.head.asInstanceOf[Numeric], unseen.tail)

    if (args.size < 2) throw new TypeException("2 or more inputs required by *")
    args(0) match {
      case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail)
      case _ => throw new TypeException("Inputs to * must be Numeric")
    }
  }

  private def sub(args: List[Value]): Value = {
    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if (unseen == Nil) result
      else helper(result - unseen.head.asInstanceOf[Numeric], unseen.tail)

    if (args.size < 2) throw new TypeException("2 or more inputs required by -")
    args(0) match {
      case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail)
      case _ => throw new TypeException("Inputs to - must be addable")
    }
  }

  private def div(args: List[Value]): Value = {
    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if (unseen == Nil) result
      else helper(result / unseen.head.asInstanceOf[Numeric], unseen.tail)

    if (args.size < 2) throw new TypeException("2 or more inputs required by -")
    args(0) match {
      case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail)
      case _ => throw new TypeException("Inputs to / must be addable")
    }
  }

  private def less(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required by <")
    if (!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to < must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] < args(1))
  }

  private def same(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required by ==")
    Boole(args(0) == args(1))
  }

  private def more(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required by >")
    if (!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to > must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] > args(1))
  }

  private def unequals(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required by !=")
    Boole(args(0) != args(1))
  }

  private def not(args: List[Value]): Value = {
    if (!args(0).isInstanceOf[Boole]) throw new TypeException("Inputs to ! must be Boole")
    !args(0).asInstanceOf[Boole]
  }
  // etc.

  private def write(args: List[Value]): Value = {println(args(0)); Notification.DONE}

  private def cons(args: List[Value]): Value = {
    if (args.size == 2) Pair(args(0), args(1))
    else throw new TypeException("Two inputs needed for constructing Pair")
  }
  private def car(args: List[Value]): Value = {
    if (args(0).isInstanceOf[Pair]) args(0).asInstanceOf[Pair].first
    else throw new TypeException("Two inputs needed for car")
  }
  private def cdr(args: List[Value]): Value = {
    if (args(0).isInstanceOf[Pair]) args(0).asInstanceOf[Pair].second
    else throw new TypeException("Two inputs needed for cdr")
  }
  private def list(args: List[Value]): Value = {
    if (args == Nil) nil(args)
    else Pair(args.head, list(args.tail))
  }
  private def nil(args: List[Value]): Value = {empty.asInstanceOf[Value]}

  private def dereference(args: List[Value]): Value = {
    if (args(0).isInstanceOf[Variable]) args(0).asInstanceOf[Variable].content
    else throw new TypeException("Error with dereference")
  }
  private def makeVar(args: List[Value]): Value = {
    if (args.size == 1) Variable(args(0))
    else throw new TypeException("Error with making Variable")
  }
  private def some(args: List[Value]): Value = {
    if (args.size == 1) SomeValue(args(0))
    else throw new TypeException("Error making some value")
  }
  private def content(args: List[Value]): Value = {
    if (args.size == 1 && args(0).isInstanceOf[SomeValue]) args(0).asInstanceOf[SomeValue].content
    else throw new TypeException("Error while getting content")
  }
}