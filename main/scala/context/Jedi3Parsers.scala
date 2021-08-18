package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {
  // assignment ::= identifier ~ ":=" ~ expression
  def assignment: Parser[Expression] = identifier ~ ":=" ~ expression ^^ {
    case i ~ ":=" ~ e => Assignment(i, e)
  }

  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Expression] = "while" ~ "(" ~> expression ~ ")" ~ expression ^^ {
    case cond ~ ")" ~ body => Iteration(cond, body)
  }

  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[Expression] = "[" ~> expression <~ "]" ^^ {
    case e => FunCall(Identifier("dereference"), List(e))
  }

  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | freeze | funCall | block |  assignment | dereference | literal | "("~>expression<~")"

}