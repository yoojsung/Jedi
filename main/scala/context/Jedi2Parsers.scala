package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {

  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def parameters: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case None => Nil
    case Some(e ~ Nil) => List(e)
    case Some(e ~ exps) => e :: exps
    case _ => Nil
  }

  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Expression] = "lambda" ~ parameters ~ expression ^^ {
    case "lambda" ~ params ~ exp => Lambda(params, exp)
  }

  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Expression] = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
    case "{" ~ e ~ exps ~ "}" => Block(e :: exps)
    case "{" ~ exp ~ Nil ~ "}" => Block(List(exp))
  }

  // freeze ::= "freeze" ~ "(" ~ expression ~ ")" // makes a MakeThunk
  def freeze: Parser[MakeThunk] = "freeze(" ~ expression ~ ")" ^^ {
    case "freeze(" ~ e ~ ")" => MakeThunk(e)
  }

  override def term: Parser[Expression]  = block | freeze | lambda | funCall | literal | "("~>expression<~")"
}
