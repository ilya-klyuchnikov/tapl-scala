package tapl.arith

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.{ CharSequenceReader => Reader }

object ArithParsers extends StandardTokenParsers with ImplicitConversions {
  lexical.reserved += ("true", "false", "if", "then", "else", "iszero", "succ", "pred")
  lexical.delimiters += ("(", ")", ";")

  private def topLevel: Parser[List[Command]] =
    eof ^^ { _ => List() } |
      (command <~ ";") ~ topLevel ^^ { case c ~ cs => c :: cs }

  private def command: Parser[Command] =
    term ^^ Eval

  private def term: Parser[Term] = appTerm |
    ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ TmIf

  private def appTerm: Parser[Term] =
    aTerm |
      "succ" ~> aTerm ^^ TmSucc |
      "pred" ~> aTerm ^^ TmPred |
      "iszero" ~> aTerm ^^ TmIsZero

  //  Atomic terms are ones that never require extra parentheses
  private def aTerm: Parser[Term] =
    "(" ~> term <~ ")" |
      "true" ^^ { _ => TmTrue } |
      "false" ^^ { _ => TmFalse } |
      numericLit ^^ { x => num(x.toInt) }

  private def num(x: Int): Term = x match {
    case 0 => TmZero
    case _ => TmSucc(num(x - 1))
  }

  private def eof: Parser[String] = elem("<eof>", _ == lexical.EOF) ^^ { _.chars }

  def input(s: String) = phrase(topLevel)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t                 => error(t.toString)
  }
}