package tapl.arith

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object ArithParsers extends StandardTokenParsers with ImplicitConversions {
  import Command._
  import Term._

  lexical.reserved ++= Seq("true", "false", "if", "then", "else", "iszero", "succ", "pred")
  lexical.delimiters ++= Seq("(", ")", ";")

  private def topLevel: Parser[List[Command]] =
    (command <~ ";") ~ topLevel ^^ { case c ~ cs => c :: cs } |
      success(List())

  private def command: Parser[Command] =
    term ^^ Eval.apply

  private def term: Parser[Term] =
    appTerm |
      ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ TmIf.apply

  private def appTerm: Parser[Term] =
    aTerm |
      "succ" ~> aTerm ^^ TmSucc.apply |
      "pred" ~> aTerm ^^ TmPred.apply |
      "iszero" ~> aTerm ^^ TmIsZero.apply

  //  Atomic terms are ones that never require extra parentheses
  private def aTerm: Parser[Term] =
    "(" ~> term <~ ")" |
      "true" ^^ { _ => TmTrue } |
      "false" ^^ { _ => TmFalse } |
      numericLit ^^ { x => num(x.toInt) }

  private def num(x: Int): Term =
    x match {
      case 0 => TmZero
      case _ => TmSucc(num(x - 1))
    }

  private def eof: Parser[String] = elem("<eof>", _ == lexical.EOF) ^^ { _.chars }

  def input(s: String): List[Command] =
    phrase(topLevel)(new lexical.Scanner(s)) match {
      case t if t.successful => t.get
      case t                 => sys.error(t.toString)
    }

  def parseTerm(s: String): Term =
    phrase(term)(new lexical.Scanner(s)) match {
      case t if t.successful => t.get
      case t                 => sys.error(t.toString)
    }
}
