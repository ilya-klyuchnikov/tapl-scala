package tapl.arith

enum Term {
  case TmTrue
  case TmFalse
  case TmIf(cond: Term, t1: Term, t2: Term)
  case TmZero
  case TmSucc(t: Term)
  case TmPred(t: Term)
  case TmIsZero(t: Term)
}

sealed trait Command
case class Eval(t: Term) extends Command

import util.Document
import util.Document._

object PrettyPrinter {
  import scala.language.implicitConversions
  import util.Print._, util.Print.text2doc
  import Term._

  def ptmTerm(outer: Boolean, t: Term): Document =
    t match {
      case TmIf(t1, t2, t3) =>
        val ifB = g2("if" :/: ptmTerm(outer, t1))
        val thenB = g2("then" :/: ptmTerm(outer, t2))
        val elseB = g2("else" :/: ptmTerm(outer, t3))
        g0(ifB :/: thenB :/: elseB)
      case t =>
        ptmAppTerm(outer, t)
    }

  def ptmAppTerm(outer: Boolean, t: Term): Document =
    t match {
      case TmPred(t1) =>
        "pred " ::: ptmATerm(false, t1)
      case TmIsZero(t1) =>
        "iszero " ::: ptmATerm(false, t1)
      case t =>
        ptmATerm(outer, t)
    }

  def ptmATerm(outer: Boolean, t: Term): Document =
    t match {
      case TmTrue =>
        "true"
      case TmFalse =>
        "false"
      case TmZero =>
        "0"
      case TmSucc(t1) =>
        def pf(i: Int, t: Term): Document =
          t match {
            case TmZero =>
              i.toString()
            case TmSucc(s) =>
              pf(i + 1, s)
            case _ =>
              "(succ " ::: ptmATerm(false, t1) ::: ")"
          }
        pf(1, t1)
      case t =>
        "(" ::: ptmTerm(outer, t) ::: ")"
    }

  def ptm(t: Term) =
    ptmTerm(true, t)
}
