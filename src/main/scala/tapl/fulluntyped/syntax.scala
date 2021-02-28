package tapl.fulluntyped

sealed trait Term
case object TmTrue extends Term
case object TmFalse extends Term
case class TmIf(cond: Term, t1: Term, t2: Term) extends Term
case class TmVar(i: Int, cl: Int) extends Term
case class TmAbs(v: String, t: Term) extends Term
case class TmApp(t1: Term, t2: Term) extends Term
case class TmRecord(fields: List[(String, Term)]) extends Term
case class TmProj(t: Term, proj: String) extends Term
case class TmString(s: String) extends Term
case object TmZero extends Term
case class TmSucc(t: Term) extends Term
case class TmPred(t: Term) extends Term
case class TmIsZero(t: Term) extends Term
case class TmLet(l: String, t1: Term, t2: Term) extends Term

sealed trait Binding
case object NameBind extends Binding
case class TmAbbBind(t: Term) extends Binding

sealed trait Command
case class Eval(t: Term) extends Command
case class Bind(n: String, b: Binding) extends Command

case class Context(l: List[(String, Binding)] = List()) {
  val length: Int = l.length
  def addBinding(s: String, bind: Binding): Context = Context((s, bind) :: l)
  def addName(s: String): Context = addBinding(s, NameBind)
  def index2Name(i: Int): String = l(i)._1

  def getBinding(i: Int): Binding = {
    val bind = l(i)._2
    Syntax.bindingShift(i + 1, bind)
  }

  def name2index(s: String): Int =
    l.indexWhere { _._1 == s } match {
      case -1 => throw new Exception("identifier " + s + " is unbound")
      case i  => i
    }

  def isNameBound(s: String): Boolean = l.exists { _._1 == s }

  def pickFreshName(n: String): (Context, String) =
    if (isNameBound(n))
      pickFreshName(n + "'")
    else
      (Context((n, NameBind) :: l), n)
}

object Syntax {

  private def tmMap(onVar: (Int, TmVar) => Term, c: Int, t: Term): Term = {
    def walk(c: Int, t: Term): Term =
      t match {
        case v: TmVar         => onVar(c, v)
        case TmAbs(x, t2)     => TmAbs(x, walk(c + 1, t2))
        case TmApp(t1, t2)    => TmApp(walk(c, t1), walk(c, t2))
        case TmLet(x, t1, t2) => TmLet(x, walk(c, t1), walk(c + 1, t2))
        case TmTrue           => TmTrue
        case TmFalse          => TmFalse
        case TmIf(t1, t2, t3) => TmIf(walk(c, t1), walk(c, t2), walk(c, t3))
        case t: TmString      => t
        case TmProj(t1, l)    => TmProj(walk(c, t1), l)
        case TmRecord(fields) => TmRecord(fields.map { case (l, t) => (l, walk(c, t)) })
        case TmZero           => TmZero
        case TmSucc(t1)       => TmSucc(walk(c, t1))
        case TmPred(t1)       => TmPred(walk(c, t1))
        case TmIsZero(t1)     => TmIsZero(walk(c, t1))
      }
    walk(c, t)
  }

  private def termShiftAbove(d: Int, c: Int, t: Term): Term = {
    val f = { (c: Int, v: TmVar) =>
      if (v.i >= c) TmVar(v.i + d, v.cl + d) else TmVar(v.i, v.cl + d)
    }
    tmMap(f, c, t)
  }

  private def termShift(d: Int, t: Term): Term =
    termShiftAbove(d, 0, t)

  def bindingShift(d: Int, bind: Binding) =
    bind match {
      case NameBind =>
        NameBind
      case TmAbbBind(t) =>
        TmAbbBind(termShift(d, t))
    }

  // usual substitution: [j -> s]
  private def termSubst(j: Int, s: Term, t: Term): Term = {
    val onVar = { (c: Int, v: TmVar) =>
      if (v.i == c) termShift(c, s) else v
    }
    tmMap(onVar, j, t)
  }

  // for beta-reduction
  def termSubstTop(s: Term, t: Term): Term =
    termShift(-1, termSubst(0, termShift(1, s), t))
}

import util.Document
import util.Document._

// outer means that the term is the top-level term
object PrettyPrinter {
  import scala.language.implicitConversions
  import util.Print._, util.Print.text2doc

  def ptmTerm(outer: Boolean, ctx: Context, t: Term): Document =
    t match {

      case TmIf(t1, t2, t3) =>
        val ifB = g2("if" :/: ptmTerm(outer, ctx, t1))
        val thenB = g2("then" :/: ptmTerm(outer, ctx, t2))
        val elseB = g2("else" :/: ptmTerm(outer, ctx, t3))
        g0(ifB :/: thenB :/: elseB)
      case TmAbs(x, t2) =>
        val (ctx1, x1) = ctx.pickFreshName(x)
        val abs = g0("lambda" :/: x1 ::: ".")
        val body = ptmTerm(outer, ctx1, t2)
        g2(abs :/: body)
      case TmLet(x, t1, t2) =>
        g0(
          "let " ::: x ::: " = " ::: ptmTerm(false, ctx, t1) :/: "in" :/: ptmTerm(
            false,
            ctx.addName(x),
            t2,
          )
        )
      case t => ptmAppTerm(outer, ctx, t)

    }

  def ptmAppTerm(outer: Boolean, ctx: Context, t: Term): Document =
    t match {
      case TmApp(t1, t2) =>
        g2(ptmAppTerm(false, ctx, t1) :/: ptmATerm(false, ctx, t2))
      case TmPred(t1) =>
        "pred " ::: ptmATerm(false, ctx, t1)
      case TmIsZero(t1) =>
        "iszero " ::: ptmATerm(false, ctx, t1)
      case t =>
        ptmPathTerm(outer, ctx, t)
    }

  def ptmPathTerm(outer: Boolean, ctx: Context, t: Term): Document =
    t match {
      case TmProj(t1, l) =>
        ptmATerm(false, ctx, t1) ::: "." ::: l
      case t1 =>
        ptmATerm(outer, ctx, t1)
    }

  def ptmATerm(outer: Boolean, ctx: Context, t: Term): Document =
    t match {
      case TmTrue =>
        "true"
      case TmFalse =>
        "false"
      case TmVar(x, n) =>
        if (ctx.length == n) ctx.index2Name(x)
        else text("[bad index: " + x + "/" + n + " in {" + ctx.l.mkString(", ") + "}]")
      case TmString(s) =>
        "\"" ::: s ::: "\""
      case TmRecord(fields) =>
        def pf(i: Int, li: String, t: Term): Document =
          if (i.toString() == li) {
            ptmTerm(false, ctx, t)
          } else {
            li ::: "=" ::: ptmTerm(false, ctx, t)
          }
        "{" ::: fields.zipWithIndex
          .map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }
          .reduceLeftOption(_ ::: "," :/: _)
          .getOrElse(empty) ::: "}"
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
              "(succ " ::: ptmATerm(false, ctx, t1) ::: ")"
          }
        pf(1, t1)
      case t =>
        "(" ::: ptmTerm(outer, ctx, t) ::: ")"
    }

  def ptm(ctx: Context, t: Term) = ptmTerm(true, ctx, t)

  def pBinding(ctx: Context, bind: Binding): Document =
    bind match {
      case NameBind =>
        empty
      case TmAbbBind(t) =>
        "= " ::: ptm(ctx, t)
    }

}
