package tapl.untyped

enum Term {
  // i - index, cl - context length
  case TmVar(i: Int, cl: Int)
  case TmAbs(v: String, t: Term)
  case TmApp(t1: Term, t2: Term)
}

enum Command {
  case Eval(t: Term)
  case Bind(n: String, b: Binding)
}

enum Binding {
  case NameBind
}

case class Context(l: List[(String, Binding)] = List()) {
  import Binding._

  val length: Int =
    l.length

  def addBinding(s: String, bind: Binding): Context =
    Context((s, bind) :: l)

  def addName(s: String): Context =
    addBinding(s, NameBind)

  def isNameBound(s: String): Boolean =
    l.exists { _._1 == s }

  def pickFreshName(n: String): (Context, String) =
    if (isNameBound(n))
      pickFreshName(n + "_")
    else
      (Context((n, NameBind) :: l), n)

  def index2Name(i: Int): String =
    l(i)._1

  def name2index(s: String): Int =
    l.indexWhere { _._1 == s } match {
      case -1 => throw new Exception("identifier " + s + " is unbound")
      case i  => i
    }

  def getBinding(i: Int): Binding =
    l(i)._2

}

object Syntax {
  import Term._

  private def tmMap[A](onVar: (Int, TmVar) => Term, c: Int, t: Term): Term = {
    def walk(c: Int, t: Term): Term =
      t match {
        case v: TmVar      => onVar(c, v)
        case TmAbs(x, t2)  => TmAbs(x, walk(c + 1, t2))
        case TmApp(t1, t2) => TmApp(walk(c, t1), walk(c, t2))
      }
    walk(c, t)
  }

  private def termShiftAbove(d: Int, c: Int, t: Term): Term = {
    val f = { (c: Int, v: TmVar) =>
      if (v.i >= c) TmVar(v.i + d, v.cl + d) else TmVar(v.i, v.cl + d)
    }
    tmMap(f, c, t)
  }

  def termShift(d: Int, t: Term): Term =
    termShiftAbove(d, 0, t)

  // usual substitution: [j -> s]
  def termSubst(j: Int, s: Term, t: Term): Term = {
    val f = { (c: Int, v: TmVar) =>
      if (v.i == j + c) termShift(c, s) else v
    }
    tmMap(f, 0, t)
  }

  def termSubstTop(s: Term, t: Term): Term =
    termShift(-1, termSubst(0, termShift(1, s), t))

}

import util.Document
import util.Document._

// outer means that the term is the top-level term
object PrettyPrinter {
  import scala.language.implicitConversions
  import util.Print._, util.Print.text2doc

  import Binding._
  import Term._

  def ptmTerm(outer: Boolean, ctx: Context, t: Term): Document =
    t match {
      case TmAbs(x, t2) =>
        val (ctx1, x1) = ctx.pickFreshName(x)
        val abs = g0("\\" ::: x1 ::: ".")
        val body = ptmTerm(outer, ctx1, t2)
        g2(abs :/: body)
      case t => ptmAppTerm(outer, ctx, t)

    }

  def ptmAppTerm(outer: Boolean, ctx: Context, t: Term): Document =
    t match {
      case TmApp(t1, t2) =>
        g2(ptmAppTerm(false, ctx, t1) :/: ptmATerm(false, ctx, t2))
      case t =>
        ptmATerm(outer, ctx, t)
    }

  def ptm(ctx: Context, t: Term) = ptmTerm(true, ctx, t)

  def ptmATerm(outer: Boolean, ctx: Context, t: Term): Document =
    t match {
      case TmVar(x, n) =>
        if (ctx.length == n) ctx.index2Name(x)
        else text("[bad index: " + x + "/" + n + " in {" + ctx.l.mkString(", ") + "}]")
      case t =>
        "(" ::: ptmTerm(outer, ctx, t) ::: ")"
    }

  def pBinding(bind: Binding): Document =
    bind match {
      case NameBind => empty
    }
}
