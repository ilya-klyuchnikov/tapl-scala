package tapl.rcdsubbot

enum Ty {
  case TyTop
  case TyBot
  case TyArr(t1: Ty, t2: Ty)
  case TyRecord(els: List[(String, Ty)])
}

enum Term {
  case TmVar(i: Int, cl: Int)
  case TmAbs(v: String, ty: Ty, t: Term)
  case TmApp(t1: Term, t2: Term)
  case TmRecord(fields: List[(String, Term)])
  case TmProj(t: Term, proj: String)
}

enum Command {
  case Eval(t: Term)
  case Bind(n: String, b: Binding)
}

enum Binding {
  case NameBind
  case VarBind(t: Ty)
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
      pickFreshName(n + "'")
    else
      (Context((n, NameBind) :: l), n)

  def index2Name(i: Int): String =
    l(i)._1

  def name2index(s: String): Int =
    l.indexWhere { _._1 == s } match {
      case -1 =>
        throw new Exception("identifier " + s + " is unbound")
      case i =>
        i
    }

  def getBinding(i: Int): Binding =
    l(i)._2

  def getType(i: Int): Ty =
    getBinding(i) match {
      case VarBind(t) =>
        t
      case _ =>
        throw new Exception("Wrong kind of binding for " + index2Name(i))
    }
}

object Syntax {
  import Term._

  private def tmMap(onVar: (Int, TmVar) => Term, c: Int, t: Term): Term = {
    def walk(c: Int, t: Term): Term =
      t match {
        case v: TmVar          => onVar(c, v)
        case TmAbs(x, ty1, t2) => TmAbs(x, ty1, walk(c + 1, t2))
        case TmApp(t1, t2)     => TmApp(walk(c, t1), walk(c, t2))
        case TmProj(t1, l)     => TmProj(walk(c, t1), l)
        case TmRecord(fields)  => TmRecord(fields.map { case (l, t) => (l, walk(c, t)) })
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
  import Binding._
  import Term._
  import Ty._

  def ptyType(outer: Boolean, ctx: Context, ty: Ty): Document =
    ty match {
      case ty => ptyArrowType(outer, ctx, ty)
    }

  def ptyArrowType(outer: Boolean, ctx: Context, tyT: Ty): Document =
    tyT match {
      case TyArr(tyT1, tyT2) =>
        g2(ptyAType(false, ctx, tyT1) ::: " ->" :/: ptyArrowType(outer, ctx, tyT2))
      case tyT =>
        ptyAType(outer, ctx, tyT)
    }

  def ptyAType(outer: Boolean, ctx: Context, tyT: Ty): Document =
    tyT match {
      case TyRecord(fields) =>
        def pf(i: Int, li: String, tyTi: Ty): Document =
          if (i.toString() == li) {
            ptyType(false, ctx, tyTi)
          } else {
            g0(li ::: ":" :/: ptyType(false, ctx, tyTi))
          }
        g2(
          "{" ::: fields.zipWithIndex
            .map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }
            .reduceLeftOption(_ ::: "," :/: _)
            .getOrElse(empty) ::: "}"
        )
      case TyBot =>
        "Bot"
      case TyTop =>
        "Top"
      case tyT =>
        "(" ::: ptyType(outer, ctx, tyT) ::: ")"
    }

  def ptyTy(ctx: Context, ty: Ty) = ptyType(true, ctx, ty)

  def ptmTerm(outer: Boolean, ctx: Context, t: Term): Document =
    t match {
      case TmAbs(x, tyT1, t2) =>
        val (ctx1, x1) = ctx.pickFreshName(x)
        val abs = g0("lambda" :/: x1 ::: ":" :/: ptyType(false, ctx, tyT1) ::: ".")
        val body = ptmTerm(outer, ctx1, t2)
        g2(abs :/: body)
      case t => ptmAppTerm(outer, ctx, t)
    }

  def ptmAppTerm(outer: Boolean, ctx: Context, t: Term): Document =
    t match {
      case TmApp(t1, t2) =>
        g2(ptmAppTerm(false, ctx, t1) :/: ptmATerm(false, ctx, t2))
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
      case TmVar(x, n) =>
        if (ctx.length == n) ctx.index2Name(x)
        else text("[bad index: " + x + "/" + n + " in {" + ctx.l.mkString(", ") + "}]")
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
      case t =>
        "(" ::: ptmTerm(outer, ctx, t) ::: ")"
    }

  def ptm(ctx: Context, t: Term) = ptmTerm(true, ctx, t)

  def pBinding(ctx: Context, bind: Binding): Document =
    bind match {
      case NameBind =>
        empty
      case VarBind(ty) =>
        ": " ::: ptyTy(ctx, ty)
    }

  def pBindingTy(ctx: Context, b: Binding): Document =
    b match {
      case NameBind =>
        empty
      case VarBind(ty) =>
        ": " ::: ptyTy(ctx, ty)
    }
}
