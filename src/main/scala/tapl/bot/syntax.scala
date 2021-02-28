package tapl.bot

sealed trait Ty
case object TyTop extends Ty
case object TyBot extends Ty
case class TyArr(t1: Ty, t2: Ty) extends Ty

sealed trait Term
case class TmVar(i: Int, cl: Int) extends Term
case class TmAbs(v: String, ty: Ty, t: Term) extends Term
case class TmApp(t1: Term, t2: Term) extends Term

sealed trait Command
case class Eval(t: Term) extends Command
case class Bind(n: String, b: Binding) extends Command

sealed trait Binding
case object NameBind extends Binding
case class VarBind(t: Ty) extends Binding

case class Context(l: List[(String, Binding)] = List()) {
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

  private def tmMap(onVar: (Int, TmVar) => Term, c: Int, t: Term): Term = {
    def walk(c: Int, t: Term): Term =
      t match {
        case v: TmVar          => onVar(c, v)
        case TmAbs(x, ty1, t2) => TmAbs(x, ty1, walk(c + 1, t2))
        case TmApp(t1, t2)     => TmApp(walk(c, t1), walk(c, t2))
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
        ptmATerm(outer, ctx, t)
    }

  def ptmATerm(outer: Boolean, ctx: Context, t: Term): Document =
    t match {
      case TmVar(x, n) =>
        if (ctx.length == n) ctx.index2Name(x)
        else text("[bad index: " + x + "/" + n + " in {" + ctx.l.mkString(", ") + "}]")
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
