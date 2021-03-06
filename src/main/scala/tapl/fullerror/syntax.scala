package tapl.fullerror

enum Ty {
  case TyVar(i: Int, cl: Int)
  case TyTop
  case TyBot
  case TyArr(t1: Ty, t2: Ty)
  case TyBool
}

enum Term {
  case TmVar(i: Int, cl: Int)
  case TmAbs(v: String, ty: Ty, t: Term)
  case TmApp(t1: Term, t2: Term)
  case TmTrue
  case TmFalse
  case TmIf(cond: Term, t1: Term, t2: Term)
  case TmError
  case TmTry(t1: Term, t2: Term)
}

enum Command {
  case Eval(t: Term)
  case Bind(n: String, b: Binding)
}

enum Binding {
  case NameBind
  case TyVarBind
  case VarBind(t: Ty)
  case TmAbbBind(t: Term, ty: Option[Ty])
  case TyAbbBind(ty: Ty)
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
  import Binding._
  import Term._
  import Ty._

  private def tyMap(onVar: (Int, TyVar) => Ty, c: Int, ty: Ty): Ty = {
    def walk(c: Int, ty: Ty): Ty =
      ty match {
        case tv: TyVar       => onVar(c, tv)
        case TyBool          => TyBool
        case TyArr(ty1, ty2) => TyArr(walk(c, ty1), walk(c, ty2))
        case TyTop           => TyTop
        case TyBot           => TyBot
      }
    walk(c, ty)
  }

  private def tmMap(onVar: (Int, TmVar) => Term, onType: (Int, Ty) => Ty, c: Int, t: Term): Term = {
    def walk(c: Int, t: Term): Term =
      t match {
        case v: TmVar          => onVar(c, v)
        case TmAbs(x, ty1, t2) => TmAbs(x, onType(c, ty1), walk(c + 1, t2))
        case TmApp(t1, t2)     => TmApp(walk(c, t1), walk(c, t2))
        case TmTrue            => TmTrue
        case TmFalse           => TmFalse
        case TmIf(t1, t2, t3)  => TmIf(walk(c, t1), walk(c, t2), walk(c, t3))
        case TmTry(t1, t2)     => TmTry(walk(c, t1), walk(c, t2))
        case TmError           => TmError
      }
    walk(c, t)
  }

  private def typeShiftAbove(d: Int, c: Int, ty: Ty): Ty = {
    val f = { (c: Int, v: TyVar) =>
      if (v.i >= c) TyVar(v.i + d, v.cl + d) else TyVar(v.i, v.cl + d)
    }
    tyMap(f, c, ty)
  }

  private def termShiftAbove(d: Int, c: Int, t: Term): Term = {
    val f = { (c: Int, v: TmVar) =>
      if (v.i >= c) TmVar(v.i + d, v.cl + d) else TmVar(v.i, v.cl + d)
    }
    tmMap(f, typeShiftAbove(d, _, _), c, t)
  }

  def termShift(d: Int, t: Term): Term =
    termShiftAbove(d, 0, t)

  def typeShift(d: Int, ty: Ty): Ty =
    typeShiftAbove(d, 0, ty)

  def bindingShift(d: Int, bind: Binding) =
    bind match {
      case NameBind  => NameBind
      case TyVarBind => TyVarBind
      case TmAbbBind(t, tyT) =>
        val tyT1 = tyT.map(typeShift(d, _))
        TmAbbBind(termShift(d, t), tyT1)
      case VarBind(tyT)   => VarBind(typeShift(d, tyT))
      case TyAbbBind(tyT) => TyAbbBind(typeShift(d, tyT))
    }

  // usual substitution: [j -> s]
  private def termSubst(j: Int, s: Term, t: Term): Term = {
    val onVar = { (c: Int, v: TmVar) =>
      if (v.i == c) termShift(c, s) else v
    }
    val onType = { (c: Int, ty: Ty) => ty }
    tmMap(onVar, onType, j, t)
  }

  // for beta-reduction
  def termSubstTop(s: Term, t: Term): Term =
    termShift(-1, termSubst(0, termShift(1, s), t))

  // [j -> tyS]
  private def typeSubst(tyS: Ty, j: Int, tyT: Ty) = {
    val onVar = { (c: Int, v: TyVar) =>
      if (v.i == c) typeShift(c, tyS) else v
    }
    tyMap(onVar, j, tyT)
  }

  // (tm ty) reduction - for system F
  def typeSubstTop(tyS: Ty, tyT: Ty): Ty =
    typeShift(-1, typeSubst(typeShift(1, tyS), 0, tyT))

  // really this is for system F only
  private def tytermSubst(tyS: Ty, j: Int, t: Term) =
    tmMap((c, tv) => tv, (j, tyT) => typeSubst(tyS, j, tyT), j, t)

  // really this is for system F only
  def tyTermSubstTop(tyS: Ty, t: Term): Term =
    termShift(-1, tytermSubst(typeShift(1, tyS), 0, t))

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
      case TyVar(x, n) =>
        if (ctx.length == n) ctx.index2Name(x)
        else text("[bad index: " + x + "/" + n + " in {" + ctx.l.mkString(", ") + "}]")
      case TyBot =>
        "Bot"
      case TyTop =>
        "Top"
      case TyBool =>
        "Bool"
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
      case TmIf(t1, t2, t3) =>
        val ifB = g2("if" :/: ptmTerm(outer, ctx, t1))
        val thenB = g2("then" :/: ptmTerm(outer, ctx, t2))
        val elseB = g2("else" :/: ptmTerm(outer, ctx, t2))
        g0(ifB :/: thenB :/: elseB)
      case TmTry(t1, t2) =>
        g0("try " ::: ptmTerm(false, ctx, t1) :/: "with " ::: ptmTerm(false, ctx, t1))
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
      case TmTrue =>
        "true"
      case TmFalse =>
        "false"
      case TmError =>
        "error"
      case t =>
        "(" ::: ptmTerm(outer, ctx, t) ::: ")"
    }

  def pBinding(ctx: Context, bind: Binding): Document =
    bind match {
      case NameBind =>
        empty
      case TyVarBind =>
        empty
      case VarBind(ty) =>
        ": " ::: ptyTy(ctx, ty)
      case TmAbbBind(t, tyT) =>
        "= " ::: ptm(ctx, t)
      case TyAbbBind(tyT) =>
        "= " ::: ptyTy(ctx, tyT)
    }

  def pBindingTy(ctx: Context, b: Binding): Document =
    b match {
      case NameBind =>
        empty
      case TyVarBind =>
        empty
      case VarBind(ty) =>
        ": " ::: ptyTy(ctx, ty)
      case TmAbbBind(t, Some(ty)) =>
        ": " ::: ptyTy(ctx, ty)
      case TmAbbBind(t, None) =>
        ": " ::: ptyTy(ctx, Typer.typeof(ctx, t))
      case TyAbbBind(ty) =>
        ":: *"
    }
}
