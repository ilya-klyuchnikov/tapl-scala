package tapl.simplebool

enum Term {
  // i - index, cl - context length
  case TmVar(i: Int, cl: Int)
  case TmAbs(v: String, ty: Ty, t: Term)
  case TmApp(t1: Term, t2: Term)
  case TmTrue
  case TmFalse
  case TmIf(cond: Term, t1: Term, t2: Term)
}

enum Ty {
  case TyArr(t1: Ty, t2: Ty)
  case TyBool
}

enum Command {
  case Eval(t: Term)
  case Bind(n: String, b: Binding)
}

enum Binding {
  // Binds variable and a name. Used during parsing
  // propagate names.
  case NameBind
  // Binds a variable to a type. Used during typechecking.
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

  private def tmMap[A](onVar: (Int, TmVar) => Term, c: Int, t: Term): Term = {
    def walk(c: Int, t: Term): Term =
      t match {
        case v: TmVar          => onVar(c, v)
        case TmAbs(x, ty1, t2) => TmAbs(x, ty1, walk(c + 1, t2))
        case TmApp(t1, t2)     => TmApp(walk(c, t1), walk(c, t2))
        case TmTrue            => TmTrue
        case TmFalse           => TmFalse
        case TmIf(t1, t2, t3)  => TmIf(walk(c, t1), walk(c, t2), walk(c, t3))
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
      if (v.i == c) termShift(c, s) else v
    }
    tmMap(f, j, t)
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
  import Ty._

  def ptyType(outer: Boolean, ty: Ty): Document =
    ty match {
      case ty => ptyArrowType(outer, ty)
    }

  def ptyArrowType(outer: Boolean, tyT: Ty): Document =
    tyT match {
      case TyArr(tyT1, tyT2) =>
        g0(ptyAType(false, tyT1) ::: " ->" :/: ptyArrowType(outer, tyT2))
      case tyT =>
        ptyAType(outer, tyT)
    }

  def ptyAType(outer: Boolean, tyT: Ty): Document =
    tyT match {
      case TyBool => "Bool"
      case tyT    => "(" ::: ptyType(outer, tyT) ::: ")"
    }

  def ptyTy(ty: Ty) = ptyType(true, ty)

  def ptmTerm(outer: Boolean, ctx: Context, t: Term): Document =
    t match {
      case TmAbs(x, tyT1, t2) =>
        val (ctx1, x1) = ctx.pickFreshName(x)
        val abs = g0("\\" ::: x1 ::: ":" ::: ptyType(false, tyT1) ::: ".")
        val body = ptmTerm(outer, ctx1, t2)
        g2(abs :/: body)
      case TmIf(t1, t2, t3) =>
        val ifB = g2("if" :/: ptmTerm(outer, ctx, t1))
        val thenB = g2("then" :/: ptmTerm(outer, ctx, t2))
        val elseB = g2("else" :/: ptmTerm(outer, ctx, t3))
        g0(ifB :/: thenB :/: elseB)
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
      case t =>
        "(" ::: ptmTerm(outer, ctx, t) ::: ")"
    }

  def pBinding(bind: Binding): Document =
    bind match {
      case NameBind    => empty
      case VarBind(ty) => ": " ::: ptyTy(ty)
    }
}
