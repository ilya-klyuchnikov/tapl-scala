package tapl.fullrecon

sealed trait Ty
case class TyId(id: String) extends Ty
case class TyArr(t1: Ty, t2: Ty) extends Ty
case object TyBool extends Ty
case object TyNat extends Ty

sealed trait Term
case class TmVar(i: Int, cl: Int) extends Term
case class TmLet(l: String, t1: Term, t2: Term) extends Term
case object TmTrue extends Term
case object TmFalse extends Term
case class TmIf(cond: Term, t1: Term, t2: Term) extends Term
case object TmZero extends Term
case class TmSucc(t: Term) extends Term
case class TmPred(t: Term) extends Term
case class TmIsZero(t: Term) extends Term
case class TmAbs(v: String, ty: Option[Ty], t: Term) extends Term
case class TmApp(t1: Term, t2: Term) extends Term

sealed trait Binding
case object NameBind extends Binding
case class VarBind(t: Ty) extends Binding

sealed trait Command
case class Eval(t: Term) extends Command
case class Bind(n: String, b: Binding) extends Command

case class Context(l: List[(String, Binding)] = List()) {
  val length: Int = l.length
  def addBinding(s: String, bind: Binding): Context = Context((s, bind) :: l)
  def addName(s: String): Context = addBinding(s, NameBind)
  def index2Name(i: Int): String = l(i)._1

  def getBinding(i: Int): Binding =
    l(i)._2

  def name2index(s: String): Int = l.indexWhere { _._1 == s } match {
    case -1 => throw new Exception("identifier " + s + " is unbound")
    case i  => i
  }

  def isNameBound(s: String): Boolean = l.exists { _._1 == s }

  def pickFreshName(n: String): (Context, String) =
    if (isNameBound(n))
      pickFreshName(n + "'")
    else
      (Context((n, NameBind) :: l), n)

  def getType(i: Int): Ty = getBinding(i) match {
    case VarBind(ty) => ty
    case _           => throw new Exception("Wrong kind of binding for " + index2Name(i))
  }
}

object Syntax {

  private def tmMap(onVar: (Int, TmVar) => Term, c: Int, t: Term): Term = {
    def walk(c: Int, t: Term): Term = t match {
      case v: TmVar          => onVar(c, v)
      case TmAbs(x, ty1, t2) => TmAbs(x, ty1, walk(c + 1, t2))
      case TmApp(t1, t2)     => TmApp(walk(c, t1), walk(c, t2))
      case TmLet(x, t1, t2)  => TmLet(x, walk(c, t1), walk(c + 1, t2))
      case TmTrue            => TmTrue
      case TmFalse           => TmFalse
      case TmIf(t1, t2, t3)  => TmIf(walk(c, t1), walk(c, t2), walk(c, t3))
      case TmZero            => TmZero
      case TmSucc(t1)        => TmSucc(walk(c, t1))
      case TmPred(t1)        => TmPred(walk(c, t1))
      case TmIsZero(t1)      => TmIsZero(walk(c, t1))
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
  import util.Print._

  def ptyType(outer: Boolean, ctx: Context, ty: Ty): Document = ty match {
    case ty => ptyArrowType(outer, ctx, ty)
  }

  def ptyArrowType(outer: Boolean, ctx: Context, tyT: Ty): Document = tyT match {
    case TyArr(tyT1, tyT2) =>
      g2(ptyAType(false, ctx, tyT1) :: " ->" :/: ptyArrowType(outer, ctx, tyT2))
    case tyT =>
      ptyAType(outer, ctx, tyT)
  }

  def ptyAType(outer: Boolean, ctx: Context, tyT: Ty): Document = tyT match {
    case TyId(b) =>
      b
    case TyBool =>
      "Bool"
    case TyNat =>
      "Nat"
    case tyT =>
      "(" :: ptyType(outer, ctx, tyT) :: ")"
  }

  def ptyTy(ctx: Context, ty: Ty) = ptyType(true, ctx, ty)

  def ptmTerm(outer: Boolean, ctx: Context, t: Term): Document = t match {

    case TmIf(t1, t2, t3) =>
      val ifB = g2("if" :/: ptmTerm(outer, ctx, t1))
      val thenB = g2("then" :/: ptmTerm(outer, ctx, t2))
      val elseB = g2("else" :/: ptmTerm(outer, ctx, t3))
      g0(ifB :/: thenB :/: elseB)
    case TmAbs(x, Some(tyT1), t2) =>
      val (ctx1, x1) = ctx.pickFreshName(x)
      val abs = g0("lambda" :/: x1 :: ":" :/: ptyType(false, ctx, tyT1) :: ".")
      val body = ptmTerm(outer, ctx1, t2)
      g2(abs :/: body)
    case TmAbs(x, None, t2) =>
      val (ctx1, x1) = ctx.pickFreshName(x)
      val abs = g0("lambda" :/: x1 :: ".")
      val body = ptmTerm(outer, ctx1, t2)
      g2(abs :/: body)
    case TmLet(x, t1, t2) =>
      g0("let " :: x :: " = " :: ptmTerm(false, ctx, t1) :/: "in" :/: ptmTerm(false, ctx.addName(x), t2))
    case t => ptmAppTerm(outer, ctx, t)
  }

  def ptmAppTerm(outer: Boolean, ctx: Context, t: Term): Document = t match {
    case TmApp(t1, t2) =>
      g2(ptmAppTerm(false, ctx, t1) :/: ptmATerm(false, ctx, t2))
    case TmPred(t1) =>
      "pred " :: ptmATerm(false, ctx, t1)
    case TmIsZero(t1) =>
      "iszero " :: ptmATerm(false, ctx, t1)
    case t =>
      ptmATerm(outer, ctx, t)
  }

  def ptmATerm(outer: Boolean, ctx: Context, t: Term): Document = t match {
    case TmTrue =>
      "true"
    case TmFalse =>
      "false"
    case TmVar(x, n) =>
      if (ctx.length == n) ctx.index2Name(x)
      else text("[bad index: " + x + "/" + n + " in {" + ctx.l.mkString(", ") + "}]")
    case TmZero =>
      "0"
    case TmSucc(t1) =>
      def pf(i: Int, t: Term): Document = t match {
        case TmZero =>
          i.toString()
        case TmSucc(s) =>
          pf(i + 1, s)
        case _ =>
          "(succ " :: ptmATerm(false, ctx, t1) :: ")"
      }
      pf(1, t1)
    case t =>
      "(" :: ptmTerm(outer, ctx, t) :: ")"
  }

  def ptm(ctx: Context, t: Term) = ptmTerm(true, ctx, t)

  def pBinding(ctx: Context, bind: Binding): Document = bind match {
    case NameBind =>
      empty
    case VarBind(ty) =>
      ": " :: ptyTy(ctx, ty)
  }

  def pBindingTy(ctx: Context, b: Binding): Document = b match {
    case NameBind =>
      empty
    case VarBind(ty) =>
      ": " :: ptyTy(ctx, ty)
  }

}