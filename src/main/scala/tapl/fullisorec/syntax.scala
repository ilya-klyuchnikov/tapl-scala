package tapl.fullisorec

sealed trait Ty
case class TyId(id: String) extends Ty
case class TyVar(i: Int, cl: Int) extends Ty
case class TyRecord(els: List[(String, Ty)]) extends Ty
case class TyArr(t1: Ty, t2: Ty) extends Ty
case object TyNat extends Ty
case class TyVariant(els: List[(String, Ty)]) extends Ty
case object TyBool extends Ty
case object TyString extends Ty
case object TyUnit extends Ty
case class TyRec(id: String, ty: Ty) extends Ty

sealed trait Term
case object TmTrue extends Term
case object TmFalse extends Term
case class TmIf(cond: Term, t1: Term, t2: Term) extends Term
case class TmVar(i: Int, cl: Int) extends Term
case class TmString(s: String) extends Term
case class TmAscribe(t: Term, ty: Ty) extends Term
case class TmRecord(fields: List[(String, Term)]) extends Term
case class TmProj(t: Term, proj: String) extends Term
case class TmAbs(v: String, ty: Ty, t: Term) extends Term
case class TmApp(t1: Term, t2: Term) extends Term
case object TmZero extends Term
case class TmSucc(t: Term) extends Term
case class TmPred(t: Term) extends Term
case class TmIsZero(t: Term) extends Term
case class TmCase(sel: Term, branches: List[(String, String, Term)]) extends Term
case class TmTag(tag: String, t: Term, ty: Ty) extends Term
case class TmLet(l: String, t1: Term, t2: Term) extends Term
case object TmUnit extends Term
case class TmFix(t: Term) extends Term
case class TmFold(ty: Ty) extends Term
case class TmUnfold(ty: Ty) extends Term

sealed trait Binding
case object NameBind extends Binding
case object TyVarBind extends Binding
case class VarBind(t: Ty) extends Binding
case class TmAbbBind(t: Term, ty: Option[Ty]) extends Binding
case class TyAbbBind(ty: Ty) extends Binding

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
    case VarBind(ty)            => ty
    case TmAbbBind(_, Some(ty)) => ty
    case TmAbbBind(_, None)     => throw new Exception("No type recorder for variable " + index2Name(i))
    case _                      => throw new Exception("Wrong kind of binding for " + index2Name(i))
  }
}

object Syntax {

  private def tyMap(onVar: (Int, TyVar) => Ty, c: Int, ty: Ty): Ty = {
    def walk(c: Int, ty: Ty): Ty = ty match {
      case TyString            => TyString
      case tv: TyVar           => onVar(c, tv)
      case id: TyId            => id
      case TyUnit              => TyUnit
      case TyRecord(fieldTys)  => TyRecord(fieldTys.map { case (li, tyi) => (li, walk(c, tyi)) })
      case TyBool              => TyBool
      case TyNat               => TyNat
      case TyArr(ty1, ty2)     => TyArr(walk(c, ty1), walk(c, ty2))
      case TyVariant(fieldTys) => TyVariant(fieldTys.map { case (li, tyi) => (li, walk(c, tyi)) })
      case TyRec(x, tyT)       => TyRec(x, walk(c + 1, tyT))
    }
    walk(c, ty)
  }

  private def tmMap(onVar: (Int, TmVar) => Term, onType: (Int, Ty) => Ty, c: Int, t: Term): Term = {
    def walk(c: Int, t: Term): Term = t match {
      case v: TmVar            => onVar(c, v)
      case TmAbs(x, ty1, t2)   => TmAbs(x, onType(c, ty1), walk(c + 1, t2))
      case TmApp(t1, t2)       => TmApp(walk(c, t1), walk(c, t2))
      case TmLet(x, t1, t2)    => TmLet(x, walk(c, t1), walk(c + 1, t2))
      case TmFix(t1)           => TmFix(walk(c, t1))
      case TmTrue              => TmTrue
      case TmFalse             => TmFalse
      case TmIf(t1, t2, t3)    => TmIf(walk(c, t1), walk(c, t2), walk(c, t3))
      case t: TmString         => t
      case TmUnit              => TmUnit
      case TmProj(t1, l)       => TmProj(walk(c, t1), l)
      case TmRecord(fields)    => TmRecord(fields.map { case (l, t) => (l, walk(c, t)) })
      case TmAscribe(t1, tyT1) => TmAscribe(walk(c, t1), onType(c, tyT1))
      case TmZero              => TmZero
      case TmSucc(t1)          => TmSucc(walk(c, t1))
      case TmPred(t1)          => TmPred(walk(c, t1))
      case TmIsZero(t1)        => TmIsZero(walk(c, t1))
      case TmTag(lbl, t1, tyT) => TmTag(lbl, walk(c, t1), onType(c, tyT))
      case TmCase(t, cases)    => TmCase(walk(c, t), cases.map { case (li, xi, ti) => (li, xi, walk(c + 1, ti)) })
      case TmFold(ty)          => TmFold(onType(c, ty))
      case TmUnfold(ty)        => TmUnfold(onType(c, ty))
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

  private def termShift(d: Int, t: Term): Term =
    termShiftAbove(d, 0, t)

  def typeShift(d: Int, ty: Ty): Ty =
    typeShiftAbove(d, 0, ty)

  def bindingShift(d: Int, bind: Binding) = bind match {
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

  // is used in equirec first time
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
  import util.Print._

  def ptyType(outer: Boolean, ctx: Context, ty: Ty): Document = ty match {
    case TyRec(x, tyT) =>
      val (ctx1, x1) = ctx.pickFreshName(x)
      g2("Rec " :: x :: "." :/: ptyType(outer, ctx1, tyT))
    case ty =>
      ptyArrowType(outer, ctx, ty)
  }

  def ptyArrowType(outer: Boolean, ctx: Context, tyT: Ty): Document = tyT match {
    case TyArr(tyT1, tyT2) =>
      g2(ptyAType(false, ctx, tyT1) :: " ->" :/: ptyArrowType(outer, ctx, tyT2))
    case tyT =>
      ptyAType(outer, ctx, tyT)
  }

  def ptyAType(outer: Boolean, ctx: Context, tyT: Ty): Document = tyT match {
    case TyVar(x, n) =>
      if (ctx.length == n) ctx.index2Name(x)
      else text("[bad index: " + x + "/" + n + " in {" + ctx.l.mkString(", ") + "}]")
    case TyId(b) =>
      b
    case TyBool =>
      "Bool"
    case TyVariant(fields) =>
      def pf(i: Int, li: String, tyTi: Ty): Document =
        if (i.toString() == li) {
          ptyType(false, ctx, tyTi)
        } else {
          li :: ":" :/: ptyType(false, ctx, tyTi)
        }
      "<" :: fields.zipWithIndex.map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }.
        reduceLeftOption(_ :: "," :/: _).getOrElse(empty) :: ">"
    case TyString =>
      "String"
    case TyUnit =>
      "Unit"
    case TyRecord(fields) =>
      def pf(i: Int, li: String, tyTi: Ty): Document =
        if (i.toString() == li) {
          ptyType(false, ctx, tyTi)
        } else {
          g0(li :: ":" :/: ptyType(false, ctx, tyTi))
        }
      g2("{" :: fields.zipWithIndex.map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }.reduceLeftOption(_ :: "," :/: _).getOrElse(empty) :: "}")
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
    case TmCase(t, cases) =>
      def pc(li: String, xi: String, ti: Term): Document = {
        val (ctx1, x1) = ctx.pickFreshName(xi)
        "<" :: li :: "=" :: xi :: ">==>" :: ptmTerm(false, ctx1, ti)
      }
      g2("case " :: ptmTerm(false, ctx, t) :: " of" :/:
        cases.map { case (x, y, z) => pc(x, y, z) }.foldRight(empty: Document)(_ :/: "|" :: _))
    case TmAbs(x, tyT1, t2) =>
      val (ctx1, x1) = ctx.pickFreshName(x)
      val abs = g0("lambda" :/: x1 :: ":" :/: ptyType(false, ctx, tyT1) :: ".")
      val body = ptmTerm(outer, ctx1, t2)
      g2(abs :/: body)
    case TmLet(x, t1, t2) =>
      g0("let " :: x :: " = " :: ptmTerm(false, ctx, t1) :/: "in" :/: ptmTerm(false, ctx.addName(x), t2))
    case TmFix(t1) =>
      g2("fix " :: ptmTerm(false, ctx, t1))
    case t => ptmAppTerm(outer, ctx, t)

  }

  def ptmAppTerm(outer: Boolean, ctx: Context, t: Term): Document = t match {
    case TmApp(t1, t2) =>
      g2(ptmAppTerm(false, ctx, t1) :/: ptmATerm(false, ctx, t2))
    case TmFold(tyS) =>
      g0("fold [" :: ptyType(false, ctx, tyS) :: "]")
    case TmUnfold(tyS) =>
      g0("unfold [" :: ptyType(false, ctx, tyS) :: "]")
    case TmPred(t1) =>
      "pred " :: ptmATerm(false, ctx, t1)
    case TmIsZero(t1) =>
      "iszero " :: ptmATerm(false, ctx, t1)
    case t =>
      ptmPathTerm(outer, ctx, t)
  }

  def ptmPathTerm(outer: Boolean, ctx: Context, t: Term): Document = t match {
    case TmProj(t1, l) =>
      ptmATerm(false, ctx, t1) :: "." :: l
    case t1 =>
      ptmAscribeTerm(outer, ctx, t1)
  }

  def ptmAscribeTerm(outer: Boolean, ctx: Context, t: Term): Document = t match {
    case TmAscribe(t1, tyT1) =>
      g0(ptmAppTerm(false, ctx, t1) :/: "as " :: ptyType(false, ctx, tyT1))
    case t1 =>
      ptmATerm(outer, ctx, t1)
  }

  def ptmATerm(outer: Boolean, ctx: Context, t: Term): Document = t match {
    case TmTrue =>
      "true"
    case TmFalse =>
      "false"
    case TmTag(l, t, ty) =>
      g2("<" :: l :: "=" :: ptmTerm(false, ctx, t) :: ">" :/: "as " :: ptyType(outer, ctx, ty))
    case TmVar(x, n) =>
      if (ctx.length == n) ctx.index2Name(x)
      else text("[bad index: " + x + "/" + n + " in {" + ctx.l.mkString(", ") + "}]")
    case TmString(s) =>
      "\"" :: s :: "\""
    case TmUnit =>
      "unit"
    case TmRecord(fields) =>
      def pf(i: Int, li: String, t: Term): Document =
        if (i.toString() == li) {
          ptmTerm(false, ctx, t)
        } else {
          li :: "=" :: ptmTerm(false, ctx, t)
        }
      "{" :: fields.zipWithIndex.map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }.
        reduceLeftOption(_ :: "," :/: _).getOrElse(empty) :: "}"
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
    case TyVarBind =>
      empty
    case VarBind(ty) =>
      ": " :: ptyTy(ctx, ty)
    case TmAbbBind(t, tyT) =>
      "= " :: ptm(ctx, t)
    case TyAbbBind(tyT) =>
      "= " :: ptyTy(ctx, tyT)
  }

  def pBindingTy(ctx: Context, b: Binding): Document = b match {
    case NameBind =>
      empty
    case TyVarBind =>
      empty
    case VarBind(ty) =>
      ": " :: ptyTy(ctx, ty)
    case TmAbbBind(t, Some(ty)) =>
      ": " :: ptyTy(ctx, ty)
    case TmAbbBind(t, None) =>
      ": " :: ptyTy(ctx, Typer.typeof(ctx, t))
    case TyAbbBind(ty) =>
      ":: *"
  }

}