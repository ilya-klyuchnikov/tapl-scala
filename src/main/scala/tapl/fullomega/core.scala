package tapl.fullomega
import util.Print

object Util {
  import Term._

  def isNumericVal(ctx: Context, t: Term): Boolean =
    t match {
      case TmZero     => true
      case TmSucc(t1) => isNumericVal(ctx, t1)
      case _          => false
    }

  def isVal(ctx: Context, t: Term): Boolean =
    t match {
      case TmString(_)                        => true
      case TmUnit                             => true
      case TmLoc(_)                           => true
      case TmTrue                             => true
      case TmFalse                            => true
      case t1 if isNumericVal(ctx, t1)        => true
      case TmAbs(_, _, _)                     => true
      case TmRecord(fields)                   => fields.forall { case (_, ti) => isVal(ctx, ti) }
      case TmPack(_, v1, _) if isVal(ctx, v1) => true
      case TmTAbs(_, _, _)                    => true
      case _                                  => false
    }
}

case class Store(l: List[Term] = List()) {
  import Syntax._

  def extend(v: Term): (Int, Store) =
    (l.length, Store(l :+ v))
  def lookup(i: Int): Term =
    l(i)
  def update(n: Int, v: Term): Store =
    Store(l.updated(n, v))
  def shift(i: Int): Store =
    Store(l.map(termShift(i, _)))
}

object Evaluator {
  import Util._
  import Syntax._

  import Binding._
  import Term._

  private def eval1(ctx: Context, store: Store, t: Term): (Term, Store) =
    t match {
      case TmAscribe(v1, tyT) if isVal(ctx, v1) =>
        (v1, store)
      case TmAscribe(t1, tyT) =>
        val (t11, store1) = eval1(ctx, store, t1)
        (TmAscribe(t11, tyT), store1)
      case TmApp(TmAbs(x, ty, t), v2) if isVal(ctx, v2) =>
        (termSubstTop(v2, t), store)
      case TmApp(v1, t2) if isVal(ctx, v1) =>
        val (t21, store1) = eval1(ctx, store, t2)
        (TmApp(v1, t21), store1)
      case TmApp(t1, t2) =>
        val (t11, store1) = eval1(ctx, store, t1)
        (TmApp(t11, t2), store1)
      case TmRecord(fields) =>
        def evalAField(l: List[(String, Term)]): (List[(String, Term)], Store) =
          l match {
            case Nil =>
              throw new NoRuleApplies(t)
            case (l, vi) :: rest if isVal(ctx, vi) =>
              val (rest1, store1) = evalAField(rest)
              ((l, vi) :: rest1, store1)
            case (l, ti) :: rest =>
              val (ti1, store1) = eval1(ctx, store, ti)
              ((l, ti1) :: rest, store1)
          }
        val (fields1, store1) = evalAField(fields)
        (TmRecord(fields1), store1)
      case TmProj(v1 @ TmRecord(fields), l) if isVal(ctx, v1) =>
        fields.find { _._1 == l } match {
          case Some((_, ti)) => (ti, store)
          case None          => throw new NoRuleApplies(t)
        }
      case TmProj(t1, l) =>
        val (t11, store1) = eval1(ctx, store, t1)
        (TmProj(t11, l), store1)
      case TmRef(t1) =>
        if (!isVal(ctx, t1)) {
          val (t11, store1) = eval1(ctx, store, t1)
          (TmRef(t11), store1)
        } else {
          val (l, store1) = store.extend(t1)
          (TmLoc(l), store1)
        }
      case TmDeref(t1) =>
        if (!isVal(ctx, t1)) {
          val (t11, store1) = eval1(ctx, store, t1)
          (TmDeref(t11), store1)
        } else {
          t1 match {
            case TmLoc(l) => (store.lookup(l), store)
            case _        => throw new NoRuleApplies(t)
          }
        }
      case TmAssign(t1, t2) =>
        if (!isVal(ctx, t1)) {
          val (t11, store1) = eval1(ctx, store, t1)
          (TmAssign(t11, t2), store1)
        } else if (!isVal(ctx, t2)) {
          val (t21, store1) = eval1(ctx, store, t2)
          (TmAssign(t1, t21), store1)
        } else {
          t1 match {
            case TmLoc(l) => (TmUnit, store.update(l, t2))
            case _        => throw new NoRuleApplies(t)
          }
        }
      case TmLet(x, v1, t2) if isVal(ctx, v1) =>
        (termSubstTop(v1, t2), store)
      case TmLet(x, t1, t2) =>
        val (t11, store1) = eval1(ctx, store, t1)
        (TmLet(x, t11, t2), store1)
      case TmIf(TmTrue, t2, t3) =>
        (t2, store)
      case TmIf(TmFalse, t2, t3) =>
        (t3, store)
      case TmIf(t1, t2, t3) =>
        val (t11, store1) = eval1(ctx, store, t1)
        (TmIf(t11, t2, t3), store1)
      case TmSucc(t1) =>
        val (t11, store1) = eval1(ctx, store, t1)
        (TmSucc(t11), store1)
      case TmPred(TmZero) =>
        (TmZero, store)
      case TmPred(TmSucc(nv1)) if isNumericVal(ctx, nv1) =>
        (nv1, store)
      case TmPred(t1) =>
        val (t2, store1) = eval1(ctx, store, t1)
        (TmPred(t2), store)
      case TmIsZero(TmZero) =>
        (TmTrue, store)
      case TmIsZero(TmSucc(nv1)) if isNumericVal(ctx, nv1) =>
        (TmFalse, store)
      case TmIsZero(t1) =>
        val (t2, store1) = eval1(ctx, store, t1)
        (TmIsZero(t2), store)
      case t @ TmFix(v1) if isVal(ctx, v1) =>
        v1 match {
          case TmAbs(_, _, t12) => (termSubstTop(t, t12), store)
          case _                => throw new NoRuleApplies(t)
        }
      case TmFix(t1) =>
        val (t2, store1) = eval1(ctx, store, t1)
        (TmFix(t2), store1)
      case TmTApp(TmTAbs(x, _, t11), tyT2) =>
        (tyTermSubstTop(tyT2, t11), store)
      case TmTApp(t1, tyT2) =>
        val (t2, store1) = eval1(ctx, store, t1)
        (TmTApp(t2, tyT2), store1)
      case TmUnPack(_, _, TmPack(tyT11, v12, _), t2) if isVal(ctx, v12) =>
        (tyTermSubstTop(tyT11, termSubstTop(termShift(1, v12), t2)), store)
      case TmUnPack(tyX, x, t1, t2) =>
        val (t11, store1) = eval1(ctx, store, t1)
        (TmUnPack(tyX, x, t11, t2), store1)
      case TmVar(n, _) =>
        ctx.getBinding(n) match {
          case TmAbbBind(t1, _) => (t1, store)
          case _                => throw new NoRuleApplies(t)
        }
      case _ =>
        throw new NoRuleApplies(t)
    }

  def eval(ctx: Context, store: Store, t: Term): (Term, Store) =
    try {
      val (t1, store1) = eval1(ctx, store, t)
      eval(ctx, store1, t1)
    } catch {
      case _: NoRuleApplies => (t, store)
    }

  def evalBinding(ctx: Context, store: Store, bind: Binding): (Binding, Store) =
    bind match {
      case TmAbbBind(t, tyT) =>
        val (t1, store1) = eval(ctx, store, t)
        (TmAbbBind(t1, tyT), store1)
      case b =>
        (b, store)
    }
}

object Typer {
  import Syntax._
  import Binding._
  import Kind._
  import Term._
  import Ty._

  private def isTyAbb(ctx: Context, i: Int) =
    ctx.getBinding(i) match {
      case TyAbbBind(_, _) => true
      case _               => false
    }

  private def getTyAbb(ctx: Context, i: Int) =
    ctx.getBinding(i) match {
      case TyAbbBind(ty, _) => ty
      case _                => throw new NoRuleApplies(null)
    }

  private def computeTy(ctx: Context, tyT: Ty) =
    tyT match {
      case TyVar(i, _) if isTyAbb(ctx, i) =>
        getTyAbb(ctx, i)
      case TyApp(TyAbs(_, _, tyT12), tyT2) =>
        typeSubstTop(tyT2, tyT12)
      case _ =>
        throw new NoRuleApplies(null)
    }

  def simplifyTy(ctx: Context, ty: Ty): Ty = {
    val tyT = ty match {
      case TyApp(tyT1, tyT2) => TyApp(simplifyTy(ctx, tyT1), tyT2)
      case _                 => ty
    }
    try {
      val tyT1 = computeTy(ctx, tyT)
      simplifyTy(ctx, tyT1)
    } catch {
      case _: NoRuleApplies => tyT
    }
  }

  def tyEqv(ctx: Context, ty1: Ty, ty2: Ty): Boolean = {
    val tyS = simplifyTy(ctx, ty1)
    val tyT = simplifyTy(ctx, ty2)
    (tyS, tyT) match {
      case (TyString, TyString)       => true
      case (TyId(b1), TyId(b2))       => b1 == b2
      case (TyUnit, TyUnit)           => true
      case (TyRef(tyT1), TyRef(tyT2)) => tyEqv(ctx, tyT1, tyT2)
      case (TyVar(i, _), _) if isTyAbb(ctx, i) =>
        tyEqv(ctx, getTyAbb(ctx, i), tyT)
      case (_, TyVar(i, _)) if isTyAbb(ctx, i) =>
        tyEqv(ctx, tyS, getTyAbb(ctx, i))
      case (TyVar(i, _), TyVar(j, _)) => i == j
      case (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) =>
        tyEqv(ctx, tyS1, tyT1) && tyEqv(ctx, tyS2, tyT2)
      case (TyBool, TyBool) => true
      case (TyNat, TyNat)   => true
      case (TyRecord(fields1), TyRecord(fields2)) =>
        fields1.length == fields2.length && fields2.forall {
          case (li2, tyTi2) =>
            fields1.find { _._1 == li2 } match {
              case Some((li1, tyTi1)) => tyEqv(ctx, tyTi1, tyTi2)
              case None               => false
            }
        }
      case (TySome(tyX1, knK1, tyS2), TySome(_, knK11, tyT2)) =>
        (knK1 == knK11) && tyEqv(ctx.addName(tyX1), tyS2, tyT2)
      case (TyAll(tyX1, knKS1, tyS2), TyAll(_, knKT2, tyT2)) =>
        (knKS1 == knKT2) && tyEqv(ctx.addName(tyX1), tyS2, tyT2)
      case (TyAbs(tyX1, knKS1, tyS2), TyAbs(_, knKT1, tyT2)) =>
        (knKS1 == knKT1) && tyEqv(ctx.addName(tyX1), tyS2, tyT2)
      case (TyApp(tyS1, tyS2), TyApp(tyT1, tyT2)) =>
        tyEqv(ctx, tyS1, tyT1) && tyEqv(ctx, tyS2, tyT2)
      case _ => false
    }
  }

  def getKind(ctx: Context, i: Int) =
    ctx.getBinding(i) match {
      case TyVarBind(knK)          => knK
      case TyAbbBind(_, Some(knK)) => knK
      case TyAbbBind(_, None)      => sys.error("No kind recorded for var " + ctx.index2Name(i))
      case _                       => sys.error("Wrong kind of binding for var " + ctx.index2Name(i))
    }

  def kindof(ctx: Context, tyT: Ty): Kind =
    tyT match {
      case TyArr(tyT1, tyT2) =>
        checkKindStar(ctx, tyT1)
        checkKindStar(ctx, tyT2)
        KnStar
      case TyVar(i, _) =>
        getKind(ctx, i)
      case TyRecord(fields) =>
        fields.foreach { case (l, tyS) => checkKindStar(ctx, tyS) }
        KnStar
      case TyAll(tyX, knK1, tyT2) =>
        val ctx1 = ctx.addBinding(tyX, TyVarBind(knK1))
        checkKindStar(ctx1, tyT2)
        KnStar
      case TyAbs(tyX, knK1, tyT2) =>
        val ctx1 = ctx.addBinding(tyX, TyVarBind(knK1))
        val knK2 = kindof(ctx1, tyT2)
        KnArr(knK1, knK2)
      case TyApp(tyT1, tyT2) =>
        val knK1 = kindof(ctx, tyT1)
        val knK2 = kindof(ctx, tyT2)
        knK1 match {
          case KnArr(knK11, knK12) =>
            if (knK2 == knK11)
              knK12
            else sys.error("parameter kind mismatch")
          case _ => sys.error("arrow type expected here")
        }
      case TySome(tyX, knK, tyT2) =>
        val ctx1 = ctx.addBinding(tyX, TyVarBind(knK))
        checkKindStar(ctx1, tyT2)
        KnStar
      case _ =>
        KnStar
    }

  def checkKindStar(ctx: Context, tyT: Ty): Unit =
    kindof(ctx, tyT) match {
      case KnStar => ()
      case _      => sys.error("kind * expected")
    }

  def typeof(ctx: Context, t: Term): Ty =
    t match {
      case TmAscribe(t1, tyT) =>
        checkKindStar(ctx, tyT)
        if (tyEqv(ctx, typeof(ctx, t1), tyT))
          tyT
        else
          throw new Exception("body of as-term doesn't have the expected type in " + t)
      case TmVar(i, _) =>
        ctx.getType(i)
      case TmAbs(v, tyT1, t2) =>
        checkKindStar(ctx, tyT1)
        val ctx1 = ctx.addBinding(v, VarBind(tyT1))
        val tyT2 = typeof(ctx1, t2)
        TyArr(tyT1, typeShift(-1, tyT2))
      case TmApp(t1, t2) =>
        val tyT1 = typeof(ctx, t1)
        val tyT2 = typeof(ctx, t2)
        simplifyTy(ctx, tyT1) match {
          case TyArr(tyT11, tyT12) =>
            if (tyEqv(ctx, tyT2, tyT11))
              tyT12
            else
              throw new Exception("parameter mismatch in " + t + " : " + tyT2 + " != " + tyT11)
          case z =>
            throw new Exception("arrow type expected in " + t1)
        }
      case TmString(_) =>
        TyString
      case TmUnit =>
        TyUnit
      case TmRef(t1) =>
        TyRef(typeof(ctx, t1))
      case TmLoc(l) =>
        sys.error("locations are not supposed to occur in source programs!")
      case TmDeref(t1) =>
        simplifyTy(ctx, typeof(ctx, t1)) match {
          case TyRef(tyT1) => tyT1
          case _           => sys.error("argument of ! is not a Ref")
        }
      case TmAssign(t1, t2) =>
        simplifyTy(ctx, typeof(ctx, t1)) match {
          case TyRef(tyT1) =>
            if (tyEqv(ctx, typeof(ctx, t2), tyT1))
              TyUnit
            else
              sys.error("arguments of := are incompatible")
          case _ =>
            sys.error("argument of ! is not a Ref")
        }
      case TmRecord(fields) =>
        val fieldTys = fields.map { case (li, ti) => (li, typeof(ctx, ti)) }
        TyRecord(fieldTys)
      case TmProj(t1, l) =>
        simplifyTy(ctx, typeof(ctx, t1)) match {
          case TyRecord(fieldTys) =>
            fieldTys find { _._1 == l } match {
              case Some((_, tyi)) => tyi
              case None           => throw new Exception("Label " + l + " not found in " + t)
            }
          case _ => throw new Exception("Expected record type for " + t1)
        }
      case TmTrue =>
        TyBool
      case TmFalse =>
        TyBool
      case TmIf(t1, t2, t3) =>
        if (tyEqv(ctx, typeof(ctx, t1), TyBool)) {
          val ty2 = typeof(ctx, t2)
          if (tyEqv(ctx, ty2, typeof(ctx, t3))) {
            ty2
          } else {
            throw new Exception("arms of conditional " + t + " have different types")
          }
        } else {
          throw new Exception("guard of conditional " + t + " is not a boolean")
        }
      case TmLet(x, t1, t2) =>
        val tyT1 = typeof(ctx, t1)
        val ctx1 = ctx.addBinding(x, VarBind(tyT1))
        typeShift(-1, typeof(ctx1, t2))
      case TmFix(t1) =>
        val tyT1 = typeof(ctx, t1)
        simplifyTy(ctx, tyT1) match {
          case TyArr(tyT11, tyT12) =>
            if (tyEqv(ctx, tyT12, tyT11))
              tyT12
            else
              throw new Exception(
                "result of body not compatible with domain " + t + " : " + tyT12 + " != " + tyT11
              )
          case _ =>
            throw new Exception("arrow type expected in " + t1)
        }
      case TmTAbs(tyX, knK1, t2) =>
        val ctx1 = ctx.addBinding(tyX, TyVarBind(knK1))
        val tyT2 = typeof(ctx1, t2)
        TyAll(tyX, knK1, tyT2)
      case tt @ TmTApp(t1, tyT2) =>
        val knKT2 = kindof(ctx, tyT2)
        val tyT1 = typeof(ctx, t1)
        simplifyTy(ctx, tyT1) match {
          case TyAll(_, knK11, tyT12) =>
            if (knK11 != knKT2)
              sys.error("type argument has wrong kind")
            else
              typeSubstTop(tyT2, tyT12)
          case z =>
            println(TmTApp(t1, tyT2))
            println(Print.print(PrettyPrinter.ptmATerm(true, ctx, tt), 60))
            println(z)
            sys.error("universal type expected")
        }
      case TmZero =>
        TyNat
      case TmSucc(t1) =>
        if (tyEqv(ctx, typeof(ctx, t1), TyNat)) {
          TyNat
        } else {
          throw new Exception("argument of Succ: is not a number: " + t)
        }
      case TmPred(t1) =>
        if (tyEqv(ctx, typeof(ctx, t1), TyNat)) {
          TyNat
        } else {
          throw new Exception("argument of Pred: is not a number: " + t)
        }
      case TmIsZero(t1) =>
        if (tyEqv(ctx, typeof(ctx, t1), TyNat)) {
          TyBool
        } else {
          throw new Exception("argument of IsZero: is not a number: " + t)
        }
      case TmPack(tyT1, t2, tyT) =>
        checkKindStar(ctx, tyT)
        simplifyTy(ctx, tyT) match {
          case TySome(tyY, k, tyT2) =>
            if (kindof(ctx, tyT1) != k) {
              sys.error("type component doesn't have expected kind")
            }
            val tyU = typeof(ctx, t2)
            val tyU1 = typeSubstTop(tyT1, tyT2)
            if (tyEqv(ctx, tyU, tyU1)) tyT
            else sys.error("doesn't match declared type")
          case _ =>
            sys.error("existential type expected")
        }
      case TmUnPack(tyX, x, t1, t2) =>
        val tyT1 = typeof(ctx, t1)
        simplifyTy(ctx, tyT1) match {
          case TySome(tyY, k, tyT11) =>
            val ctx1 = ctx.addBinding(tyX, TyVarBind(k))
            val ctx2 = ctx1.addBinding(x, VarBind(tyT11))
            val tyT2 = typeof(ctx2, t2)
            typeShift(-2, tyT2)
          case _ =>
            sys.error("existential type expected")
        }
    }
}

class NoRuleApplies(t: Term) extends Exception("No rule applies for term: " + t)
