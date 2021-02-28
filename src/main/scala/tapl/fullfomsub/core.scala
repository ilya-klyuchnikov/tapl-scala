package tapl.fullfomsub

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

object Evaluator {
  import Util._
  import Syntax._
  import Binding._
  import Term._

  private def eval1(ctx: Context, t: Term): Term =
    t match {
      case TmApp(TmAbs(x, ty, t), v2) if isVal(ctx, v2) =>
        termSubstTop(v2, t)
      case TmApp(v1, t2) if isVal(ctx, v1) =>
        val t21 = eval1(ctx, t2)
        TmApp(v1, t21)
      case TmApp(t1, t2) =>
        val t11 = eval1(ctx, t1)
        TmApp(t11, t2)
      case TmLet(x, v1, t2) if isVal(ctx, v1) =>
        termSubstTop(v1, t2)
      case TmLet(x, v1, t2) =>
        TmLet(x, eval(ctx, v1), t2)
      case t @ TmFix(v1) if isVal(ctx, v1) =>
        v1 match {
          case TmAbs(_, _, t12) => termSubstTop(t, t12)
          case _                => throw new NoRuleApplies(t)
        }
      case TmFix(t1) =>
        TmFix(eval1(ctx, t1))
      case TmAscribe(v1, tyT) if isVal(ctx, v1) =>
        v1
      case TmAscribe(t1, tyT) =>
        TmAscribe(eval1(ctx, t1), tyT)
      case TmRecord(fields) =>
        def evalAField(l: List[(String, Term)]): List[(String, Term)] =
          l match {
            case Nil                               => throw new NoRuleApplies(t)
            case (l, v1) :: rest if isVal(ctx, v1) => (l, v1) :: evalAField(rest)
            case (l, t1) :: rest                   => (l, eval1(ctx, t1)) :: rest
          }
        TmRecord(evalAField(fields))
      case TmProj(v1 @ TmRecord(fields), l) if isVal(ctx, v1) =>
        fields.find { _._1 == l } match {
          case Some((_, ti)) => ti
          case None          => throw new NoRuleApplies(t)
        }
      case TmProj(t1, l) =>
        TmProj(eval1(ctx, t1), l)
      case TmIf(TmTrue, t2, t3) =>
        t2
      case TmIf(TmFalse, t2, t3) =>
        t3
      case TmIf(t1, t2, t3) =>
        val t11 = eval1(ctx, t1)
        TmIf(t11, t2, t3)
      case TmVar(n, _) =>
        ctx.getBinding(n) match {
          case TmAbbBind(t1, _) => t1
          case _                => throw new NoRuleApplies(t)
        }
      case TmSucc(t1) =>
        val t11 = eval1(ctx, t1)
        TmSucc(t11)
      case TmPred(TmZero) =>
        TmZero
      case TmPred(TmSucc(nv1)) if isNumericVal(ctx, nv1) =>
        nv1
      case TmPred(t1) =>
        val t2 = eval1(ctx, t1)
        TmPred(t2)
      case TmIsZero(TmZero) =>
        TmTrue
      case TmIsZero(TmSucc(nv1)) if isNumericVal(ctx, nv1) =>
        TmFalse
      case TmIsZero(t1) =>
        val t2 = eval(ctx, t1)
        TmIsZero(t2)
      case TmUnPack(_, _, TmPack(tyT11, v12, _), t2) if isVal(ctx, v12) =>
        tyTermSubstTop(tyT11, termSubstTop(termShift(1, v12), t2))
      case TmUnPack(tyX, x, t1, t2) =>
        TmUnPack(tyX, x, eval1(ctx, t1), t2)
      case TmTApp(TmTAbs(x, _, t11), tyT2) =>
        tyTermSubstTop(tyT2, t11)
      case TmTApp(t1, tyT2) =>
        TmTApp(eval1(ctx, t1), tyT2)
      case _ =>
        throw new NoRuleApplies(t)
    }

  def eval(ctx: Context, t: Term): Term =
    try {
      val t1 = eval1(ctx, t)
      eval(ctx, t1)
    } catch {
      case _: NoRuleApplies => t
    }

  def evalBinding(ctx: Context, bind: Binding): Binding =
    bind match {
      case TmAbbBind(t, tyT) =>
        TmAbbBind(eval(ctx, t), tyT)
      case b =>
        b
    }
}

object Typer {
  import Binding._
  import Kind._
  import Term._
  import Ty._
  import Syntax._

  private def promote(ctx: Context, t: Ty): Ty =
    t match {
      case TyVar(i, _) =>
        ctx.getBinding(i) match {
          case TyVarBind(tyT) => tyT
          case _              => throw new NoRuleApplies(null)
        }
      case _ => throw new NoRuleApplies(null)
    }

  def lcst(ctx: Context, tyS: Ty): Ty = {
    val tyS1 = simplifyTy(ctx, tyS)
    try {
      lcst(ctx, promote(ctx, tyS1))
    } catch {
      case _: NoRuleApplies => tyS1
    }
  }

  private def isTyAbb(ctx: Context, i: Int) =
    ctx.getBinding(i) match {
      case TyAbbBind(tyT, _) => true
      case _                 => false
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
      val ty1 = computeTy(ctx, tyT)
      simplifyTy(ctx, ty1)
    } catch {
      case _: NoRuleApplies => tyT
    }
  }

  def tyEqv(ctx: Context, ty1: Ty, ty2: Ty): Boolean = {
    val tyS = simplifyTy(ctx, ty1)
    val tyT = simplifyTy(ctx, ty2)
    (tyS, tyT) match {
      case (TyString, TyString) => true
      case (TyTop, TyTop)       => true
      case (TyUnit, TyUnit)     => true
      case (TyId(b1), TyId(b2)) => b1 == b2
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
      case (TySome(tyX1, tyS1, tyS2), TySome(_, tyT1, tyT2)) =>
        tyEqv(ctx.addName(tyX1), tyS1, tyT1) && tyEqv(ctx.addName(tyX1), tyS2, tyT2)
      case (TyAll(tyX1, tyS1, tyS2), TyAll(_, tyT1, tyT2)) =>
        tyEqv(ctx.addName(tyX1), tyS1, tyT1) && tyEqv(ctx.addName(tyX1), tyS2, tyT2)
      case (TyAbs(tyX1, knKS1, tyS2), TyAbs(_, knKT1, tyT2)) =>
        (knKS1 == knKT1) && tyEqv(ctx.addName(tyX1), tyS2, tyT2)
      case (TyApp(tyS1, tyS2), TyApp(tyT1, tyT2)) =>
        tyEqv(ctx, tyS1, tyT1) && tyEqv(ctx, tyS2, tyT2)
      case _ => false
    }
  }

  def getKind(ctx: Context, i: Int): Kind =
    ctx.getBinding(i) match {
      case TyVarBind(tyT)          => kindof(ctx, tyT)
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
        val ctx1 = ctx.addBinding(tyX, TyVarBind(makeTop(knK1)))
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

  def subtype(ctx: Context, ty1: Ty, ty2: Ty): Boolean = {
    if (tyEqv(ctx, ty1, ty2)) {
      return true
    }
    val tyS = simplifyTy(ctx, ty1)
    val tyT = simplifyTy(ctx, ty2)
    (tyS, tyT) match {
      case (_, TyTop) => true
      case (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) =>
        subtype(ctx, tyT1, tyS1) && subtype(ctx, tyS2, tyT2)
      case (TyRecord(fS), TyRecord(fT)) =>
        fT.forall {
          case (li, tyTi) =>
            fS.find { _._1 == li } match {
              case Some((_, tySi)) => subtype(ctx, tySi, tyTi)
              case None            => false
            }
        }
      case (TyVar(_, _), _) =>
        subtype(ctx, promote(ctx, tyS), tyT)
      case (TyAll(tyX1, tyS1, tyS2), TyAll(_, tyT1, tyT2)) =>
        subtype(ctx, tyS1, tyT1) && subtype(ctx, tyT1, tyS1) && subtype(
          ctx.addBinding(tyX1, TyVarBind(tyT1)),
          tyS2,
          tyT2,
        )
      case (TySome(tyX1, tyS1, tyS2), TySome(_, tyT1, tyT2)) =>
        subtype(ctx, tyS1, tyT1) && subtype(ctx, tyT1, tyS1) && subtype(
          ctx.addBinding(tyX1, TyVarBind(tyT1)),
          tyS2,
          tyT2,
        )
      case (TyAbs(tyX1, knKS1, tyS2), TyAbs(_, knKT1, tyT2)) =>
        (knKS1 == knKT1) && subtype(ctx.addName(tyX1), tyS2, tyT2)
      case (TyApp(_, _), _) =>
        subtype(ctx, promote(ctx, tyS), tyT)
      case (_, _) => false
    }

  }

  def join(ctx: Context, ty1: Ty, ty2: Ty): Ty =
    if (subtype(ctx, ty1, ty2)) {
      ty2
    } else if (subtype(ctx, ty2, ty1)) {
      ty1
    } else {
      val tyS = simplifyTy(ctx, ty1)
      val tyT = simplifyTy(ctx, ty2)
      (tyS, tyT) match {
        case (TyRecord(fS), TyRecord(fT)) =>
          val labelS = fS.map { _._1 }
          val labelT = fT.map { _._1 }
          val commonLabels = labelS intersect labelT
          val commonFs = commonLabels.map { li =>
            val (_, tySi) = fS.find { _._1 == li }.get
            val (_, tyTi) = fT.find { _._1 == li }.get
            (li, join(ctx, tySi, tyTi))
          }
          TyRecord(commonFs)
        case (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) =>
          try { TyArr(meet(ctx, tyS1, tyT1), join(ctx, tyS2, tyT2)) }
          catch { case _: NotFound => TyTop }
        case (TyAll(tyX1, tyS1, tyS2), TyAll(_, tyT1, tyT2)) =>
          if (!(subtype(ctx, tyS1, tyT1) && subtype(ctx, tyT1, tyS1))) {
            TyTop
          } else {
            val ctx1 = ctx.addBinding(tyX1, TyVarBind(tyT1))
            TyAll(tyX1, tyS1, join(ctx1, tyT1, tyT2))
          }
        case _ => TyTop
      }
    }

  def meet(ctx: Context, ty1: Ty, ty2: Ty): Ty =
    if (subtype(ctx, ty1, ty2)) {
      ty1
    } else if (subtype(ctx, ty2, ty1)) {
      ty2
    } else {
      val tyS = simplifyTy(ctx, ty1)
      val tyT = simplifyTy(ctx, ty2)
      (tyS, tyT) match {
        case (TyRecord(fS), TyRecord(fT)) =>
          val labelS = fS.map { _._1 }
          val labelT = fT.map { _._1 }
          val allLabels = (labelS concat labelT).distinct
          // there was an error by Pierce!!
          val allFs = allLabels.flatMap { li =>
            (fS.find { _._1 == li }, fT.find { _._1 == li }) match {
              case (Some((_, tySi)), Some((_, tyTi))) =>
                Some(li -> meet(ctx, tySi, tyTi))
              case (Some((_, tySi)), _) =>
                Some(li -> tySi)
              case (_, Some((_, tySi))) =>
                Some(li -> tySi)
              case (None, None) =>
                None
            }
          }
          TyRecord(allFs)
        case (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) =>
          TyArr(join(ctx, tyS1, tyT1), meet(ctx, tyS2, tyT2))
        case (TyAll(tyX1, tyS1, tyS2), TyAll(_, tyT1, tyT2)) =>
          if (!(subtype(ctx, tyS1, tyT1) && subtype(ctx, tyT1, tyS1))) {
            throw new NotFound()
          } else {
            val ctx1 = ctx.addBinding(tyX1, TyVarBind(tyT1))
            TyAll(tyX1, tyS1, meet(ctx1, tyT1, tyT2))
          }
        case _ => throw new NotFound()
      }
    }

  def typeof(ctx: Context, t: Term): Ty =
    t match {
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
        lcst(ctx, tyT1) match {
          case TyArr(tyT11, tyT12) =>
            if (subtype(ctx, tyT2, tyT11))
              tyT12
            else
              throw new Exception("parameter mismatch in " + t + " : " + tyT2 + " != " + tyT11)
          case z =>
            println(z)
            throw new Exception("arrow type expected in " + t1)
        }
      case TmLet(x, t1, t2) =>
        val tyT1 = typeof(ctx, t1)
        val ctx1 = ctx.addBinding(x, VarBind(tyT1))
        typeShift(-1, typeof(ctx1, t2))
      case TmFix(t1) =>
        val tyT1 = typeof(ctx, t1)
        lcst(ctx, tyT1) match {
          case TyArr(tyT11, tyT12) =>
            if (subtype(ctx, tyT12, tyT11))
              tyT12
            else
              throw new Exception(
                "result of body not compatible with domain " + t + " : " + tyT12 + " != " + tyT11
              )
          case _ =>
            throw new Exception("error type expected in " + t1)
        }
      case TmString(_) =>
        TyString
      case TmUnit =>
        TyUnit
      case TmAscribe(t1, tyT) =>
        checkKindStar(ctx, tyT)
        if (subtype(ctx, typeof(ctx, t1), tyT))
          tyT
        else
          throw new Exception("body of as-term doesn't have the expected type in " + t)
      case TmRecord(fields) =>
        val fieldTys = fields.map { case (li, ti) => (li, typeof(ctx, ti)) }
        TyRecord(fieldTys)
      case TmProj(t1, l) =>
        lcst(ctx, typeof(ctx, t1)) match {
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
        if (subtype(ctx, typeof(ctx, t1), TyBool)) {
          join(ctx, typeof(ctx, t2), typeof(ctx, t3))
        } else {
          throw new Exception("guard of conditional " + t + " is not a boolean")
        }
      case TmZero =>
        TyNat
      case TmSucc(t1) =>
        if (subtype(ctx, typeof(ctx, t1), TyNat)) {
          TyNat
        } else {
          throw new Exception("argument of Succ: is not a number: " + t)
        }
      case TmPred(t1) =>
        if (subtype(ctx, typeof(ctx, t1), TyNat)) {
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
          case TySome(tyY, tyBound, tyT2) =>
            if (!subtype(ctx, tyT1, tyBound)) {
              sys.error("hidden type not a subtype of bound")
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
        lcst(ctx, tyT1) match {
          case TySome(tyY, tyBound, tyT11) =>
            val ctx1 = ctx.addBinding(tyX, TyVarBind(tyBound))
            val ctx2 = ctx1.addBinding(x, VarBind(tyT11))
            val tyT2 = typeof(ctx2, t2)
            typeShift(-2, tyT2)
          case _ =>
            sys.error("existential type expected")
        }
      case TmTAbs(tyX, tyT1, t2) =>
        val ctx1 = ctx.addBinding(tyX, TyVarBind(tyT1))
        val tyT2 = typeof(ctx1, t2)
        TyAll(tyX, tyT1, tyT2)
      case TmTApp(t1, tyT2) =>
        val tyT1 = typeof(ctx, t1)
        lcst(ctx, tyT1) match {
          case TyAll(_, tyT11, tyT12) =>
            if (!subtype(ctx, tyT2, tyT11)) {
              sys.error("hidden type not a subtype of bound")
            }
            typeSubstTop(tyT2, tyT12)
          case _ =>
            sys.error("universal type expected")
        }
    }
}

class NoRuleApplies(t: Term) extends Exception("No rule applies for term: " + t)
class NotFound extends Exception
