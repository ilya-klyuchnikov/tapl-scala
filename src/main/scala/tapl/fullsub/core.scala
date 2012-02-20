package tapl.fullsub

object Util {

  def isNumericVal(ctx: Context, t: Term): Boolean = t match {
    case TmZero     => true
    case TmSucc(t1) => isNumericVal(ctx, t1)
    case _          => false
  }

  def isVal(ctx: Context, t: Term): Boolean = t match {
    case TmTrue                      => true
    case TmFalse                     => true
    case TmString(_)                 => true
    case TmUnit                      => true
    case t1 if isNumericVal(ctx, t1) => true
    case TmAbs(_, _, _)              => true
    case TmRecord(fields)            => fields.forall { case (_, ti) => isVal(ctx, ti) }
    case _                           => false
  }
}

object Evaluator {
  import Util._
  import Syntax._

  private def eval1(ctx: Context, t: Term): Term = t match {
    case TmIf(TmTrue, t2, t3) =>
      t2
    case TmIf(TmFalse, t2, t3) =>
      t3
    case TmIf(t1, t2, t3) =>
      val t11 = eval1(ctx, t1)
      TmIf(t11, t2, t3)
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
    case TmVar(n, _) =>
      ctx.getBinding(n) match {
        case TmAbbBind(t1, _) => t1
        case _                => throw new NoRuleApplies(t)
      }
    case TmAscribe(v1, tyT) if isVal(ctx, v1) =>
      v1
    case TmAscribe(t1, tyT) =>
      TmAscribe(eval1(ctx, t1), tyT)
    case TmRecord(fields) =>
      def evalAField(l: List[(String, Term)]): List[(String, Term)] = l match {
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

  def evalBinding(ctx: Context, bind: Binding): Binding = bind match {
    case TmAbbBind(t, tyT) =>
      TmAbbBind(eval(ctx, t), tyT)
    case b =>
      b
  }
}

object Typer {
  import Syntax._

  private def isTyAbb(ctx: Context, i: Int) = ctx.getBinding(i) match {
    case TyAbbBind(_) => true
    case _            => false
  }

  private def getTyAbb(ctx: Context, i: Int) = ctx.getBinding(i) match {
    case TyAbbBind(ty) => ty
    case _             => throw new NoRuleApplies(null)
  }

  private def computeTy(ctx: Context, tyT: Ty) = tyT match {
    case TyVar(i, _) if isTyAbb(ctx, i) =>
      getTyAbb(ctx, i)
    case _ =>
      throw new NoRuleApplies(null)
  }

  def simplifyTy(ctx: Context, ty: Ty): Ty =
    try {
      val ty1 = computeTy(ctx, ty)
      simplifyTy(ctx, ty1)
    } catch {
      case _: NoRuleApplies => ty
    }

  def tyEqv(ctx: Context, ty1: Ty, ty2: Ty): Boolean = {
    val tyS = simplifyTy(ctx, ty1)
    val tyT = simplifyTy(ctx, ty2)
    (tyS, tyT) match {
      case (TyString, TyString) => true
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
          case (li2, tyTi2) => fields1.find { _._1 == li2 } match {
            case Some((li1, tyTi1)) => tyEqv(ctx, tyTi1, tyTi2)
            case None               => false
          }
        }
      case _ => false
    }
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
          val allLabels = (labelS union labelT).distinct
          // there was an error by Pierce!!
          val allFs = allLabels.map { li =>
            (fS.find { _._1 == li }, fT.find { _._1 == li }) match {
              case (Some((_, tySi)), Some((_, tyTi))) =>
                (li, meet(ctx, tySi, tyTi))
              case (Some((_, tySi)), _) =>
                (li, tySi)
              case (_, Some((_, tySi))) =>
                (li, tySi)
            }
          }
          TyRecord(allFs)
        case (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) =>
          TyArr(join(ctx, tyS1, tyT1), meet(ctx, tyS2, tyT2))
        case _ => throw new NotFound()
      }
    }

  def typeof(ctx: Context, t: Term): Ty = t match {
    case TmInert(ty) =>
      ty
    case TmVar(i, _) =>
      ctx.getType(i)
    case TmAbs(v, tyT1, t2) =>
      val ctx1 = ctx.addBinding(v, VarBind(tyT1))
      val tyT2 = typeof(ctx1, t2)
      TyArr(tyT1, typeShift(-1, tyT2))
    case TmApp(t1, t2) =>
      val tyT1 = typeof(ctx, t1)
      val tyT2 = typeof(ctx, t2)
      simplifyTy(ctx, tyT1) match {
        case TyArr(tyT11, tyT12) =>
          if (subtype(ctx, tyT2, tyT11))
            tyT12
          else
            throw new Exception("parameter mismatch in " + t + " : " + tyT2 + " != " + tyT11)
        case z =>
          println(z)
          throw new Exception("arrow type expected in " + t1)
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
    case TmLet(x, t1, t2) =>
      val tyT1 = typeof(ctx, t1)
      val ctx1 = ctx.addBinding(x, VarBind(tyT1))
      typeShift(-1, typeof(ctx1, t2))
    case TmFix(t1) =>
      val tyT1 = typeof(ctx, t1)
      simplifyTy(ctx, tyT1) match {
        case TyArr(tyT11, tyT12) =>
          if (subtype(ctx, tyT12, tyT11))
            tyT12
          else
            throw new Exception("result of body not compatible with domain " + t + " : " + tyT12 + " != " + tyT11)
        case _ =>
          throw new Exception("error type expected in " + t1)
      }
    case TmString(_) =>
      TyString
    case TmUnit =>
      TyUnit
    case TmAscribe(t1, tyT) =>
      if (subtype(ctx, typeof(ctx, t1), tyT))
        tyT
      else
        throw new Exception("body of as-term doesn't have the expected type in " + t)
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
      if (subtype(ctx, typeof(ctx, t1), TyNat)) {
        TyBool
      } else {
        throw new Exception("argument of IsZero: is not a number: " + t)
      }
  }
}

class NoRuleApplies(t: Term) extends Exception("No rule applies for term: " + t)
class NotFound extends Exception