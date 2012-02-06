package tapl.fullpoly

object Util {

  def isNumericVal(ctx: Context, t: Term): Boolean = t match {
    case TmZero     => true
    case TmSucc(t1) => isNumericVal(ctx, t1)
    case _          => false
  }

  def isVal(ctx: Context, t: Term): Boolean = t match {
    case TmString(_)                        => true
    case TmUnit                             => true
    case TmTrue                             => true
    case TmFalse                            => true
    case t1 if isNumericVal(ctx, t1)        => true
    case TmAbs(_, _, _)                     => true
    case TmRecord(fields)                   => fields.forall { case (_, ti) => isVal(ctx, ti) }
    case TmPack(_, v1, _) if isVal(ctx, v1) => true
    case TmTAbs(_, _)                       => true
    case _                                  => false
  }
}

object Evaluator {
  import Util._
  import Syntax._

  private def eval1(ctx: Context, t: Term): Term = t match {
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
      TmAscribe(eval(ctx, t1), tyT)
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
    case TmTApp(TmTAbs(x, t11), tyT2) =>
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
      case (TySome(tyX1, tyS2), TySome(_, tyT2)) =>
        tyEqv(ctx.addName(tyX1), tyS2, tyT2)
      case (TyAll(tyX1, tyS2), TyAll(_, tyT2)) =>
        tyEqv(ctx.addName(tyX1), tyS2, tyT2)
      case _ => false
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
          if (tyEqv(ctx, tyT2, tyT11))
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
      simplifyTy(ctx, tyT1) match {
        case TyArr(tyT11, tyT12) =>
          if (tyEqv(ctx, tyT12, tyT11))
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
      if (tyEqv(ctx, typeof(ctx, t1), tyT))
        tyT
      else
        throw new Exception("body of as-term doesn't have the expected type in " + t)
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
      simplifyTy(ctx, tyT) match {
        case TySome(tyY, tyT2) =>
          val tyU = typeof(ctx, t2)
          val tyU1 = typeSubstTop(tyT1, tyT2)
          if (tyEqv(ctx, tyU, tyU1)) tyT
          else error("doesn't match declared type")
        case _ =>
          error("existential type expected")
      }
    case TmUnPack(tyX, x, t1, t2) =>
      val tyT1 = typeof(ctx, t1)
      simplifyTy(ctx, tyT1) match {
        case TySome(tyY, tyT11) =>
          val ctx1 = ctx.addBinding(tyX, TyVarBind)
          val ctx2 = ctx1.addBinding(x, VarBind(tyT11))
          val tyT2 = typeof(ctx2, t2)
          typeShift(-2, tyT2)
        case _ =>
          error("existential type expected")
      }
    case TmTAbs(tyX, t2) =>
      val ctx1 = ctx.addBinding(tyX, TyVarBind)
      val tyT2 = typeof(ctx1, t2)
      TyAll(tyX, tyT2)
    case TmTApp(t1, tyT2) =>
      val tyT1 = typeof(ctx, t1)
      simplifyTy(ctx, tyT1) match {
        case TyAll(_, tyT12) =>
          typeSubstTop(tyT2, tyT12)
        case _ =>
          error("universal type expected")
      }
  }
}

class NoRuleApplies(t: Term) extends Exception("No rule applies for term: " + t)