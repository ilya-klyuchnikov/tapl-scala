package tapl.fullerror

object Util {
  import Term._

  def isVal(ctx: Context, t: Term): Boolean =
    t match {
      case TmTrue         => true
      case TmFalse        => true
      case TmAbs(_, _, _) => true
      case _              => false
    }

}

object Evaluator {
  import Util._
  import Syntax._

  import Binding._
  import Term._

  def eval1(ctx: Context, t: Term): Term =
    t match {
      case TmIf(TmTrue, t2, t3) =>
        t2
      case TmIf(TmFalse, t2, t3) =>
        t3
      case TmVar(n, _) =>
        ctx.getBinding(n) match {
          case TmAbbBind(t1, _) => t1
          case _                => throw new NoRuleApplies(t)
        }
      case TmApp(TmError, t2) =>
        TmError
      case TmApp(v1, TmError) if isVal(ctx, v1) =>
        TmError
      case TmApp(TmAbs(x, ty, t), v2) if isVal(ctx, v2) =>
        termSubstTop(v2, t)
      case TmApp(v1, t2) if isVal(ctx, v1) =>
        val t21 = eval1(ctx, t2)
        TmApp(v1, t21)
      case TmApp(t1, t2) =>
        val t11 = eval1(ctx, t1)
        TmApp(t11, t2)
      case TmIf(TmError, t2, t3) =>
        TmError
      case TmIf(t1, t2, t3) =>
        val t11 = eval1(ctx, t1)
        TmIf(t11, t2, t3)
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
        val t1 = eval(ctx, t)
        TmAbbBind(t1, tyT)
      case b =>
        b
    }
}

object Typer {
  import Syntax._
  import Binding._
  import Term._
  import Ty._

  private def isTyAbb(ctx: Context, i: Int) =
    ctx.getBinding(i) match {
      case TyAbbBind(_) => true
      case _            => false
    }

  private def getTyAbb(ctx: Context, i: Int) =
    ctx.getBinding(i) match {
      case TyAbbBind(ty) => ty
      case _             => throw new NoRuleApplies(null)
    }

  private def computeTy(ctx: Context, tyT: Ty) =
    tyT match {
      case TyVar(i, _) if isTyAbb(ctx, i) =>
        getTyAbb(ctx, i)
      case _ =>
        throw new NoRuleApplies(null)
    }

  def simplifyTy(ctx: Context, ty: Ty): Ty =
    try {
      val tyT1 = computeTy(ctx, ty)
      simplifyTy(ctx, tyT1)
    } catch {
      case _: NoRuleApplies => ty
    }

  def tyEqv(ctx: Context, ty1: Ty, ty2: Ty): Boolean = {
    val tyS = simplifyTy(ctx, ty1)
    val tyT = simplifyTy(ctx, ty2)
    (tyS, tyT) match {
      case (TyTop, TyTop) => true
      case (TyBot, TyBot) => true
      case (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) =>
        tyEqv(ctx, tyS1, tyT1) && tyEqv(ctx, tyS2, tyT2)
      case (TyVar(i, _), _) if isTyAbb(ctx, i) =>
        tyEqv(ctx, getTyAbb(ctx, i), tyT)
      case (_, TyVar(i, _)) if isTyAbb(ctx, i) =>
        tyEqv(ctx, tyS, getTyAbb(ctx, i))
      case (TyVar(i, _), TyVar(j, _)) => i == j
      case (TyBool, TyBool)           => true
      case _                          => false
    }
  }

  def subtype(ctx: Context, ty1: Ty, ty2: Ty): Boolean = {
    if (tyEqv(ctx, ty1, ty2)) {
      return true
    }
    val tyS = simplifyTy(ctx, ty1)
    val tyT = simplifyTy(ctx, ty2)
    (tyS, tyT) match {
      case (_, TyTop) =>
        true
      case (TyBot, _) =>
        true
      case (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) =>
        subtype(ctx, tyT1, tyS1) && subtype(ctx, tyS2, tyT2)
      case (_, _) =>
        false
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
        case (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) =>
          TyArr(meet(ctx, tyS1, tyT1), join(ctx, tyS2, tyT2))
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
        case (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) =>
          TyArr(join(ctx, tyS1, tyT1), meet(ctx, tyS2, tyT2))
        case (_, _) => TyBot
      }
    }

  def typeof(ctx: Context, t: Term): Ty =
    t match {
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
          case TyBot => TyBot
          case z =>
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
      case TmError =>
        TyBot
      case TmTry(t1, t2) =>
        join(ctx, typeof(ctx, t1), typeof(ctx, t2))
    }
}

class NoRuleApplies(t: Term) extends Exception("No rule applies for term: " + t)
