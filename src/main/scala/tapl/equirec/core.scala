package tapl.equirec

object Util {
  import Term._

  def isVal(ctx: Context, t: Term): Boolean =
    t match {
      case TmAbs(_, _, _) => true
      case _              => false
    }
}

object Evaluator {
  import Util._
  import Syntax._
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
}

object Typer {
  import Binding._
  import Syntax._
  import Term._
  import Ty._

  private def computeTy(ctx: Context, tyT: Ty) =
    tyT match {
      case TyRec(x, tyS1) => typeSubstTop(tyT, tyS1)
      case _              => throw new NoRuleApplies(null)
    }

  def simplifyTy(ctx: Context, ty: Ty): Ty =
    try {
      val ty1 = computeTy(ctx, ty)
      simplifyTy(ctx, ty1)
    } catch {
      case _: NoRuleApplies => ty
    }

  // the huge change here
  private def tyEqv(seen: List[(Ty, Ty)], ctx: Context, tyS: Ty, tyT: Ty): Boolean = {
    seen.contains((tyS, tyT)) || ((tyS, tyT) match {
      case (TyRec(x, tyS1), _) =>
        tyEqv((tyS, tyT) :: seen, ctx, typeSubstTop(tyS, tyS1), tyT)
      case (_, TyRec(x, tyT1)) =>
        tyEqv((tyS, tyT) :: seen, ctx, tyS, typeSubstTop(tyT, tyT1))
      case (TyId(b1), TyId(b2)) => b1 == b2
      case (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) =>
        tyEqv(seen, ctx, tyS1, tyT1) && tyEqv(seen, ctx, tyS2, tyT2)
      case _ => false
    })
  }

  def tyEqv(ctx: Context, tyS: Ty, tyT: Ty): Boolean =
    tyEqv(List(), ctx, tyS, tyT)

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
            if (tyEqv(ctx, tyT2, tyT11))
              tyT12
            else
              throw new Exception("parameter mismatch in " + t + " : " + tyT2 + " != " + tyT11)
          case z =>
            println(z)
            throw new Exception("arrow type expected in " + t1)
        }
    }
}

class NoRuleApplies(t: Term) extends Exception("No rule applies for term: " + t)
