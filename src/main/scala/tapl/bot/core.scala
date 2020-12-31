package tapl.bot

object Util {

  def isVal(ctx: Context, t: Term): Boolean =
    t match {
      case TmAbs(_, _, _) => true
      case _              => false
    }
}

object Evaluator {
  import Util._
  import Syntax._

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
  import Syntax._

  def subtype(tyS: Ty, tyT: Ty): Boolean =
    tyS == tyT ||
      ((tyS, tyT) match {
        case (_, TyTop) => true
        case (TyBot, _) => true
        case (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) =>
          subtype(tyT1, tyS1) && subtype(tyS2, tyT2)
        case (_, _) => false
      })

  def typeof(ctx: Context, t: Term): Ty =
    t match {
      case TmVar(i, _) =>
        ctx.getType(i)
      case TmAbs(v, tyT1, t2) =>
        val ctx1 = ctx.addBinding(v, VarBind(tyT1))
        val tyT2 = typeof(ctx1, t2)
        TyArr(tyT1, tyT2)
      case TmApp(t1, t2) =>
        val tyT1 = typeof(ctx, t1)
        val tyT2 = typeof(ctx, t2)
        tyT1 match {
          case TyArr(tyT11, tyT12) =>
            if (subtype(tyT2, tyT11)) tyT12
            else sys.error("parameter mismatch in " + t + " : " + tyT2 + " != " + tyT11)
          case TyBot => TyBot
          case z     => sys.error("arrow type expected in " + t1)
        }
    }
}

class NoRuleApplies(t: Term) extends Exception("No rule applies for term: " + t)
