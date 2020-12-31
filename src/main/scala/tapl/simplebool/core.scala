package tapl.simplebool

object Evaluator {
  import Util._
  import Syntax._

  def eval1(ctx: Context, t: Term): Term =
    t match {
      case TmApp(TmAbs(x, ty, t), v2) if isVal(ctx, v2) =>
        termSubstTop(v2, t)
      case TmApp(v1, t2) if isVal(ctx, v1) =>
        val t21 = eval1(ctx, t2)
        TmApp(v1, t21)
      case TmApp(t1, t2) =>
        val t11 = eval1(ctx, t1)
        TmApp(t11, t2)
      case TmIf(TmTrue, t2, t3) =>
        t2
      case TmIf(TmFalse, t2, t3) =>
        t3
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
}

object Typer {
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
            if (tyT2 == tyT11)
              tyT12
            else
              throw new Exception("parameter mismatch in " + t + " : " + tyT2 + " != " + tyT11)
          case _ =>
            throw new Exception("error type expected in " + t1)
        }
      case TmTrue =>
        TyBool
      case TmFalse =>
        TyBool
      case TmIf(t1, t2, t3) =>
        if (typeof(ctx, t1) == TyBool) {
          val ty2 = typeof(ctx, t2)
          if (ty2 == typeof(ctx, t3)) {
            ty2
          } else {
            throw new Exception("arms of conditional " + t + " have different types")
          }
        } else {
          throw new Exception("guard of conditional " + t + " is not a boolean")
        }
    }
}

object Util {
  def isVal(ctx: Context, t: Term): Boolean =
    t match {
      case TmAbs(_, _, _) => true
      case TmTrue         => true
      case TmFalse        => true
      case _              => false
    }
}

class NoRuleApplies(t: Term) extends Exception("No rule applies for term: " + t)
