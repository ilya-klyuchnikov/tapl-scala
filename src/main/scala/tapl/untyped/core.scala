package tapl.untyped

object Evaluator {
  import Util._
  import Syntax._

  def eval1(ctx: Context, t: Term): Term = t match {
    case TmApp(TmAbs(x, t1), v2) if isVal(ctx, v2) =>
      termSubstTop(v2, t1)
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

object Util {
  def isVal(ctx: Context, t: Term): Boolean = t match {
    case TmAbs(_, _) => true
    case _           => false
  }
}

class NoRuleApplies(t: Term) extends Exception("No rule applies for term: " + t)