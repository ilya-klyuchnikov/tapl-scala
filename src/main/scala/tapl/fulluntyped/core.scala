package tapl.fulluntyped

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
      case TmTrue                      => true
      case TmFalse                     => true
      case TmString(_)                 => true
      case t1 if isNumericVal(ctx, t1) => true
      case TmAbs(_, _)                 => true
      case TmRecord(fields)            => fields.forall { case (_, ti) => isVal(ctx, ti) }
      case _                           => false
    }
}

object Evaluator {
  import Util._
  import Syntax._
  import Binding._
  import Term._

  private def eval1(ctx: Context, t: Term): Term =
    t match {
      case TmIf(TmTrue, t2, t3) =>
        t2
      case TmIf(TmFalse, t2, t3) =>
        t3
      case TmIf(t1, t2, t3) =>
        val t11 = eval1(ctx, t1)
        TmIf(t11, t2, t3)
      case TmApp(TmAbs(x, t), v2) if isVal(ctx, v2) =>
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
      case TmVar(n, _) =>
        ctx.getBinding(n) match {
          case TmAbbBind(t1) => t1
          case _             => throw new NoRuleApplies(t)
        }
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

  def evalBinding(ctx: Context, bind: Binding): Binding =
    bind match {
      case TmAbbBind(t) =>
        TmAbbBind(eval(ctx, t))
      case b =>
        b
    }
}

class NoRuleApplies(t: Term) extends Exception("No rule applies for term: " + t)
