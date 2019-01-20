package tapl.arith

// Small-step semantics as described by Pierce
object Evaluator {
  import Util._

  private def eval1(t: Term): Term = t match {
    case TmIf(TmTrue, t2, t3) =>
      t2
    case TmIf(TmFalse, t2, t3) =>
      t3
    case TmIf(t1, t2, t3) =>
      val t11 = eval1(t1)
      TmIf(t11, t2, t3)
    case TmSucc(t1) =>
      val t11 = eval1(t1)
      TmSucc(t11)
    case TmPred(TmZero) =>
      TmZero
    case TmPred(TmSucc(nv1)) if isNumericVal(nv1) =>
      nv1
    case TmPred(t1) =>
      val t2 = eval1(t1)
      TmPred(t2)
    case TmIsZero(TmZero) =>
      TmTrue
    case TmIsZero(TmSucc(nv1)) if isNumericVal(nv1) =>
      TmFalse
    case TmIsZero(t1) =>
      val t2 = eval(t1)
      TmIsZero(t2)
    case _ => throw new NoRuleApplies(t)
  }

  def eval(t: Term): Term =
    try {
      val t1 = eval1(t)
      eval(t1)
    } catch {
      case _: NoRuleApplies if isVal(t) => t
      case _: NoRuleApplies             => throw new NoRuleApplies(t)
    }

}

// This is solution to the Exercise 3.5.17
object BigStepEvaluator {
  import Util._

  def eval(t: Term): Term = t match {
    case t if isVal(t) =>
      t
    case TmIf(t1, t2, t3) =>
      eval(t1) match {
        case TmTrue =>
          eval(t2)
        case TmFalse =>
          eval(t3)
        case _ => throw new NoRuleApplies(t)
      }
    case TmSucc(t1) =>
      val t2 = eval(t1)
      if (isNumericVal(t2)) {
        TmSucc(t2)
      } else {
        throw new NoRuleApplies(t)
      }
    case TmPred(t1) =>
      eval(t1) match {
        case TmZero =>
          TmZero
        case TmSucc(t2) if isNumericVal(t2) =>
          t2
        case _ => throw new NoRuleApplies(t)
      }
    case TmIsZero(t1) =>
      eval(t1) match {
        case TmZero =>
          TmTrue
        case TmSucc(t2) if isNumericVal(t2) =>
          TmFalse
        case _ => throw new NoRuleApplies(t)
      }
    case _ => throw new NoRuleApplies(t)
  }
}

object Util {
  def isNumericVal(t: Term): Boolean = t match {
    case TmZero     => true
    case TmSucc(t1) => isNumericVal(t1)
    case _          => false
  }

  def isVal(t: Term): Boolean = t match {
    case TmTrue               => true
    case TmFalse              => true
    case t if isNumericVal(t) => true
    case _                    => false
  }
}

class NoRuleApplies(t: Term) extends Exception("No rule applies for term: " + t)