package tapl.rcdsubbot

object Util {
  import Term._

  def isVal(ctx: Context, t: Term): Boolean =
    t match {
      case TmAbs(_, _, _)   => true
      case TmRecord(fields) => fields.forall { case (_, ti) => isVal(ctx, ti) }
      case _                => false
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
  import Binding._
  import Term._
  import Ty._

  def subtype(tyS: Ty, tyT: Ty): Boolean =
    tyS == tyT ||
      ((tyS, tyT) match {
        case (_, TyTop) => true
        case (TyBot, _) => true
        case (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) =>
          subtype(tyT1, tyS1) && subtype(tyS2, tyT2)
        case (TyRecord(fS), TyRecord(fT)) =>
          fT.forall {
            case (li2, tyTi) =>
              fS.find { _._1 == li2 } match {
                case Some((_, tySi)) => subtype(tySi, tyTi)
                case None            => false
              }
          }
        case _ => false
      })

  def typeof(ctx: Context, t: Term): Ty =
    t match {
      case TmRecord(fields) =>
        val fieldTys = fields.map { case (li, ti) => (li, typeof(ctx, ti)) }
        TyRecord(fieldTys)
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
            if (subtype(tyT2, tyT11))
              tyT12
            else
              sys.error("parameter mismatch in " + t + " : " + tyT2 + " != " + tyT11)
          case TyBot =>
            TyBot
          case z =>
            sys.error("arrow type expected in " + t1)
        }
      case TmProj(t1, l) =>
        typeof(ctx, t1) match {
          case TyRecord(fieldTys) =>
            fieldTys find { _._1 == l } match {
              case Some((_, tyi)) => tyi
              case None           => throw new Exception("Label " + l + " not found in " + t)
            }
          case TyBot => TyBot
          case _     => throw new Exception("Expected record type for " + t1)
        }
    }
}

class NoRuleApplies(t: Term) extends Exception("No rule applies for term: " + t)
