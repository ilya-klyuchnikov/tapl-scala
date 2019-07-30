package tapl.fullref

object Util {

  def isNumericVal(ctx: Context, t: Term): Boolean = t match {
    case TmZero     => true
    case TmSucc(t1) => isNumericVal(ctx, t1)
    case _          => false
  }

  def isVal(ctx: Context, t: Term): Boolean = t match {
    case TmString(_)                 => true
    case TmUnit                      => true
    case TmTag(_, t1, _)             => isVal(ctx, t1)
    case TmLoc(_)                    => true
    case TmTrue                      => true
    case TmFalse                     => true
    case t1 if isNumericVal(ctx, t1) => true
    case TmAbs(_, _, _)              => true
    case TmRecord(fields)            => fields.forall { case (_, ti) => isVal(ctx, ti) }
    case _                           => false
  }

}

case class Store(l: List[Term] = List()) {
  import Syntax._

  def extend(v: Term): (Int, Store) =
    (l.length, Store(l :+ v))
  def lookup(i: Int): Term =
    l(i)
  def update(n: Int, v: Term): Store =
    Store(l.updated(n, v))
  def shift(i: Int): Store =
    Store(l.map(termShift(i, _)))
}

object Evaluator {
  import Util._
  import Syntax._

  private def eval1(ctx: Context, store: Store, t: Term): (Term, Store) = t match {
    case TmAscribe(v1, tyT) if isVal(ctx, v1) =>
      (v1, store)
    case TmAscribe(t1, tyT) =>
      val (t11, store1) = eval1(ctx, store, t1)
      (TmAscribe(t11, tyT), store1)
    case TmApp(TmAbs(x, ty, t), v2) if isVal(ctx, v2) =>
      (termSubstTop(v2, t), store)
    case TmApp(v1, t2) if isVal(ctx, v1) =>
      val (t21, store1) = eval1(ctx, store, t2)
      (TmApp(v1, t21), store1)
    case TmApp(t1, t2) =>
      val (t11, store1) = eval1(ctx, store, t1)
      (TmApp(t11, t2), store1)
    case TmRecord(fields) =>
      def evalAField(l: List[(String, Term)]): (List[(String, Term)], Store) = l match {
        case Nil =>
          throw new NoRuleApplies(t)
        case (l, vi) :: rest if isVal(ctx, vi) =>
          val (rest1, store1) = evalAField(rest)
          ((l, vi) :: rest1, store1)
        case (l, ti) :: rest =>
          val (ti1, store1) = eval1(ctx, store, ti)
          ((l, ti1) :: rest, store1)
      }
      val (fields1, store1) = evalAField(fields)
      (TmRecord(fields1), store1)
    case TmProj(v1 @ TmRecord(fields), l) if isVal(ctx, v1) =>
      fields.find { _._1 == l } match {
        case Some((_, ti)) => (ti, store)
        case None          => throw new NoRuleApplies(t)
      }
    case TmProj(t1, l) =>
      val (t11, store1) = eval1(ctx, store, t1)
      (TmProj(t11, l), store1)
    case TmTag(l, t1, tyT) =>
      val (t11, store1) = eval1(ctx, store, t1)
      (TmTag(l, t11, tyT), store1)
    case TmCase(TmTag(li, v11, _), bs) if isVal(ctx, v11) =>
      bs find { _._1 == li } match {
        case Some((_, x, body)) => (termSubstTop(v11, body), store)
        case None               => throw new NoRuleApplies(t)
      }
    case TmCase(t1, bs) =>
      val (t11, store1) = eval1(ctx, store, t1)
      (TmCase(t11, bs), store1)
    case TmRef(t1) =>
      if (!isVal(ctx, t1)) {
        val (t11, store1) = eval1(ctx, store, t1)
        (TmRef(t11), store1)
      } else {
        val (l, store1) = store.extend(t1)
        (TmLoc(l), store1)
      }
    case TmDeref(t1) =>
      if (!isVal(ctx, t1)) {
        val (t11, store1) = eval1(ctx, store, t1)
        (TmDeref(t11), store1)
      } else {
        t1 match {
          case TmLoc(l) => (store.lookup(l), store)
          case _        => throw new NoRuleApplies(t)
        }
      }
    case TmAssign(t1, t2) =>
      if (!isVal(ctx, t1)) {
        val (t11, store1) = eval1(ctx, store, t1)
        (TmAssign(t11, t2), store1)
      } else if (!isVal(ctx, t2)) {
        val (t21, store1) = eval1(ctx, store, t2)
        (TmAssign(t1, t21), store1)
      } else {
        t1 match {
          case TmLoc(l) => (TmUnit, store.update(l, t2))
          case _        => throw new NoRuleApplies(t)
        }
      }
    case TmLet(x, v1, t2) if isVal(ctx, v1) =>
      (termSubstTop(v1, t2), store)
    case TmLet(x, t1, t2) =>
      val (t11, store1) = eval1(ctx, store, t1)
      (TmLet(x, t11, t2), store1)
    case TmIf(TmTrue, t2, t3) =>
      (t2, store)
    case TmIf(TmFalse, t2, t3) =>
      (t3, store)
    case TmIf(t1, t2, t3) =>
      val (t11, store1) = eval1(ctx, store, t1)
      (TmIf(t11, t2, t3), store1)
    case TmSucc(t1) =>
      val (t11, store1) = eval1(ctx, store, t1)
      (TmSucc(t11), store1)
    case TmPred(TmZero) =>
      (TmZero, store)
    case TmPred(TmSucc(nv1)) if isNumericVal(ctx, nv1) =>
      (nv1, store)
    case TmPred(t1) =>
      val (t2, store1) = eval1(ctx, store, t1)
      (TmPred(t2), store)
    case TmIsZero(TmZero) =>
      (TmTrue, store)
    case TmIsZero(TmSucc(nv1)) if isNumericVal(ctx, nv1) =>
      (TmFalse, store)
    case TmIsZero(t1) =>
      val (t2, store1) = eval1(ctx, store, t1)
      (TmIsZero(t2), store)
    case t @ TmFix(v1) if isVal(ctx, v1) =>
      v1 match {
        case TmAbs(_, _, t12) => (termSubstTop(t, t12), store)
        case _                => throw new NoRuleApplies(t)
      }
    case TmFix(t1) =>
      val (t2, store1) = eval1(ctx, store, t1)
      (TmFix(t2), store1)
    case TmVar(n, _) =>
      ctx.getBinding(n) match {
        case TmAbbBind(t1, _) => (t1, store)
        case _                => throw new NoRuleApplies(t)
      }
    case _ =>
      throw new NoRuleApplies(t)
  }

  def eval(ctx: Context, store: Store, t: Term): (Term, Store) =
    try {
      val (t1, store1) = eval1(ctx, store, t)
      eval(ctx, store1, t1)
    } catch {
      case _: NoRuleApplies => (t, store)
    }

  def evalBinding(ctx: Context, store: Store, bind: Binding): (Binding, Store) = bind match {
    case TmAbbBind(t, tyT) =>
      val (t1, store1) = eval(ctx, store, t)
      (TmAbbBind(t1, tyT), store1)
    case b =>
      (b, store)
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
      case (TyString, TyString)             => true
      case (TyId(b1), TyId(b2))             => b1 == b2
      case (TyUnit, TyUnit)                 => true
      case (TyRef(tyT1), TyRef(tyT2))       => tyEqv(ctx, tyT1, tyT2)
      case (TySource(tyT1), TySource(tyT2)) => tyEqv(ctx, tyT1, tyT2)
      case (TySink(tyT1), TySink(tyT2))     => tyEqv(ctx, tyT1, tyT2)
      case (TyVar(i, _), _) if isTyAbb(ctx, i) =>
        tyEqv(ctx, getTyAbb(ctx, i), tyT)
      case (_, TyVar(i, _)) if isTyAbb(ctx, i) =>
        tyEqv(ctx, tyS, getTyAbb(ctx, i))
      case (TyVar(i, _), TyVar(j, _)) => i == j
      case (TyBool, TyBool)           => true
      case (TyNat, TyNat)             => true
      case (TyRecord(fields1), TyRecord(fields2)) =>
        fields1.length == fields2.length && fields2.forall {
          case (li2, tyTi2) => fields1.find { _._1 == li2 } match {
            case Some((li1, tyTi1)) => tyEqv(ctx, tyTi1, tyTi2)
            case None               => false
          }
        }
      case (TyVariant(fields1), TyVariant(fields2)) =>
        fields1.length == fields2.length && (fields1 zip fields2).forall {
          case (f1, f2) => (f1._1 == f2._1) && tyEqv(ctx, f1._2, f2._2)
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
      case (TyBot, _) => true
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
      case (TyVariant(fS), TyVariant(fT)) =>
        fS.forall {
          case (li, tySi) =>
            fT.find { _._1 == li } match {
              case Some((_, tyTi)) => subtype(ctx, tySi, tyTi)
              case None            => false
            }
        }
      case (TyRef(tyT1), TyRef(tyT2))       => subtype(ctx, tyT1, tyT2) && subtype(ctx, tyT2, tyT1)
      case (TyRef(tyT1), TySource(tyT2))    => subtype(ctx, tyT1, tyT2)
      case (TySource(tyT1), TySource(tyT2)) => subtype(ctx, tyT1, tyT2)
      case (TyRef(tyT1), TySink(tyT2))      => subtype(ctx, tyT2, tyT1)
      case (TySink(tyT1), TySink(tyT2))     => subtype(ctx, tyT2, tyT1)
      case (_, _)                           => false
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
          TyArr(meet(ctx, tyS1, tyT1), join(ctx, tyS2, tyT2))
        case (TyRef(tyT1), TyRef(tyT2)) =>
          if (subtype(ctx, tyT1, tyT2) && subtype(ctx, tyT2, tyT1)) {
            TyRef(ty1)
          } else {
            // TODO (* Warning: this is incomplete... *)
            TySource(join(ctx, tyT1, tyT2))
          }
        case (TySource(tyT1), TySource(tyT2)) =>
          TySource(join(ctx, tyT1, tyT2))
        case (TyRef(tyT1), TySource(tyT2)) =>
          TySource(join(ctx, tyT1, tyT2))
        case (TySource(tyT1), TyRef(tyT2)) =>
          TySource(join(ctx, tyT1, tyT2))
        case (TySink(tyT1), TySink(tyT2)) =>
          TySink(meet(ctx, tyT1, tyT2))
        case (TyRef(tyT1), TySink(tyT2)) =>
          TySink(meet(ctx, tyT1, tyT2))
        case (TySink(tyT1), TyRef(tyT2)) =>
          TySink(meet(ctx, tyT1, tyT2))
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
          val allLabels = (labelS concat labelT).distinct
          // there was an error by Pierce!!
          val allFs = allLabels.flatMap { li =>
            (fS.find { _._1 == li }, fT.find { _._1 == li }) match {
              case (Some((_, tySi)), Some((_, tyTi))) =>
                Some(li -> meet(ctx, tySi, tyTi))
              case (Some((_, tySi)), _) =>
                Some(li -> tySi)
              case (_, Some((_, tySi))) =>
                Some(li -> tySi)
              case (None, None) =>
                None
            }
          }
          TyRecord(allFs)
        case (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) =>
          TyArr(join(ctx, tyS1, tyT1), meet(ctx, tyS2, tyT2))
        case (TyRef(tyT1), TyRef(tyT2)) =>
          if (subtype(ctx, tyT1, tyT2) && subtype(ctx, tyT2, tyT1)) {
            TyRef(ty1)
          } else {
            // TODO (* Warning: this is incomplete... *)
            // The alternative here is:
            // TySink(join(ctx, tyT1, tyT2))
            TySource(meet(ctx, tyT1, tyT2))
          }
        case (TySource(tyT1), TySource(tyT2)) =>
          TySource(meet(ctx, tyT1, tyT2))
        case (TyRef(tyT1), TySource(tyT2)) =>
          TySource(meet(ctx, tyT1, tyT2))
        case (TySource(tyT1), TyRef(tyT2)) =>
          TySource(meet(ctx, tyT1, tyT2))
        case (TySink(tyT1), TySink(tyT2)) =>
          TySink(join(ctx, tyT1, tyT2))
        case (TyRef(tyT1), TySink(tyT2)) =>
          TySink(join(ctx, tyT1, tyT2))
        case (TySink(tyT1), TyRef(tyT2)) =>
          TySink(join(ctx, tyT1, tyT2))
        case _ => TyBot
      }
    }

  def typeof(ctx: Context, t: Term): Ty = t match {
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
          else {
            println(tyT1)
            println(tyT2)
            println(simplifyTy(ctx, tyT1))
            throw new Exception("parameter mismatch in " + t + " : " + tyT2 + " != " + tyT11)
          }
        case TyBot => TyBot
        case z =>
          {
            println(tyT1)
            println(tyT2)
            println(simplifyTy(ctx, tyT1))
            throw new Exception("arrow type expected in " + t1)
          }
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
    case TmLet(x, t1, t2) =>
      val tyT1 = typeof(ctx, t1)
      val ctx1 = ctx.addBinding(x, VarBind(tyT1))
      typeShift(-1, typeof(ctx1, t2))
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
    case TmCase(t, cases) =>
      simplifyTy(ctx, typeof(ctx, t)) match {
        case TyVariant(fieldTys) =>
          cases.foreach {
            case (l, _, _) => fieldTys.find(_._1 == l) match {
              case Some(_) =>
              case None    => throw new Exception("label" + l + " is not in type")
            }
          }
          val casetypes = cases map {
            case (li, xi, ti) =>
              val tyTi = fieldTys.find(_._1 == li) match {
                case Some(ty) => ty._2
                case None     => throw new Exception("label" + li + " is not found")
              }
              val ctx1 = ctx.addBinding(xi, VarBind(tyTi))
              typeShift(-1, typeof(ctx1, ti))
          }
          casetypes.foldLeft(TyBot: Ty) { join(ctx, _, _) }
        case TyBot => TyBot
        case _     => throw new Exception("Expected variant type " + t)
      }
    case TmFix(t1) =>
      val tyT1 = typeof(ctx, t1)
      simplifyTy(ctx, tyT1) match {
        case TyArr(tyT11, tyT12) =>
          if (subtype(ctx, tyT12, tyT11))
            tyT12
          else
            throw new Exception("result of body not compatible with domain " + t + " : " + tyT12 + " != " + tyT11)
        case _ =>
          throw new Exception("arrow type expected in " + t1)
      }
    case TmTag(li, ti, tyT) =>
      simplifyTy(ctx, tyT) match {
        case TyVariant(fieldTys) =>
          fieldTys.find { _._1 == li } match {
            case Some((_, tyTiExpected)) =>
              val tyTi = typeof(ctx, ti)
              if (subtype(ctx, tyTi, tyTiExpected))
                tyT
              else
                throw new Exception("field doesn't have expected type in " + t)
            case None => throw new Exception("label " + li + " not found " + t)
          }
        case z =>
          println(z)
          throw new Exception("annotation is not a variant type: " + t)
      }
    case TmAscribe(t1, tyT) =>
      if (subtype(ctx, typeof(ctx, t1), tyT))
        tyT
      else
        throw new Exception("body of as-term doesn't have the expected type in " + t)
    case TmString(_) =>
      TyString
    case TmUnit =>
      TyUnit
    case TmRef(t1) =>
      TyRef(typeof(ctx, t1))
    case TmLoc(l) =>
      sys.error("locations are not supposed to occur in source programs!")
    case TmDeref(t1) =>
      simplifyTy(ctx, typeof(ctx, t1)) match {
        case TyRef(tyT1)    => tyT1
        case TyBot          => TyBot
        case TySource(tyT1) => tyT1
        case _              => sys.error("argument of ! is not a Ref")
      }
    case TmAssign(t1, t2) =>
      simplifyTy(ctx, typeof(ctx, t1)) match {
        case TyRef(tyT1) =>
          if (subtype(ctx, typeof(ctx, t2), tyT1))
            TyUnit
          else
            sys.error("arguments of := are incompatible")
        case TySink(tyT1) =>
          if (subtype(ctx, typeof(ctx, t2), tyT1))
            TyUnit
          else
            sys.error("arguments of := are incompatible")
        case TyBot =>
          val _ = typeof(ctx, t2)
          TyBot
        case _ =>
          sys.error("argument of ! is not a Ref")
      }
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