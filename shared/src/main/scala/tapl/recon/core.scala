package tapl.recon

object Util {

  def isNumericVal(ctx: Context, t: Term): Boolean = t match {
    case TmZero     => true
    case TmSucc(t1) => isNumericVal(ctx, t1)
    case _          => false
  }

  def isVal(ctx: Context, t: Term): Boolean = t match {
    case TmTrue                      => true
    case TmFalse                     => true
    case t1 if isNumericVal(ctx, t1) => true
    case TmAbs(_, _, _)              => true
    case _                           => false
  }
}

object Evaluator {
  import Util._
  import Syntax._

  private def eval1(ctx: Context, t: Term): Term = t match {
    case TmIf(TmTrue, t2, t3) =>
      t2
    case TmIf(TmFalse, t2, t3) =>
      t3
    case TmIf(t1, t2, t3) =>
      val t11 = eval1(ctx, t1)
      TmIf(t11, t2, t3)
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
  import Util._

  type Constr = List[(Ty, Ty)]
  type Subst = List[(TyId, Ty)]
  type UVarGenerator = Unit => NextUVar
  case class NextUVar(n: String, gen: UVarGenerator)

  val uvargen: UVarGenerator = {
    def f(n: Int): UVarGenerator = _ => NextUVar("?X" + n, f(n + 1))
    f(0)
  }

  val emptyConstr: Constr = List()

  def recon(ctx: Context, nextUVar: UVarGenerator, t: Term): (Ty, UVarGenerator, Constr) = t match {
    case TmVar(i, _) =>
      val tyT = ctx.getType(i)
      (tyT, nextUVar, emptyConstr)
    case TmAbs(x, Some(tyT1), t2) =>
      val ctx1 = ctx.addBinding(x, VarBind(tyT1))
      val (tyT2, nextUVar2, constr2) = recon(ctx1, nextUVar, t2)
      (TyArr(tyT1, tyT2), nextUVar2, constr2)
    case TmAbs(x, None, t2) =>
      val NextUVar(u, nextUVar0) = nextUVar(())
      val tyX = TyId(u)
      val ctx1 = ctx.addBinding(x, VarBind(tyX))
      val (tyT2, nextUVar2, constr2) = recon(ctx1, nextUVar0, t2)
      (TyArr(tyX, tyT2), nextUVar2, constr2)
    case TmApp(t1, t2) =>
      val (tyT1, nextuvar1, constr1) = recon(ctx, nextUVar, t1)
      val (tyT2, nextuvar2, constr2) = recon(ctx, nextuvar1, t2)
      val NextUVar(tyX, nextuvar3) = nextuvar2(())
      val newconstr = List((tyT1, TyArr(tyT2, TyId(tyX))))
      (TyId(tyX), nextuvar3, newconstr ++ constr1 ++ constr2)
    case TmZero =>
      (TyNat, nextUVar, emptyConstr)
    case TmSucc(t1) =>
      val (tyT1, nextuvar1, constr1) = recon(ctx, nextUVar, t1)
      (TyNat, nextuvar1, (tyT1, TyNat) :: constr1)
    case TmPred(t1) =>
      val (tyT1, nextuvar1, constr1) = recon(ctx, nextUVar, t1)
      (TyNat, nextuvar1, (tyT1, TyNat) :: constr1)
    case TmIsZero(t1) =>
      val (tyT1, nextuvar1, constr1) = recon(ctx, nextUVar, t1)
      (TyBool, nextuvar1, (tyT1, TyNat) :: constr1)
    case TmTrue =>
      (TyBool, nextUVar, emptyConstr)
    case TmFalse =>
      (TyBool, nextUVar, emptyConstr)
    case TmIf(t1, t2, t3) =>
      val (tyT1, nextuvar1, constr1) = recon(ctx, nextUVar, t1)
      val (tyT2, nextuvar2, constr2) = recon(ctx, nextuvar1, t2)
      val (tyT3, nextuvar3, constr3) = recon(ctx, nextuvar2, t3)
      val newconstr = List((tyT1, TyBool), (tyT2, TyBool))
      (tyT3, nextuvar3, newconstr ++ constr1 ++ constr2 ++ constr3)
  }

  private def substInTy(tyX: String, tyT: Ty, tyS: Ty): Ty = {
    def f(tyS: Ty): Ty = tyS match {
      case TyArr(tyS1, tyS2) => TyArr(f(tyS1), f(tyS2))
      case TyNat             => TyNat
      case TyBool            => TyBool
      case TyId(s) =>
        if (s == tyX) tyT else TyId(s)
    }
    f(tyS)
  }

  def applySub(ctr: Subst, tyT: Ty): Ty = {
    ctr.reverse.foldLeft(tyT)((tyS, ctr) => ctr match { case (TyId(tyX), tyC2) => substInTy(tyX, tyC2, tyS) })
  }

  def substInConstr(tyX: String, tyT: Ty, constr: Constr): Constr =
    constr.map { case (tyS1, tyS2) => (substInTy(tyX, tyT, tyS1), substInTy(tyX, tyT, tyS2)) }

  def occursIn(tyX: String, tyT: Ty): Boolean = {
    def occ(tyT: Ty): Boolean = tyT match {
      case TyArr(tyT1, tyT2) => occ(tyT1) || occ(tyT2)
      case TyNat             => false
      case TyBool            => false
      case TyId(id)          => id == tyX
    }
    occ(tyT)
  }

  // why do we need ctx here?
  def unify(ctx: Context, msg: String, constr: Constr): Subst = {
    def u(constr: Constr): Subst = constr match {
      case Nil =>
        Nil
      case (tyS, TyId(tyX)) :: rest =>
        if (tyS == TyId(tyX))
          u(rest)
        else if (occursIn(tyX, tyS))
          sys.error(msg + ": circular constraints")
        else
          u(substInConstr(tyX, tyS, rest)) ++ List((TyId(tyX), tyS))
      case (TyId(tyX), tyT) :: rest =>
        if (tyT == TyId(tyX))
          u(rest)
        else if (occursIn(tyX, tyT))
          sys.error(msg + ": circular constraints")
        else
          u(substInConstr(tyX, tyT, rest)) ++ List((TyId(tyX), tyT))
      case (TyNat, TyNat) :: rest =>
        u(rest)
      case (TyBool, TyBool) :: rest =>
        u(rest)
      case (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) :: rest =>
        u((tyS1, tyT1) :: (tyS2, tyT2) :: rest)
      case (tyS, tyT) :: rest =>
        //output(tyS, tyT)
        sys.error("unsolvable constraints")
    }
    u(constr)
  }

}

class NoRuleApplies(t: Term) extends Exception("No rule applies for term: " + t)