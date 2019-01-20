package tapl.recon

import Typer._

class ReconDemo extends util.Demo {

  override type Ctx = (Context, UVarGenerator, Constr)
  override type Cmd = Command

  import Evaluator._
  import util.Print._
  import PrettyPrinter._

  val width = 60

  override val initialContext: Ctx = (Context(), uvargen, emptyConstr)
  override val defaultExample: String = "examples/recon.tapl"
  override val name: String = "Recon"

  override def parseInput(s: String): List[Cmd] =
    ReconParsers.input(s)(Context())._1

  def processCommand(in: Ctx, cmd: Cmd): Ctx = in match {
    case (ctx, nextuvar, constr) => cmd match {
      case Eval(t1) =>

        val (tyT, nextuvar1, constrT) = recon(ctx, nextuvar, t1)
        val constr11 = constr ++ constrT
        val constr12 = unify(ctx, "Could not simplify constraints", constr11)
        
        val ty1 = applySub(constr12, tyT)
        val doc1 = g2(ptmATerm(true, ctx, t1) :: ":" :/: ptyTy(ctx, ty1) :: ";")

        val t2 = eval(ctx, t1)
        val (tyT2, nextuvar2, constrT2) = recon(ctx, nextuvar, t1)
        val constr21 = constr ++ constrT2
        val constr22 = unify(ctx, "Could not simplify constraints", constr21)
        val ty2 = applySub(constr22, tyT2)
        val doc2 = g2(ptmATerm(true, ctx, t2) :: ":" :/: ptyTy(ctx, ty1) :: ";")

        output("====================")
        output(print(doc1, width))
        output("""||""")
        output("""\/""")
        output(print(doc2, width))
        (ctx, nextuvar1, constr12)
      case Bind(x, bind) =>
        val doc1 = x :: pBindingTy(ctx, bind) :: ";"
        output("====================")
        output(print(doc1, width))
        (ctx.addBinding(x, bind), nextuvar, constr)
    }
  }

}
