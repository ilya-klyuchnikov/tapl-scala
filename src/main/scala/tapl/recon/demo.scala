package tapl.recon

import Typer._

object ReconDemo extends util.Demo[(Context, UVarGenerator, IdConstr), Command] {
  import Evaluator._
  import PrettyPrinter._
  import scala.language.implicitConversions
  import util.Print._, util.Print.text2doc

  val width = 60

  override val initialContext: (Context, UVarGenerator, IdConstr) =
    (Context(), uvargen, emptyIdConstr)
  override val defaultExample: String = "examples/recon.tapl"

  override def parseInput(s: String): List[Command] =
    ReconParsers.input(s)(Context())._1

  def processCommand(
      in: (Context, UVarGenerator, IdConstr),
      cmd: Command,
  ): (Context, UVarGenerator, IdConstr) =
    in match {
      case (ctx, nextuvar, constr) =>
        cmd match {
          case Eval(t1) =>
            val (tyT, nextuvar1, constrT: Constr) = recon(ctx, nextuvar, t1)
            val constr11: Constr = constr ++ constrT
            val constr12: IdConstr = unify(ctx, "Could not simplify constraints", constr11)

            val ty1 = applySub(constr12, tyT)
            val doc1 = g2(ptmATerm(true, ctx, t1) ::: ":" :/: ptyTy(ctx, ty1) ::: ";")

            val t2 = eval(ctx, t1)
            val (tyT2, nextuvar2, constrT2) = recon(ctx, nextuvar, t1)
            val constr21 = constr ++ constrT2
            val constr22 = unify(ctx, "Could not simplify constraints", constr21)
            val ty2 = applySub(constr22, tyT2)
            val doc2 = g2(ptmATerm(true, ctx, t2) ::: ":" :/: ptyTy(ctx, ty1) ::: ";")

            println("====================")
            println(print(doc1, width))
            println("""||""")
            println("""\/""")
            println(print(doc2, width))
            (ctx, nextuvar1, constr12)
          case Bind(x, bind) =>
            val doc1 = x ::: pBindingTy(ctx, bind) ::: ";"
            println("====================")
            println(print(doc1, width))
            (ctx.addBinding(x, bind), nextuvar, constr)
        }
    }

}
