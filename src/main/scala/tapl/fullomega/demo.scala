package tapl.fullomega

class FullOmegaDemo extends util.Demo {

  override type Ctx = (Context, Store)
  override type Cmd = Command

  import Evaluator._
  import Typer._
  import Syntax._
  import util.Print._
  import PrettyPrinter._

  val width = 60

  override val initialContext: Ctx = (Context(), Store())
  override val defaultExample: String = "examples/fullomega.tapl"
  override val name: String = "FullOmega"

  override def parseInput(s: String): List[Cmd] =
    FullOmegaParsers.input(s)(Context())._1

  private def checkBinding(ctx: Context, bind: Binding): Binding = bind match {
    case NameBind =>
      NameBind
    case TyVarBind(knK) =>
      TyVarBind(knK)
    case VarBind(tyT) =>
      VarBind(tyT)
    case TyAbbBind(tyT, None) =>
      TyAbbBind(tyT, Some(kindof(ctx, tyT)))
    case TmAbbBind(t, None) =>
      TmAbbBind(t, Some(typeof(ctx, t)))
    case TmAbbBind(t, Some(tyT)) =>
      val tyT1 = typeof(ctx, t)
      if (tyEqv(ctx, tyT1, tyT))
        TmAbbBind(t, Some(tyT))
      else
        throw new Exception("type of binding doesn't match declared type in " + bind)
    case TyAbbBind(tyT, Some(knK)) =>
      val knK1 = kindof(ctx, tyT)
      if (knK == knK1) TyAbbBind(tyT, Some(knK))
      else throw new Exception("type of binding doesn't match declared type in " + bind)
  }

  def processCommand(in: Ctx, cmd: Cmd): Ctx = {
    val (ctx, store) = in
    cmd match {
      case Eval(t1) =>
        val ty1 = Typer.typeof(ctx, t1)
        val doc1 = g2(ptmATerm(true, ctx, t1) :: ":" :/: ptyTy(ctx, ty1) :: ";")

        val (t2, store1) = eval(ctx, store, t1)
        val ty2 = Typer.typeof(ctx, t2)
        val doc2 = g2(ptmATerm(true, ctx, t2) :: ":" :/: ptyTy(ctx, ty2) :: ";")

        output("====================")
        output(print(doc1, width))
        output("""||""")
        output("""\/""")
        output(print(doc2, width))

        (ctx, store1)

      case Bind(x, bind) =>
        output("")
        val bind1 = checkBinding(ctx, bind)
        val (bind2, store1) = evalBinding(ctx, store, bind1)
        val doc0 = x :: pBinding(ctx, bind2) :: ";"
        val doc1 = x :: pBindingTy(ctx, bind2) :: ";"
        output("====================")
        output(print(doc0, width))
        output(print(doc1, width))
        (ctx.addBinding(x, bind2), store1.shift(1))

      case SomeBind(tyX, x, t1) =>
        val tyT = typeof(ctx, t1)
        simplifyTy(ctx, tyT) match {
          case TySome(_, knK, tyBody) =>
            val (t2, store1) = eval(ctx, store, t1)
            val b = t2 match {
              case TmPack(_, t12, _) => (TmAbbBind(termShift(1, t12), Some(tyBody)))
              case _                 => VarBind(tyBody)
            }
            val ctx1 = ctx.addBinding(tyX, TyVarBind(knK))
            val ctx2 = ctx1.addBinding(x, b)

            output(tyX)
            val doc2 = x :/: ":" :/: ptyTy(ctx1, tyBody)
            output(print(doc2, width))
            (ctx2, store1)
          case _ =>
            sys.error("existential type expected")
        }
    }
  }

}

object FullOmegaDemo extends FullOmegaDemo with util.DemoCL
object FullOmegaDemoJS extends FullOmegaDemo with util.DemoJS
