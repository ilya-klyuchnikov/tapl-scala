package tapl.fullref

class FullRefDemo extends util.Demo {

  override type Ctx = (Context, Store)
  override type Cmd = Command

  import Evaluator._
  import Typer._
  import util.Print._
  import PrettyPrinter._

  val width = 60

  override val initialContext: Ctx = (Context(), Store())
  override val defaultExample: String = "examples/fullref.tapl"
  override val name: String = "FullRef"

  override def parseInput(s: String): List[Cmd] =
    FullRefParsers.input(s)(Context())._1

  private def checkBinding(ctx: Context, bind: Binding): Binding = bind match {
    case NameBind =>
      NameBind
    case TyVarBind =>
      TyVarBind
    case VarBind(tyT) =>
      VarBind(tyT)
    case TyAbbBind(tyT) =>
      TyAbbBind(tyT)
    case TmAbbBind(t, None) =>
      TmAbbBind(t, Some(typeof(ctx, t)))
    case TmAbbBind(t, Some(tyT)) =>
      val tyT1 = typeof(ctx, t)
      if (subtype(ctx, tyT1, tyT))
        TmAbbBind(t, Some(tyT))
      else
        throw new Exception("type of binding doesn't match declared type in " + bind)
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
    }
  }

}

object FullRefDemo extends FullRefDemo with util.DemoCL
object FullRefDemoJS extends FullRefDemo with util.DemoJS
