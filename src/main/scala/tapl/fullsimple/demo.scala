package tapl.fullsimple

class FullSimpleDemo extends util.Demo {

  override type Ctx = Context
  override type Cmd = Command

  import Evaluator._
  import Typer._
  import util.Print._
  import PrettyPrinter._
  
  val width = 60

  override val initialContext: Ctx = Context()
  override val defaultExample: String = "examples/fullsimple.tapl"
  override val name: String = "FullSimple"

  override def parseInput(s: String): List[Cmd] =
    FullSimpleParsers.input(s)(Context())._1

  private def checkBinding(ctx: Ctx, bind: Binding): Binding = bind match {
    case NameBind =>
      NameBind
    case VarBind(tyT) =>
      VarBind(tyT)
    case TmAbbBind(t, None) =>
      TmAbbBind(t, Some(typeof(ctx, t)))
    case TmAbbBind(t, Some(tyT)) =>
      val tyT1 = typeof(ctx, t)
      if (tyEqv(ctx, tyT1, tyT))
        TmAbbBind(t, Some(tyT))
      else
        throw new Exception("type of binding doesn't match declared type in " + bind)
    case TyVarBind =>
      TyVarBind
    case TyAbbBind(tyT) =>
      TyAbbBind(tyT)
  }

  def processCommand(ctx: Ctx, cmd: Cmd): Ctx = cmd match {
    case Eval(t1) =>
      val ty1 = Typer.typeof(ctx, t1)
      val doc1 = g2(ptmATerm(true, ctx, t1) :: ":" :/: ptyTy(ctx, ty1) :: ";")

      val t2 = eval(ctx, t1)
      val ty2 = Typer.typeof(ctx, t2)
      val doc2 = g2(ptmATerm(true, ctx, t2) :: ":" :/: ptyTy(ctx, ty2) :: ";")

      output("====================")
      output(print(doc1, width))
      output("""||""")
      output("""\/""")
      output(print(doc2, width))

      ctx

    case Bind(x, bind) =>
      val bind1 = checkBinding(ctx, bind)
      val bind2 = evalBinding(ctx, bind1)
      val doc1 = x :: pBindingTy(ctx, bind2) :: ";"
      output("====================")
      output(print(doc1, width))

      ctx.addBinding(x, bind2)
  }

}

object FullSimpleDemo extends FullSimpleDemo with util.DemoCL
object FullSimpleDemoJS extends FullSimpleDemo with util.DemoJS
