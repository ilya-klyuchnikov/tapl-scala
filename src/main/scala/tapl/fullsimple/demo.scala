package tapl.fullsimple

import scala.io.Source

object FullSimpleDemo extends App {
  import Evaluator._
  import Typer._
  import util.Print._
  import PrettyPrinter._
  
  val width = 60

  private def checkBinding(ctx: Context, bind: Binding): Binding = bind match {
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

  def processCommand(ctx: Context, cmd: Command): Context = cmd match {
    case Eval(t1) =>
      val ty1 = Typer.typeof(ctx, t1)
      val doc1 = g2(ptmATerm(true, ctx, t1) :: ":" :/: ptyTy(ctx, ty1) :: ";")

      val t2 = eval(ctx, t1)
      val ty2 = Typer.typeof(ctx, t2)
      val doc2 = g2(ptmATerm(true, ctx, t2) :: ":" :/: ptyTy(ctx, ty2) :: ";")

      println("====================")
      println(print(doc1, width))
      println("""||""")
      println("""\/""")
      println(print(doc2, width))

      ctx

    case Bind(x, bind) =>
      val bind1 = checkBinding(ctx, bind)
      val bind2 = evalBinding(ctx, bind1)
      val doc1 = x :: pBindingTy(ctx, bind2) :: ";"
      println("====================")
      println(print(doc1, width))

      ctx.addBinding(x, bind2)
  }

  def demo(s: String): Unit = {
    val (commands, _) = FullSimpleParsers.input(s)(Context())
    commands.foldLeft(Context())(processCommand)
  }

  val inFile = if (args.isEmpty) "examples/fullsimple.tapl" else args(0)
  val input = Source.fromFile(inFile).mkString("")

  demo(input)

}