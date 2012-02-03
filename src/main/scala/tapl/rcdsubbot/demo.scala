package tapl.rcdsubbot

import scala.io.Source

object RcdSubBotDemo extends App {
  import Evaluator._
  import Typer._
  import util.Print._
  import PrettyPrinter._

  val width = 60

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
      val doc1 = x :: pBindingTy(ctx, bind) :: ";"
      println("====================")
      println(print(doc1, width))
      ctx.addBinding(x, bind)
  }

  def demo(s: String): Unit = {
    val (commands, _) = RcdSubBotParsers.input(s)(Context())
    commands.foldLeft(Context())(processCommand)
  }

  val inFile = if (args.isEmpty) "examples/rcdsubbot.tapl" else args(0)
  val input = Source.fromFile(inFile).mkString("")

  demo(input)

}