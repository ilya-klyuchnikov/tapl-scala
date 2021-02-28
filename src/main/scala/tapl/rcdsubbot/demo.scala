package tapl.rcdsubbot

object RcdSubBotDemo extends util.Demo[Context, Command] {
  import Evaluator._
  import Typer._
  import PrettyPrinter._
  import scala.language.implicitConversions
  import util.Print._, util.Print.text2doc

  val width = 60

  override val initialContext: Context = Context()
  override val defaultExample: String = "examples/rcdsubbot.tapl"

  override def parseInput(s: String): List[Command] =
    RcdSubBotParsers.input(s)(Context())._1

  def processCommand(ctx: Context, cmd: Command): Context =
    cmd match {
      case Eval(t1) =>
        val ty1 = Typer.typeof(ctx, t1)
        val doc1 = g2(ptmATerm(true, ctx, t1) ::: ":" :/: ptyTy(ctx, ty1) ::: ";")

        val t2 = eval(ctx, t1)
        val ty2 = Typer.typeof(ctx, t2)
        val doc2 = g2(ptmATerm(true, ctx, t2) ::: ":" :/: ptyTy(ctx, ty2) ::: ";")

        println("====================")
        println(print(doc1, width))
        println("""||""")
        println("""\/""")
        println(print(doc2, width))

        ctx

      case Bind(x, bind) =>
        val doc1 = x ::: pBindingTy(ctx, bind) ::: ";"
        println("====================")
        println(print(doc1, width))
        ctx.addBinding(x, bind)
    }

}
