package tapl.simplebool

object SimpleBoolDemo extends util.Demo[Context, Command] {
  import Evaluator._
  import PrettyPrinter._
  import scala.language.implicitConversions
  import util.Print._, util.Print.text2doc

  val width = 60

  override val initialContext: Context = Context()
  override val defaultExample: String = "examples/simplebool.tapl"

  override def parseInput(s: String): List[Command] =
    SimpleBoolParsers.input(s)(Context())._1

  def processCommand(ctx: Context, cmd: Command): Context =
    cmd match {
      case Eval(t1) =>
        val ty1 = Typer.typeof(ctx, t1)
        val doc1 = g2(ptmATerm(true, ctx, t1) ::: ":" :/: ptyTy(ty1) ::: ";")

        val t2 = eval(ctx, t1)
        val ty2 = Typer.typeof(ctx, t2)
        val doc2 = g2(ptmATerm(true, ctx, t2) ::: ":" :/: ptyTy(ty2) ::: ";")

        println("====================")
        println(print(doc1, width))
        println("""||""")
        println("""\/""")
        println(print(doc2, width))

        ctx
      case Bind(n, b) =>
        val doc1 = n ::: pBinding(b) ::: ";"

        println("====================")
        println(print(doc1, width))

        ctx.addBinding(n, b)
    }

}
