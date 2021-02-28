package tapl.untyped

object UntypedDemo extends util.Demo[Context, Command] {
  import Evaluator._
  import PrettyPrinter._
  import scala.language.implicitConversions
  import util.Print._, util.Print.text2doc

  val width = 60

  override val initialContext: Context = Context()
  override val defaultExample: String = "examples/untyped.tapl"

  override def parseInput(s: String): List[Command] =
    UntypedParsers.input(s)(Context())._1

  def processCommand(ctx: Context, cmd: Command): Context =
    cmd match {
      case Eval(t1) =>
        val doc1 = g2(ptmATerm(true, ctx, t1) ::: ";")
        val t2 = eval(ctx, t1)
        val doc2 = g2(ptmATerm(true, ctx, t2) ::: ";")

        println("====================")
        println(print(doc1, width))
        println("""||""")
        println("""\/""")
        println(print(doc2, width))

        ctx

      case Bind(n, b) =>
        ctx.addBinding(n, b)
    }

}
