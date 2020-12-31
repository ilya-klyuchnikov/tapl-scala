package tapl.fulluntyped

object FullUntypedDemo extends util.Demo[Context, Command] {
  import Evaluator._
  import util.Print._
  import PrettyPrinter._

  val width = 60

  override val initialContext: Context = Context()
  override val defaultExample: String = "examples/fulluntyped.tapl"

  override def parseInput(s: String): List[Command] =
    FullUntypedParsers.input(s)(Context())._1

  def processCommand(ctx: Context, cmd: Command): Context =
    cmd match {
      case Eval(t1) =>
        val doc1 = g2(ptmATerm(true, ctx, t1) :: ";")
        val t2 = eval(ctx, t1)
        val doc2 = g2(ptmATerm(true, ctx, t2) :: ";")

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
