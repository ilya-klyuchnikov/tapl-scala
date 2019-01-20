package tapl.untyped

class UntypedDemo extends util.Demo {

  override type Ctx = Context
  override type Cmd = Command

  import Evaluator._
  import util.Print._
  import PrettyPrinter._

  val width = 60

  override val initialContext: Ctx = Context()
  override val defaultExample: String = "examples/untyped.tapl"
  override val name: String = "Untyped"

  override def parseInput(s: String): List[Cmd] =
    UntypedParsers.input(s)(Context())._1


  def processCommand(ctx: Ctx, cmd: Cmd): Ctx = cmd match {
    case Eval(t1) =>
      val doc1 = g2(ptmATerm(true, ctx, t1) :: ";")
      val t2 = eval(ctx, t1)
      val doc2 = g2(ptmATerm(true, ctx, t2) :: ";")

      output("====================")
      output(print(doc1, width))
      output("""||""")
      output("""\/""")
      output(print(doc2, width))

      ctx

    case Bind(n, b) =>
      ctx.addBinding(n, b)
  }

}
