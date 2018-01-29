package tapl.simplebool

class SimpleBoolDemo extends util.Demo {

  override type Ctx = Context
  override type Cmd = Command

  import Evaluator._
  import util.Print._
  import PrettyPrinter._

  val width = 60

  override val initialContext: Ctx = Context()
  override val defaultExample: String = "examples/simplebool.tapl"
  override val name: String = "SimpleBool"

  override def parseInput(s: String): List[Cmd] =
    SimpleBoolParsers.input(s)(Context())._1

  def processCommand(ctx: Ctx, cmd: Cmd): Ctx = cmd match {
    case Eval(t1) =>

      val ty1 = Typer.typeof(ctx, t1)
      val doc1 = g2(ptmATerm(true, ctx, t1) :: ":" :/: ptyTy(ty1) :: ";")

      val t2 = eval(ctx, t1)
      val ty2 = Typer.typeof(ctx, t2)
      val doc2 = g2(ptmATerm(true, ctx, t2) :: ":" :/: ptyTy(ty2) :: ";")

      output("====================")
      output(print(doc1, width))
      output("""||""")
      output("""\/""")
      output(print(doc2, width))

      ctx
    case Bind(n, b) =>
      val doc1 = n :: pBinding(b) :: ";"

      output("====================")
      output(print(doc1, width))

      ctx.addBinding(n, b)
  }

}
