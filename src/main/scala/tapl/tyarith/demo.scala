package tapl.tyarith

class TyArithDemo extends util.Demo {

  override type Ctx = Unit
  override type Cmd = Command

  import Evaluator._
  import util.Print._
  import PrettyPrinter._

  val width = 60

  override val initialContext: Ctx = ()
  override val defaultExample: String = "examples/tyarith.tapl"
  override val name: String = "TyArith"

  override def parseInput(s: String): List[Cmd] =
    ArithParsers.input(s)

  def processCommand(ctx: Unit, cmd: Cmd): Unit = cmd match {
    case Eval(t1) =>
      val ty1 = Typer.typeof(t1)
      val doc1 = g2(ptmATerm(true, t1) :: ":" :/: ptyTy(ty1) :: ";")

      val t2 = eval(t1)
      val ty2 = Typer.typeof(t2)
      val doc2 = g2(ptmATerm(true, t2) :: ":" :/: ptyTy(ty2) :: ";")

      output("====================")
      output(print(doc1, width))
      output("""||""")
      output("""\/""")
      output(print(doc2, width))

  }

}
