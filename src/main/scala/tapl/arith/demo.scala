package tapl.arith

class ArithDemo extends util.Demo {
  override type Ctx = Unit
  override type Cmd = Command

  import Evaluator._
  import util.Print._
  import PrettyPrinter._

  val width = 60

  override val initialContext: Unit = ()
  override val defaultExample: String = "examples/arith.tapl"
  override val name: String = "Arith"

  override def parseInput(s: String): List[Cmd] =
    ArithParsers.input(s)

  override def processCommand(ctx: Unit, cmd: Cmd): Unit = cmd match {
    case Eval(t1) =>
      val doc1 = g2(ptmATerm(true, t1) :: ";")
      val t2 = eval(t1)
      val doc2 = g2(ptmATerm(true, t2) :: ";")

      output("====================")
      output(print(doc1, width))
      output("""||""")
      output("""\/""")
      output(print(doc2, width))
  }
}

object ArithDemo extends ArithDemo with util.DemoCL
object ArithDemoJS extends ArithDemo with util.DemoJS
