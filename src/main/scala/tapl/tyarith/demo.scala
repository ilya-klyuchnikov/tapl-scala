package tapl.tyarith

import scala.io.Source

object TyArithDemo extends App {

  import Evaluator._
  import util.Print._
  import PrettyPrinter._

  val width = 60

  private def processCommand(cmd: Command): Unit = cmd match {
    case Eval(t1) =>
      val ty1 = Typer.typeof(t1)
      val doc1 = g2(ptmATerm(true, t1) :: ":" :/: ptyTy(ty1) :: ";")

      val t2 = eval(t1)
      val ty2 = Typer.typeof(t2)
      val doc2 = g2(ptmATerm(true, t2) :: ":" :/: ptyTy(ty2) :: ";")

      println("====================")
      println(print(doc1, width))
      println("""||""")
      println("""\/""")
      println(print(doc2, width))

  }

  private def demo(input: String): Unit = {
    val cmds = ArithParsers.input(input)
    cmds.foreach(processCommand(_))
  }

  val inFile = if (args.isEmpty) "examples/tyarith.tapl" else args(0)
  val input = Source.fromFile(inFile).mkString("")

  demo(input)
}