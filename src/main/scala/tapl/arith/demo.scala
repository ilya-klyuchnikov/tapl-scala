package tapl.arith

import scala.io.Source

object ArithDemo extends App {

  import Evaluator._
  import util.Print._
  import PrettyPrinter._

  val width = 60

  private def processCommand(cmd: Command): Unit = cmd match {
    case Eval(t1) =>
      val doc1 = g2(ptmATerm(true, t1) :: ";")
      val t2 = eval(t1)
      val doc2 = g2(ptmATerm(true, t2) :: ";")

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

  val inFile = if (args.isEmpty) "examples/arith.tapl" else args(0)
  val input = Source.fromFile(inFile).mkString("")

  demo(input)
}