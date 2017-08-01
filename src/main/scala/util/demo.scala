package util

import scala.io.Source

trait Demo {
  type Ctx
  type Cmd
  val initialContext: Ctx
  val defaultExample: String
  val name: String
  def parseInput(s: String): List[Cmd]
  def processCommand(ctx: Ctx, cmd: Cmd): Ctx

  def demo(s: String): Unit = {
    parseInput(s).foldLeft(initialContext)(processCommand)
  }

  def output(s: String): Unit = {
    Console.println(s)
  }
}

trait DemoCL extends Demo {
  def main(args: Array[String]): Unit = {
    val inFile = if (args.isEmpty) defaultExample else args(0)
    val input = Source.fromFile(inFile).mkString("")
    demo(input)
  }
}

trait DemoJS extends Demo {
  var output: String = ""

  override def output(s: String): Unit = {
    output += s + "\n"
  }

  override def demo(s: String): Unit = {
    output = ""
    super.demo(s)
  }
}
