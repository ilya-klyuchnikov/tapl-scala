package util

import scala.io.Source

trait Demo[Context, Command] {
  val initialContext: Context
  val defaultExample: String
  def parseInput(s: String): List[Command]
  def processCommand(ctx: Context, cmd: Command): Context

  def demo(s: String): Unit = {
    parseInput(s).foldLeft(initialContext)(processCommand)
  }

  def main(args: Array[String]) = {
    val inFile = if (args.isEmpty) defaultExample else args(0)
    val input = Source.fromFile(inFile).mkString("")
    demo(input)
  }

}