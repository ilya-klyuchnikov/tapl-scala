package util

trait Demo[Context, Command] {
  val initialContext: Context
  val defaultExample: String
  def parseInput(s: String): List[Command]
  def processCommand(ctx: Context, cmd: Command): Context

  def demo(s: String): Unit =
    parseInput(s).foldLeft(initialContext)(processCommand)

  def main(args: Array[String]): Unit = {
    import java.nio.file.{Files, Paths}

    val inFile = if (args.isEmpty) defaultExample else args(0)
    val input = Files.readString(Paths.get(inFile))
    val taplPackage = this.getClass.getPackage.getName.split("\\.").toList.last
    println()
    println()
    println("====================")
    println(s">> $taplPackage $inFile")
    demo(input)
  }
}
