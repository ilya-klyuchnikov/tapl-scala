package util

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
