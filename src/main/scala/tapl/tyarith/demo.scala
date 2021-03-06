package tapl.tyarith

object TyArithDemo extends util.Demo[Unit, Command] {

  import Evaluator._
  import PrettyPrinter._
  import scala.language.implicitConversions
  import util.Print._, util.Print.text2doc
  import Command._

  val width = 60

  override val initialContext: Unit = ()
  override val defaultExample: String = "examples/tyarith.tapl"

  override def parseInput(s: String): List[Command] =
    ArithParsers.input(s)

  def processCommand(ctx: Unit, cmd: Command): Unit =
    cmd match {
      case Eval(t1) =>
        val ty1 = Typer.typeof(t1)
        val doc1 = g2(ptmATerm(true, t1) ::: ":" :/: ptyTy(ty1) ::: ";")

        val t2 = eval(t1)
        val ty2 = Typer.typeof(t2)
        val doc2 = g2(ptmATerm(true, t2) ::: ":" :/: ptyTy(ty2) ::: ";")

        println("====================")
        println(print(doc1, width))
        println("""||""")
        println("""\/""")
        println(print(doc2, width))

    }

}
