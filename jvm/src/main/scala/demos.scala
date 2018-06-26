package tapl

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.generic.semiauto._

trait DemoCL extends util.Demo {
  implicit val enc: Encoder[Cmd]
  def main(args: Array[String]): Unit = {
    var myArgs = args

    val dumpAst = args.contains("--dump-ast")
    myArgs = args.filterNot("--dump-ast".equals)

    val inFile = if (myArgs.isEmpty) defaultExample else myArgs(0)
    val input = scala.io.Source.fromFile(inFile).mkString("")

    if (dumpAst)
      println(parseInput(input).asJson.noSpaces)
    else
      demo(input)
  }
}

// TODO = use a macro for this stuff
object ArithDemo extends arith.ArithDemo with DemoCL { override implicit val enc = deriveEncoder }
object BotDemo extends tapl.bot.BotDemo with DemoCL { override implicit val enc = deriveEncoder }
object EquirecDemo extends tapl.equirec.EquirecDemo with DemoCL { override implicit val enc = deriveEncoder }
object FullEquirecDemo extends tapl.fullequirec.FullEquirecDemo with DemoCL { override implicit val enc = deriveEncoder }
object FullErrorDemo extends tapl.fullerror.FullErrorDemo with DemoCL { override implicit val enc = deriveEncoder }
object FullFomSubDemo extends tapl.fullfomsub.FullFomSubDemo with DemoCL { override implicit val enc = deriveEncoder }
object FullFomSubRefDemo extends tapl.fullfomsubref.FullFomSubRefDemo with DemoCL { override implicit val enc = deriveEncoder }
object FullFSubDemo extends tapl.fullfsub.FullFSubDemo with DemoCL { override implicit val enc = deriveEncoder }
object FullIsorecDemo extends tapl.fullisorec.FullIsorecDemo with DemoCL { override implicit val enc = deriveEncoder }
object FullOmegaDemo extends tapl.fullomega.FullOmegaDemo with DemoCL { override implicit val enc = deriveEncoder }
object FullPolyDemo extends tapl.fullpoly.FullPolyDemo with DemoCL { override implicit val enc = deriveEncoder }
object FullReconDemo extends tapl.fullrecon.FullReconDemo with DemoCL { override implicit val enc = deriveEncoder }
object FullRefDemo extends tapl.fullref.FullRefDemo with DemoCL { override implicit val enc = deriveEncoder }
object FullSimpleDemo extends tapl.fullsimple.FullSimpleDemo with DemoCL { override implicit val enc = deriveEncoder }
object FullSubDemo extends tapl.fullsub.FullSubDemo with DemoCL { override implicit val enc = deriveEncoder }
object FullUntypedDemo extends tapl.fulluntyped.FullUntypedDemo with DemoCL { override implicit val enc = deriveEncoder }
object RcdSubBotDemo extends tapl.rcdsubbot.RcdSubBotDemo with DemoCL { override implicit val enc = deriveEncoder }
object ReconDemo extends tapl.recon.ReconDemo with DemoCL { override implicit val enc = deriveEncoder }
object SimpleBoolDemo extends tapl.simplebool.SimpleBoolDemo with DemoCL { override implicit val enc = deriveEncoder }
object TyArithDemo extends tapl.tyarith.TyArithDemo with DemoCL { override implicit val enc = deriveEncoder }
object UntypedDemo extends tapl.untyped.UntypedDemo with DemoCL { override implicit val enc = deriveEncoder }
