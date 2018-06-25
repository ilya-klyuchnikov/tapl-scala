package tapl

trait DemoCL extends util.Demo {
  def main(args: Array[String]): Unit = {
    val inFile = if (args.isEmpty) defaultExample else args(0)
    val input = scala.io.Source.fromFile(inFile).mkString("")
    demo(input)
  }
}

object ArithDemo extends tapl.arith.ArithDemo with DemoCL
object BotDemo extends tapl.bot.BotDemo with DemoCL
object EquirecDemo extends tapl.equirec.EquirecDemo with DemoCL
object FullEquirecDemo extends tapl.fullequirec.FullEquirecDemo with DemoCL
object FullErrorDemo extends tapl.fullerror.FullErrorDemo with DemoCL
object FullFomSubDemo extends tapl.fullfomsub.FullFomSubDemo with DemoCL
object FullFomSubRefDemo extends tapl.fullfomsubref.FullFomSubRefDemo with DemoCL
object FullFSubDemo extends tapl.fullfsub.FullFSubDemo with DemoCL
object FullIsorecDemo extends tapl.fullisorec.FullIsorecDemo with DemoCL
object FullOmegaDemo extends tapl.fullomega.FullOmegaDemo with DemoCL
object FullPolyDemo extends tapl.fullpoly.FullPolyDemo with DemoCL
object FullReconDemo extends tapl.fullrecon.FullReconDemo with DemoCL
object FullRefDemo extends tapl.fullref.FullRefDemo with DemoCL
object FullSimpleDemo extends tapl.fullsimple.FullSimpleDemo with DemoCL
object FullSubDemo extends tapl.fullsub.FullSubDemo with DemoCL
object FullUntypedDemo extends tapl.fulluntyped.FullUntypedDemo with DemoCL
object RcdSubBotDemo extends tapl.rcdsubbot.RcdSubBotDemo with DemoCL
object ReconDemo extends tapl.recon.ReconDemo with DemoCL
object SimpleBoolDemo extends tapl.simplebool.SimpleBoolDemo with DemoCL
object TyArithDemo extends tapl.tyarith.TyArithDemo with DemoCL
object UntypedDemo extends tapl.untyped.UntypedDemo with DemoCL