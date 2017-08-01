package tapl.fullfomsub

class FullFomSubDemo extends util.Demo {

  override type Ctx = Context
  override type Cmd = Command

  import Evaluator._
  import Typer._
  import Syntax._
  import util.Print._
  import PrettyPrinter._

  val width = 60

  override val initialContext: Ctx = Context()
  override val defaultExample: String = "examples/fullfomsub.tapl"
  override val name: String = "FullFomSub"

  override def parseInput(s: String): List[Cmd] =
    FullFomSubParsers.input(s)(Context())._1

  private def checkBinding(ctx: Ctx, bind: Binding): Binding = bind match {
    case NameBind =>
      NameBind
    case VarBind(tyT) =>
      VarBind(tyT)
    case TmAbbBind(t, None) =>
      TmAbbBind(t, Some(typeof(ctx, t)))
    case TmAbbBind(t, Some(tyT)) =>
      val tyT1 = typeof(ctx, t)
      if (subtype(ctx, tyT1, tyT))
        TmAbbBind(t, Some(tyT))
      else
        throw new Exception("type of binding doesn't match declared type in " + bind)
    case TyVarBind(tyS) =>
      TyVarBind(tyS)
    case TyAbbBind(tyT, None) =>
      TyAbbBind(tyT, Some(kindof(ctx, tyT)))
    case TyAbbBind(tyT, Some(knK)) =>
      val knK1 = kindof(ctx, tyT)
      if (knK == knK1) TyAbbBind(tyT, Some(knK))
      else throw new Exception("type of binding doesn't match declared type in " + bind)
  }

  def processCommand(ctx: Ctx, cmd: Cmd): Ctx = cmd match {
    case Eval(t1) =>
      val ty1 = Typer.typeof(ctx, t1)
      val doc1 = g2(ptmATerm(true, ctx, t1) :: ":" :/: ptyTy(ctx, ty1) :: ";")

      val t2 = eval(ctx, t1)
      val ty2 = Typer.typeof(ctx, t2)
      val doc2 = g2(ptmATerm(true, ctx, t2) :: ":" :/: ptyTy(ctx, ty2) :: ";")

      output("====================")
      output(print(doc1, width))
      output("""||""")
      output("""\/""")
      output(print(doc2, width))

      ctx

    case Bind(x, bind) =>
      val bind1 = checkBinding(ctx, bind)
      val bind2 = evalBinding(ctx, bind1)
      val doc1 = x :: pBindingTy(ctx, bind2) :: ";"
      output("====================")
      output(print(doc1, width))

      ctx.addBinding(x, bind2)

    case SomeBind(tyX, x, t1) =>
      val tyT = typeof(ctx, t1)
      lcst(ctx, tyT) match {
        case TySome(_, tyBound, tyBody) =>
          val t2 = eval(ctx, t1)
          val b = t2 match {
            case TmPack(_, t12, _) => (TmAbbBind(termShift(1, t12), Some(tyBody)))
            case _                 => VarBind(tyBody)
          }
          val ctx1 = ctx.addBinding(tyX, TyVarBind(tyBound))
          val ctx2 = ctx1.addBinding(x, b)

          output(tyX)
          val doc2 = x :/: ":" :/: ptyTy(ctx1, tyBody)
          output(print(doc2, width))
          ctx2
        case _ =>
          sys.error("existential type expected")
      }
  }

}

object FullFomSubDemo extends FullFomSubDemo with util.DemoCL
object FullFomSubDemoJS extends FullFomSubDemo with util.DemoJS
