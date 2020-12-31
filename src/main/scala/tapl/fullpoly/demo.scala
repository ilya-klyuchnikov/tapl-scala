package tapl.fullpoly

object FullPolyDemo extends util.Demo[Context, Command] {
  import Evaluator._
  import Typer._
  import Syntax._
  import util.Print._
  import PrettyPrinter._

  val width = 60

  override val initialContext: Context = Context()
  override val defaultExample: String = "examples/fullpoly.tapl"

  override def parseInput(s: String): List[Command] =
    FullPolyParsers.input(s)(Context())._1

  private def checkBinding(ctx: Context, bind: Binding): Binding =
    bind match {
      case NameBind =>
        NameBind
      case VarBind(tyT) =>
        VarBind(tyT)
      case TmAbbBind(t, None) =>
        TmAbbBind(t, Some(typeof(ctx, t)))
      case TmAbbBind(t, Some(tyT)) =>
        val tyT1 = typeof(ctx, t)
        if (tyEqv(ctx, tyT1, tyT))
          TmAbbBind(t, Some(tyT))
        else
          throw new Exception("type of binding doesn't match declared type in " + bind)
      case TyVarBind =>
        TyVarBind
      case TyAbbBind(tyT) =>
        TyAbbBind(tyT)
    }

  def processCommand(ctx: Context, cmd: Command): Context =
    cmd match {
      case Eval(t1) =>
        val ty1 = Typer.typeof(ctx, t1)
        val doc1 = g2(ptmATerm(true, ctx, t1) :: ":" :/: ptyTy(ctx, ty1) :: ";")

        val t2 = eval(ctx, t1)
        val ty2 = Typer.typeof(ctx, t2)
        val doc2 = g2(ptmATerm(true, ctx, t2) :: ":" :/: ptyTy(ctx, ty2) :: ";")

        println("====================")
        println(print(doc1, width))
        println("""||""")
        println("""\/""")
        println(print(doc2, width))

        ctx

      case Bind(x, bind) =>
        val bind1 = checkBinding(ctx, bind)
        val bind2 = evalBinding(ctx, bind1)
        val doc1 = x :: pBindingTy(ctx, bind2) :: ";"
        println("====================")
        println(print(doc1, width))

        ctx.addBinding(x, bind2)

      case SomeBind(tyX, x, t1) =>
        val tyT = typeof(ctx, t1)
        simplifyTy(ctx, tyT) match {
          case TySome(_, tyBody) =>
            val t2 = eval(ctx, t1)
            val b = t2 match {
              case TmPack(_, t12, _) => (TmAbbBind(termShift(1, t12), Some(tyBody)))
              case _                 => VarBind(tyBody)
            }
            val ctx1 = ctx.addBinding(tyX, TyVarBind)
            val ctx2 = ctx1.addBinding(x, b)

            println(tyX)
            val doc2 = x :/: ":" :/: ptyTy(ctx1, tyBody)
            println(doc2)
            ctx2
          case _ =>
            sys.error("existential type expected")
        }
    }

}
