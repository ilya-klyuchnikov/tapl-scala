package tapl.fullfomsubref

object FullFomSubRefDemo extends util.Demo[(Context, Store), Command] {
  import scala.language.implicitConversions
  import Evaluator._
  import Typer._
  import Syntax._
  import util.Print._, util.Print.text2doc
  import PrettyPrinter._
  import Binding._
  import Command._
  import Term._
  import Ty._

  val width = 60

  override val initialContext: (Context, Store) = (Context(), Store())
  override val defaultExample: String = "examples/fullfomsubref.tapl"

  override def parseInput(s: String): List[Command] =
    FullFomSubRefParsers.input(s)(Context())._1

  private def checkBinding(ctx: Context, bind: Binding): Binding =
    bind match {
      case NameBind =>
        NameBind
      case TyVarBind(tyS) =>
        kindof(ctx, tyS)
        TyVarBind(tyS)
      case TyAbbBind(tyT, None) =>
        TyAbbBind(tyT, Some(kindof(ctx, tyT)))
      case VarBind(tyT) =>
        VarBind(tyT)
      case TmAbbBind(t, None) =>
        TmAbbBind(t, Some(typeof(ctx, t)))
      case TmAbbBind(t, Some(tyT)) =>
        val tyT1 = typeof(ctx, t)
        if (subtype(ctx, tyT1, tyT))
          TmAbbBind(t, Some(tyT))
        else
          sys.error("type of binding doesn't match declared type in " + bind)
      case TyAbbBind(tyT, Some(knK)) =>
        val knK1 = kindof(ctx, tyT)
        if (knK == knK1) TyAbbBind(tyT, Some(knK))
        else throw new Exception("type of binding doesn't match declared type in " + bind)
    }

  def processCommand(in: (Context, Store), cmd: Command): (Context, Store) = {
    val (ctx, store) = in
    cmd match {
      case Eval(t1) =>
        val ty1 = Typer.typeof(ctx, t1)
        val doc1 = g2(ptmATerm(true, ctx, t1) ::: ":" :/: ptyTy(ctx, ty1) ::: ";")

        val (t2, store1) = eval(ctx, store, t1)
        val ty2 = Typer.typeof(ctx, t2)
        val doc2 = g2(ptmATerm(true, ctx, t2) ::: ":" :/: ptyTy(ctx, ty2) ::: ";")

        println("====================")
        println(print(doc1, width))
        println("""||""")
        println("""\/""")
        println(print(doc2, width))

        (ctx, store1)

      case Bind(x, bind) =>
        println()
        val bind1 = checkBinding(ctx, bind)
        val (bind2, store1) = evalBinding(ctx, store, bind1)
        val doc0 = x ::: pBinding(ctx, bind2) ::: ";"
        //val doc1 = x ::: pBindingTy(ctx, bind2) ::: ";"
        println("====================")
        println(print(doc0, width))
        //println(print(doc1, width))
        (ctx.addBinding(x, bind2), store1.shift(1))

      case SomeBind(tyX, x, t1) =>
        val tyT = typeof(ctx, t1)
        simplifyTy(ctx, tyT) match {
          case TySome(_, knK, tyBody) =>
            val (t2, store1) = eval(ctx, store, t1)
            val b = t2 match {
              case TmPack(_, t12, _) => (TmAbbBind(termShift(1, t12), Some(tyBody)))
              case _                 => VarBind(tyBody)
            }
            val ctx1 = ctx.addBinding(tyX, TyVarBind(knK))
            val ctx2 = ctx1.addBinding(x, b)

            println(tyX)
            val doc2 = x :/: ":" :/: ptyTy(ctx1, tyBody)
            println(doc2)
            (ctx2, store1)
          case _ =>
            sys.error("existential type expected")
        }
    }
  }

}
