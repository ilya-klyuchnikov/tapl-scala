package tapl.arith

import tapl.grammars.ArithParser._
import tapl.grammars.ArithBaseListener
import org.antlr.v4.runtime.tree.{ParseTree, ParseTreeProperty}

import scala.jdk.CollectionConverters._

class ArithAstListener extends ArithBaseListener {
  private val commands =
    new ParseTreeProperty[Command]
  private val terms =
    new ParseTreeProperty[Term]
  var program: List[Command] =
    List()

  private def getCommand(pt: ParseTree): Command =
    commands.get(pt)
  private def putCommand(pt: ParseTree, cmd: Command): Unit =
    commands.put(pt, cmd)
  private def getTerm(pt: ParseTree): Term =
    terms.get(pt)
  private def putTerm(pt: ParseTree, t: Term): Unit =
    terms.put(pt, t)

  override def exitProgram(ctx: ProgramContext): Unit =
    program = ctx.cmds.asScala.map(getCommand).toList

  override def exitCommandEval(ctx: CommandEvalContext): Unit =
    putCommand(ctx, Eval(getTerm(ctx.t)))

  override def exitTmAtomic(ctx: TmAtomicContext): Unit =
    putTerm(ctx, getTerm(ctx.t))

  override def exitTmSucc(ctx: TmSuccContext): Unit =
    putTerm(ctx, TmSucc(getTerm(ctx.t)))

  override def exitTmPred(ctx: TmPredContext): Unit =
    putTerm(ctx, TmPred(getTerm(ctx.t)))

  override def exitTmIszero(ctx: TmIszeroContext): Unit =
    putTerm(ctx, TmIsZero(getTerm(ctx.t)))

  override def exitTmIf(ctx: TmIfContext): Unit =
    putTerm(ctx, TmIf(getTerm(ctx.cond), getTerm(ctx.t1), getTerm(ctx.t2)))

  override def exitTmParens(ctx: TmParensContext): Unit =
    putTerm(ctx, getTerm(ctx.inner))

  override def exitTmFalse(ctx: TmFalseContext): Unit =
    putTerm(ctx, TmFalse)

  override def exitTmTrue(ctx: TmTrueContext): Unit =
    putTerm(ctx, TmTrue)

  override def exitTmNum(ctx: TmNumContext): Unit =
    putTerm(ctx, num(ctx.num.getText.toInt))

  private def num(x: Int): Term =
    x match {
      case 0 => TmZero
      case _ => TmSucc(num(x - 1))
    }
}
