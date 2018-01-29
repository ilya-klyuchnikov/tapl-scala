package tapl.simplebool

import org.scalacheck.Gen
import Gen._

// naive generator or terms which are possibly not well-typed
object TermGen {

  // The main generator of terms
  def terms: Gen[Term] =
    for {
      size <- choose(1, 30)
      t <- tm(size, Context())
    } yield t

  private def tm(depth: Int, ctx: Context): Gen[Term] =
    if (depth == 1 && ctx.length > 0) oneOf(tmVar(ctx), oneOf(TmTrue, TmFalse))
    else if (depth == 1) oneOf(TmTrue, TmFalse)
    else if (depth == 2) oneOf(tmAbs(depth, ctx), tmApp(depth, ctx))
    else oneOf(tmAbs(depth, ctx), tmApp(depth, ctx), tmIf(depth, ctx))

  private def tmVar(ctx: Context): Gen[Term] = for {
    i <- choose(1, ctx.length)
  } yield TmVar(i - 1, ctx.length)

  private def tmApp(depth: Int, ctx: Context): Gen[Term] =
    for {
      i <- choose(1, depth - 1)
      t1 <- tm(i, ctx)
      t2 <- tm(depth - i, ctx)
    } yield TmApp(t1, t2)

  private def tmAbs(depth: Int, ctx: Context): Gen[Term] =
    for {
      ty <- `type`
      (ctx1, v) = freshName(ctx)
      t1 <- tm(depth - 1, ctx1)
    } yield TmAbs(v, ty, t1)

  private def tmIf(depth: Int, ctx: Context): Gen[Term] =
    for {
      i <- choose(1, depth - 2)
      t1 <- tm(i, ctx)
      t2 <- tm(i, ctx)
      t3 <- tm(i, ctx)
    } yield TmIf(t1, t2, t3)

  private def freshName(ctx: Context): (Context, String) =
    ctx.pickFreshName("v" + ctx.length)

  private def `type`(): Gen[Ty] =
    for {
      i <- choose(1, 4)
      ty <- ty(i)
    } yield ty

  private def ty(depth: Int): Gen[Ty] =
    if (depth == 1 | depth == 2)
      TyBool
    else
      for {
        i <- choose(1, depth - 2)
        from <- ty(i)
        to <- ty(depth - i - 1)
      } yield TyArr(from, to)

}