package tapl.untyped

import org.scalacheck.{ Arbitrary, Gen }

// naive generator or terms
object TermGen {

  private def freshName(ctx: Context): (Context, String) =
    ctx.pickFreshName("v" + ctx.length)

  private def tmVar(ctx: Context): Gen[Term] =
    for { i <- Gen.oneOf(0 until ctx.length) } yield TmVar(i, ctx.length)

  private def tmApp(depth: Int, ctx: Context): Gen[Term] =
    for {
      i <- Gen.choose(1, depth - 1);
      t1 <- tm(i, ctx);
      t2 <- tm(depth - i, ctx)
    } yield TmApp(t1, t2)

  private def tmAbs(depth: Int, ctx: Context): Gen[Term] = {
    val (ctx1, v) = freshName(ctx)
    for { t1 <- tm(depth - 1, ctx1) } yield TmAbs(v, t1)
  }

  private def tm(depth: Int, ctx: Context): Gen[Term] =
    if (depth == 1)
      tmVar(ctx)
    else
      tmAbs(depth, ctx) | tmApp(depth, ctx)

  def terms: Gen[Term] =
    for {
      size <- Gen.choose(1, 10);
      t <- tm(size, Context())
    } yield t

}