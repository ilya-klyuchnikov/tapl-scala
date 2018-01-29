package tapl.untyped

import org.scalacheck.Gen
import Gen._

// naive generator or terms
object TermGen {

  private def freshName(ctx: Context): (Context, String) =
    ctx.pickFreshName("v" + ctx.length)

  private def tmVar(ctx: Context): Gen[Term] =
    if (ctx.length == 0) Gen.fail else
    for { i <- Gen.choose(1, ctx.length) } yield TmVar(i - 1, ctx.length)

  private def tmApp(depth: Int, ctx: Context): Gen[Term] =
    for {
      i1 <- Gen.choose(1, depth - 1)
      i2 <- Gen.choose(1, depth - 1)
      t1 <- tm(i1, ctx)
      t2 <- tm(i2, ctx)
    } yield TmApp(t1, t2)

  private def tmAbs(depth: Int, ctx: Context): Gen[Term] = {
    val (ctx1, v) = freshName(ctx)
    for {
      i <- Gen.choose(1, depth - 1)
      t1 <- tm(i, ctx1)
    } yield TmAbs(v, t1)
  }

  private def tm(depth: Int, ctx: Context): Gen[Term] = depth match {
    case d if d <= 0 => Gen.fail
    case 1           => tmVar(ctx)
    case 2           => tmVar(ctx)
    case _           => oneOf(tmApp(depth, ctx), tmAbs(depth, ctx))
  }

  def terms: Gen[Term] =
    for {
      size <- Gen.choose(1, 10)
      t <- tm(size, Context())
    } yield t

}