package tapl.simplebool

import org.scalacheck.{ Arbitrary, Gen }
import Gen._

// naive generator or terms which are possibly not well-typed
object TermGen {

  private def freshName(ctx: Context): (Context, String) =
    ctx.pickFreshName("v" + ctx.length)

  private def tmVar(ctx: Context): Gen[Term] =
    for { i <- Gen.oneOf(0 until ctx.length) } yield TmVar(i, ctx.length)

  private def tmApp(depth: Int, ctx: Context): Gen[Term] =
    for {
      i <- Gen.choose(1, depth - 1)
      t1 <- tm(i, ctx)
      t2 <- tm(i, ctx)
    } yield TmApp(t1, t2)

  private def tmAbs(depth: Int, ctx: Context): Gen[Term] =
    for {
      i <- Gen.choose(1, depth - 1)
      (ctx1, v) = freshName(ctx)
      t1 <- tm(i, ctx1)
      ty <- `type`
    } yield TmAbs(v, ty, t1)

  private def tmIf(depth: Int, ctx: Context): Gen[Term] =
    for {
      i <- Gen.choose(1, depth - 1)
      t1 <- tm(i, ctx)
      t2 <- tm(i, ctx)
      t3 <- tm(i, ctx)
    } yield TmIf(t1, t2, t3)

  private def tm(depth: Int, ctx: Context): Gen[Term] =
    if (depth == 1) tmVar(ctx) | TmTrue | TmFalse
    else tmAbs(depth, ctx) | tmApp(depth, ctx) | tmIf(depth, ctx)

  private def `type`(): Gen[Ty] =
    for {
      i <- Gen.choose(1, 10)
      ty <- arrowType(i)
    } yield ty

  private def arrowType(depth: Int): Gen[Ty] =
    if (depth == 1)
      TyBool
    else
      for {
        i <- Gen.choose(1, depth - 1)
        from <- arrowType(i)
        to <- arrowType(depth - i)
      } yield TyArr(from, to)

  def terms: Gen[Term] =
    for {
      size <- Gen.choose(1, 3);
      t <- tm(size, Context())
    } yield t

}