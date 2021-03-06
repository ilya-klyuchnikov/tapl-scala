package tapl.fulluntyped

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object FullUntypedParsers
    extends StandardTokenParsers
    with PackratParsers
    with ImplicitConversions {
  import Binding._
  import Command._
  import Term._

  lexical.reserved ++= Seq(
    "lambda",
    "Bool",
    "true",
    "false",
    "if",
    "then",
    "else",
    "Nat",
    "String",
    "Unit",
    "Float",
    "unit",
    "case",
    "let",
    "in",
    "succ",
    "pred",
    "as",
    "of",
    "iszero",
    "letrec",
    "_",
  )
  lexical.delimiters ++= Seq(
    "(",
    ")",
    ";",
    "/",
    ".",
    ":",
    "->",
    "=",
    "<",
    ">",
    "{",
    "}",
    "=>",
    "==>",
    ",",
    "|",
    "\\",
  )

  // lower-case identifier
  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  // upper-case identifier
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }
  lazy val eof: PackratParser[String] = elem("<eof>", _ == lexical.EOF) ^^ { _.chars }

  type Res[A] = Context => A
  type Res1[A] = Context => (A, Context)

  lazy val topLevel: PackratParser[Res1[List[Command]]] =
    ((command <~ ";") ~ topLevel) ^^ {
      case f ~ g =>
        (ctx: Context) =>
          val (cmd1, ctx1) = f(ctx)
          val (cmds, ctx2) = g(ctx1)
          (cmd1 :: cmds, ctx2)
    } | success { (ctx: Context) => (List(), ctx) }

  lazy val command: PackratParser[Res1[Command]] =
    lcid ~ binder ^^ { case id ~ bind => (ctx: Context) => (Bind(id, bind(ctx)), ctx.addName(id)) } |
      term ^^ { t => (ctx: Context) =>
        val t1 = t(ctx); (Eval(t1), ctx)
      }

  lazy val binder: Parser[Context => Binding] =
    "/" ^^ { _ => (ctx: Context) => NameBind } |
      "=" ~> term ^^ { t => (ctx: Context) => TmAbbBind(t(ctx)) }

  lazy val term: PackratParser[Res[Term]] =
    appTerm |
      ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ {
        case t1 ~ t2 ~ t3 => (ctx: Context) => TmIf(t1(ctx), t2(ctx), t3(ctx))
      } |
      ("\\" ~> lcid) ~ ("." ~> term) ^^ {
        case v ~ t => (ctx: Context) => TmAbs(v, t(ctx.addName(v)))
      } |
      ("lambda" ~> lcid) ~ ("." ~> term) ^^ {
        case v ~ t => (ctx: Context) => TmAbs(v, t(ctx.addName(v)))
      } |
      ("lambda" ~ "_") ~> ("." ~> term) ^^ { t => (ctx: Context) =>
        TmAbs("_", t(ctx.addName("_")))
      } |
      ("let" ~> lcid) ~ ("=" ~> term) ~ ("in" ~> term) ^^ {
        case id ~ t1 ~ t2 => (ctx: Context) => TmLet(id, t1(ctx), t2(ctx.addName(id)))
      } |
      ("let" ~ "_") ~> ("=" ~> term) ~ ("in" ~> term) ^^ {
        case t1 ~ t2 => (ctx: Context) => TmLet("_", t1(ctx), t2(ctx.addName("_")))
      }
  lazy val appTerm: PackratParser[Res[Term]] =
    appTerm ~ pathTerm ^^ { case t1 ~ t2 => (ctx: Context) => TmApp(t1(ctx), t2(ctx)) } |
      "succ" ~> pathTerm ^^ { t => (ctx: Context) => TmSucc(t(ctx)) } |
      "pred" ~> pathTerm ^^ { t => (ctx: Context) => TmPred(t(ctx)) } |
      "iszero" ~> pathTerm ^^ { t => (ctx: Context) => TmIsZero(t(ctx)) } |
      pathTerm

  lazy val pathTerm: PackratParser[Res[Term]] =
    pathTerm ~ ("." ~> lcid) ^^ { case t1 ~ l => (ctx: Context) => TmProj(t1(ctx), l) } |
      pathTerm ~ ("." ~> numericLit) ^^ { case t1 ~ l => (ctx: Context) => TmProj(t1(ctx), l) } |
      aTerm

  lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> term <~ ")" |
      "true" ^^ { _ => (ctx: Context) => TmTrue } |
      "false" ^^ { _ => (ctx: Context) => TmFalse } |
      lcid ^^ { i => (ctx: Context) => TmVar(ctx.name2index(i), ctx.length) } |
      stringLit ^^ { l => (ctx: Context) => TmString(l) } |
      "{" ~> fields <~ "}" ^^ { fs => (ctx: Context) => TmRecord(fs(ctx)) } |
      numericLit ^^ { x => (ctx: Context) => num(x.toInt) }

  lazy val fields: PackratParser[Res[List[(String, Term)]]] =
    repsep(field, ",") ^^ { fs => (ctx: Context) =>
      fs.zipWithIndex.map { case (ft, i) => ft(ctx, i + 1) }
    }
  lazy val field: PackratParser[(Context, Int) => (String, Term)] =
    lcid ~ ("=" ~> term) ^^ { case id ~ t => (ctx: Context, i: Int) => (id, t(ctx)) } |
      term ^^ { t => (ctx: Context, i: Int) => (i.toString, t(ctx)) }

  private def num(x: Int): Term =
    x match {
      case 0 => TmZero
      case _ => TmSucc(num(x - 1))
    }

  lazy val phraseTopLevel: PackratParser[Res1[List[Command]]] = phrase(topLevel)

  def input(s: String): Res1[List[Command]] =
    phraseTopLevel(new lexical.Scanner(s)) match {
      case t if t.successful => t.get
      case t                 => sys.error(t.toString)
    }

}
