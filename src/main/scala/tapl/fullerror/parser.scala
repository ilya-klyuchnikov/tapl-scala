package tapl.fullerror

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import util.PackratParsers

object FullErrorParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  import Binding._
  import Command._
  import Term._
  import Ty._

  lexical.reserved ++= Seq(
    "lambda",
    "Bool",
    "true",
    "false",
    "if",
    "then",
    "else",
    "_",
    "try",
    "with",
    "error",
    "Top",
    "Bot",
  )
  lexical.delimiters ++= Seq("(", ")", ";", "/", ".", ":", "->")

  // lower-case identifier
  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  // upper-case identifier
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  type Res[A] = Context => A
  type Res1[A] = Context => (A, Context)

  lazy val topLevel: PackratParser[Res1[List[Command]]] =
    ((command <~ ";") ~ topLevel) ^^ { case f ~ g =>
      (ctx: Context) =>
        val (cmd1, ctx1) = f(ctx)
        val (cmds, ctx2) = g(ctx1)
        (cmd1 :: cmds, ctx2)
    } | success { (ctx: Context) => (List(), ctx) }

  lazy val command: PackratParser[Res1[Command]] =
    lcid ~ binder ^^ { case id ~ bind =>
      (ctx: Context) => (Bind(id, bind(ctx)), ctx.addName(id))
    } |
      ucid ~ tyBinder ^^ { case id ~ bind =>
        (ctx: Context) => (Bind(id, bind(ctx)), ctx.addName(id))
      } |
      term ^^ { t => (ctx: Context) =>
        val t1 = t(ctx); (Eval(t1), ctx)
      }

  lazy val eof: PackratParser[String] = elem("<eof>", _ == lexical.EOF) ^^ { _.chars }
  lazy val binder: Parser[Context => Binding] =
    ":" ~> typ ^^ { ty => c => VarBind(ty(c)) }

  lazy val tyBinder: Parser[Context => Binding] =
    ("=" ~> typ) ^^ { ty => (ctx: Context) => TyAbbBind(ty(ctx)) } |
      success({ (ctx: Context) => TyVarBind })

  lazy val typ: PackratParser[Res[Ty]] = arrowType
  lazy val arrowType: PackratParser[Res[Ty]] =
    (aType <~ "->") ~ arrowType ^^ { case t1 ~ t2 => (ctx: Context) => TyArr(t1(ctx), t2(ctx)) } |
      aType
  lazy val aType: PackratParser[Res[Ty]] =
    "(" ~> typ <~ ")" |
      "Bool" ^^ { _ => (ctx: Context) => TyBool } |
      "Top" ^^ { _ => (ctx: Context) => TyTop } |
      "Bot" ^^ { _ => (ctx: Context) => TyBot }

  lazy val term: PackratParser[Res[Term]] =
    appTerm |
      ("lambda" ~> lcid) ~ (":" ~> typ) ~ ("." ~> term) ^^ { case v ~ ty ~ t =>
        (ctx: Context) => TmAbs(v, ty(ctx), t(ctx.addName(v)))
      } |
      ("lambda" ~ "_") ~> (":" ~> typ) ~ ("." ~> term) ^^ { case ty ~ t =>
        (ctx: Context) => TmAbs("_", ty(ctx), t(ctx.addName("_")))
      } |
      ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ { case t1 ~ t2 ~ t3 =>
        (ctx: Context) => TmIf(t1(ctx), t2(ctx), t3(ctx))
      } |
      ("try" ~> term) ~ ("with" ~> term) ^^ { case t1 ~ t2 =>
        (ctx: Context) => TmTry(t1(ctx), t2(ctx))
      }

  lazy val appTerm: PackratParser[Res[Term]] =
    (appTerm ~ aTerm) ^^ { case t1 ~ t2 => (ctx: Context) => TmApp(t1(ctx), t2(ctx)) } |
      aTerm

  lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> term <~ ")" |
      lcid ^^ { i => (ctx: Context) => TmVar(ctx.name2index(i), ctx.length) } |
      "true" ^^ { _ => (ctx: Context) => TmTrue } |
      "false" ^^ { _ => (ctx: Context) => TmFalse } |
      "error" ^^ { _ => (ctx: Context) => TmError }

  lazy val phraseTopLevel: PackratParser[Res1[List[Command]]] = phrase(topLevel)

  def input(s: String): Res1[List[Command]] =
    phraseTopLevel(new lexical.Scanner(s)) match {
      case t if t.successful => t.get
      case t                 => sys.error(t.toString)
    }
}
