package tapl.bot

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object BotParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  import Binding._
  import Command._
  import Term._
  import Ty._

  lexical.reserved ++=
    Seq("lambda", "Top", "Bot")
  lexical.delimiters ++=
    Seq("(", ")", ";", ".", ":", "->")

  private lazy val lcid: PackratParser[String] =
    ident ^? { case id if id.charAt(0).isLower => id }
  private lazy val ucid: PackratParser[String] =
    ident ^? { case id if id.charAt(0).isUpper => id }
  private lazy val eof: PackratParser[String] =
    elem("<eof>", _ == lexical.EOF) ^^ { _.chars }

  type Res[A] = Context => A
  type Res1[A] = Context => (A, Context)

  private lazy val topLevel: PackratParser[Res1[List[Command]]] =
    ((command <~ ";") ~ topLevel) ^^ {
      case f ~ g =>
        (ctx: Context) =>
          val (cmd1, ctx1) = f(ctx)
          val (cmds, ctx2) = g(ctx1)
          (cmd1 :: cmds, ctx2)
    } | success { (ctx: Context) => (List(), ctx) }

  private lazy val command: PackratParser[Res1[Command]] =
    lcid ~ binder ^^ { case id ~ bind => (ctx: Context) => (Bind(id, bind(ctx)), ctx.addName(id)) } |
      term ^^ { t => (ctx: Context) =>
        val t1 = t(ctx); (Eval(t1), ctx)
      }

  private lazy val binder: PackratParser[Res[Binding]] =
    ":" ~> typ ^^ { t => (ctx: Context) => VarBind(t(ctx)) }

  private lazy val typ: PackratParser[Res[Ty]] =
    arrowType
  private lazy val aType: PackratParser[Res[Ty]] =
    "(" ~> typ <~ ")" |
      "Bot" ^^ { _ => (ctx: Context) => TyBot } |
      "Top" ^^ { _ => (ctx: Context) => TyTop }

  private lazy val arrowType: PackratParser[Res[Ty]] =
    (aType <~ "->") ~ arrowType ^^ { case t1 ~ t2 => (ctx: Context) => TyArr(t1(ctx), t2(ctx)) } |
      aType

  private lazy val term: PackratParser[Res[Term]] =
    appTerm |
      ("lambda" ~> lcid) ~ (":" ~> typ) ~ ("." ~> term) ^^ {
        case v ~ ty ~ t => (ctx: Context) => TmAbs(v, ty(ctx), t(ctx.addName(v)))
      } |
      ("lambda" ~ "_") ~> (":" ~> typ) ~ ("." ~> term) ^^ {
        case ty ~ t => (ctx: Context) => TmAbs("_", ty(ctx), t(ctx.addName("_")))
      }
  private lazy val appTerm: PackratParser[Res[Term]] =
    appTerm ~ aTerm ^^ { case t1 ~ t2 => (ctx: Context) => TmApp(t1(ctx), t2(ctx)) } |
      aTerm

  private lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> term <~ ")" |
      lcid ^^ { i => (ctx: Context) => TmVar(ctx.name2index(i), ctx.length) }

  private lazy val phraseTopLevel: PackratParser[Res1[List[Command]]] =
    phrase(topLevel)

  def input(s: String): Res1[List[Command]] =
    phraseTopLevel(new lexical.Scanner(s)) match {
      case t if t.successful => t.get
      case t                 => sys.error(t.toString)
    }
}
