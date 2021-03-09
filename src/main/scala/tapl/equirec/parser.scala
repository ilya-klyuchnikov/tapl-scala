package tapl.equirec

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import util.PackratParsers

object EquirecParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  import Binding._
  import Command._
  import Term._
  import Ty._

  lexical.reserved ++= Seq("lambda", "Rec", "_")
  lexical.delimiters ++= Seq("(", ")", ";", ".", ":", "->", "=")

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
      ucid ~ tyBinder ^^ {
        case id ~ bind => (ctx: Context) => (Bind(id, bind(ctx)), ctx.addName(id))
      } |
      term ^^ { t => (ctx: Context) =>
        val t1 = t(ctx); (Eval(t1), ctx)
      }

  lazy val binder: Parser[Context => Binding] =
    ":" ~> typ ^^ { t => (ctx: Context) => VarBind(t(ctx)) }
  lazy val tyBinder: Parser[Context => Binding] =
    success({ (ctx: Context) => TyVarBind })

  // TYPES
  lazy val typ: PackratParser[Res[Ty]] =
    arrowType |
      ("Rec" ~> ucid) ~ ("." ~> typ) ^^ {
        case id ~ ty => (ctx: Context) => TyRec(id, ty(ctx.addName(id)))
      }

  lazy val aType: PackratParser[Res[Ty]] =
    "(" ~> typ <~ ")" |
      ucid ^^ { tn => (ctx: Context) =>
        if (ctx.isNameBound(tn)) TyVar(ctx.name2index(tn), ctx.length) else TyId(tn)
      }

  lazy val arrowType: PackratParser[Res[Ty]] =
    (aType <~ "->") ~ arrowType ^^ { case t1 ~ t2 => (ctx: Context) => TyArr(t1(ctx), t2(ctx)) } |
      aType

  // TERMS
  lazy val term: PackratParser[Res[Term]] =
    appTerm |
      (("lambda" ~ "_") ~> (":" ~> typ)) ~ ("." ~> term) ^^ {
        case ty ~ t => (ctx: Context) => TmAbs("_", ty(ctx), t(ctx.addName("_")))
      } |
      ("lambda" ~> lcid) ~ (":" ~> typ) ~ ("." ~> term) ^^ {
        case v ~ ty ~ t => (ctx: Context) => TmAbs(v, ty(ctx), t(ctx.addName(v)))
      }

  lazy val appTerm: PackratParser[Res[Term]] =
    appTerm ~ aTerm ^^ { case t1 ~ t2 => (ctx: Context) => TmApp(t1(ctx), t2(ctx)) } |
      aTerm

  lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> term <~ ")" |
      lcid ^^ { i => (ctx: Context) => TmVar(ctx.name2index(i), ctx.length) }

  lazy val phraseTopLevel: PackratParser[Res1[List[Command]]] = phrase(topLevel)

  def input(s: String): Res1[List[Command]] =
    phraseTopLevel(new lexical.Scanner(s)) match {
      case t if t.successful => t.get
      case t                 => sys.error(t.toString)
    }

}
