package tapl.untyped

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import util.PackratParsers

// This parser is done exactly in the same way as in TAPL.
// The oddity of this parser (elementary parsers return functions) is driven by the desire
// to avoid double hierarchy of terms (named and nameless).
// The input text represents named terms. The module works with nameless terms
// So translation from named form into nameless form is done on the fly during parsing.
object UntypedParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  import Binding._
  import Command._
  import Term._

  lexical.reserved ++= Seq("_")
  lexical.delimiters ++= Seq("(", ")", ";", "/", ".", "\\")

  // lower-case identifier
  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  // upper-case identifier
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

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

  lazy val eof: PackratParser[String] = elem("<eof>", _ == lexical.EOF) ^^ { _.chars }
  lazy val binder: Parser[Context => Binding] =
    "/" ^^ { _ => c => NameBind }

  lazy val term: PackratParser[Res[Term]] =
    appTerm |
      ("\\" ~> lcid) ~ ("." ~> term) ^^ {
        case v ~ t => (ctx: Context) => TmAbs(v, t(ctx.addName(v)))
      } |
      ("\\" ~ "_") ~> ("." ~> term) ^^ { t => (ctx: Context) => TmAbs("_", t(ctx.addName("_"))) }

  lazy val appTerm: PackratParser[Res[Term]] =
    (appTerm ~ aTerm) ^^ { case t1 ~ t2 => (ctx: Context) => TmApp(t1(ctx), t2(ctx)) } | aTerm

  lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> term <~ ")" |
      lcid ^^ { i => (ctx: Context) => TmVar(ctx.name2index(i), ctx.length) }

  lazy val phraseTerm: PackratParser[Res[Term]] =
    phrase(term)

  lazy val phraseTopLevel: PackratParser[Res1[List[Command]]] =
    phrase(topLevel)

  def inputTerm(s: String): Res[Term] =
    phraseTerm(new lexical.Scanner(s)) match {
      case t if t.successful => t.get
      case t                 => sys.error(t.toString)
    }

  def input(s: String): Res1[List[Command]] =
    phraseTopLevel(new lexical.Scanner(s)) match {
      case t if t.successful => t.get
      case t                 => sys.error(t.toString)
    }
}
