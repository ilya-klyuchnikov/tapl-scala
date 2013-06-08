package tapl.rcdsubbot

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object RcdSubBotParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.reserved += ("lambda", "Bool", "true", "false", "if", "then", "else",
    "Nat", "String", "Unit", "Float", "unit", "case", "let", "in", "succ", "pred",
    "as", "of", "fix", "iszero", "letrec", "_", "Top", "Bot")
  lexical.delimiters += ("(", ")", ";", "/", ".", ":", "->", "=", "<", ">", "{", "}", "=>", "==>", ",", "|")

  // lower-case identifier
  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  // upper-case identifier
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }
  lazy val eof: PackratParser[String] = elem("<eof>", _ == lexical.EOF) ^^ { _.chars }

  type Res[A] = Context => A
  type Res1[A] = Context => (A, Context)

  lazy val topLevel: PackratParser[Res1[List[Command]]] =
    ((command <~ ";") ~ topLevel) ^^ {
      case f ~ g => ctx: Context =>
        val (cmd1, ctx1) = f(ctx)
        val (cmds, ctx2) = g(ctx1)
        (cmd1 :: cmds, ctx2)
    } | success{ctx: Context => (List(), ctx)}

  lazy val command: PackratParser[Res1[Command]] =
    lcid ~ binder ^^ { case id ~ bind => ctx: Context => (Bind(id, bind(ctx)), ctx.addName(id)) } |
      term ^^ { t => ctx: Context => val t1 = t(ctx); (Eval(t1), ctx) }

  lazy val binder: Parser[Context => Binding] =
    ":" ~> `type` ^^ { t => ctx: Context => VarBind(t(ctx)) }

  lazy val `type`: PackratParser[Res[Ty]] = arrowType
  lazy val aType: PackratParser[Res[Ty]] =
    "(" ~> `type` <~ ")" |
      "Bot" ^^ { _ => ctx: Context => TyBot } |
      "Top" ^^ { _ => ctx: Context => TyTop } |
      "{" ~> fieldTypes <~ "}" ^^ { ft => ctx: Context => TyRecord(ft(ctx)) }

  lazy val fieldTypes: PackratParser[Res[List[(String, Ty)]]] =
    repsep(fieldType, ",") ^^ { fs => ctx: Context => fs.zipWithIndex.map { case (ft, i) => ft(ctx, i + 1) } }

  lazy val fieldType: PackratParser[(Context, Int) => (String, Ty)] =
    lcid ~ (":" ~> `type`) ^^ { case id ~ ty => (ctx: Context, i: Int) => (id, ty(ctx)) } |
      `type` ^^ { ty => (ctx: Context, i: Int) => (i.toString, ty(ctx)) }

  lazy val arrowType: PackratParser[Res[Ty]] =
    (aType <~ "->") ~ arrowType ^^ { case t1 ~ t2 => ctx: Context => TyArr(t1(ctx), t2(ctx)) } |
      aType

  // TERMS
  lazy val term: PackratParser[Res[Term]] =
    appTerm |
      ("lambda" ~> lcid) ~ (":" ~> `type`) ~ ("." ~> term) ^^ { case v ~ ty ~ t => ctx: Context => TmAbs(v, ty(ctx), t(ctx.addName(v))) } |
      ("lambda" ~ "_") ~> (":" ~> `type`) ~ ("." ~> term) ^^ { case ty ~ t => ctx: Context => TmAbs("_", ty(ctx), t(ctx.addName("_"))) }
  lazy val appTerm: PackratParser[Res[Term]] =
    appTerm ~ pathTerm ^^ { case t1 ~ t2 => ctx: Context => TmApp(t1(ctx), t2(ctx)) } |
      pathTerm

  lazy val pathTerm: PackratParser[Res[Term]] =
    pathTerm ~ ("." ~> lcid) ^^ { case t1 ~ l => ctx: Context => TmProj(t1(ctx), l) } |
      pathTerm ~ ("." ~> numericLit) ^^ { case t1 ~ l => ctx: Context => TmProj(t1(ctx), l) } |
      aTerm

  lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> term <~ ")" |
      lcid ^^ { i => ctx: Context => TmVar(ctx.name2index(i), ctx.length) } |
      "{" ~> fields <~ "}" ^^ { fs => ctx: Context => TmRecord(fs(ctx)) }

  lazy val fields: PackratParser[Res[List[(String, Term)]]] =
    repsep(field, ",") ^^ { fs => ctx: Context => fs.zipWithIndex.map { case (ft, i) => ft(ctx, i + 1) } }
  lazy val field: PackratParser[(Context, Int) => (String, Term)] =
    lcid ~ ("=" ~> term) ^^ { case id ~ t => (ctx: Context, i: Int) => (id, t(ctx)) } |
      term ^^ { t => (ctx: Context, i: Int) => (i.toString, t(ctx)) }

  def input(s: String) = phrase(topLevel)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t                 => error(t.toString)
  }

}