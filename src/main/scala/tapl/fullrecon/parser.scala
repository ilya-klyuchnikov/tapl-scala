package tapl.fullrecon

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object FullReconParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.reserved ++= Seq("lambda", "Bool", "true", "false", "if", "then", "else",
    "Nat", "String", "Unit", "Float", "unit", "case", "let", "in", "succ", "pred",
    "as", "of", "fix", "iszero", "letrec", "_")
  lexical.delimiters ++= Seq("(", ")", ";", "/", ".", ":", "->", "=", "<", ">", "{", "}", "=>", "==>", ",", "|")

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

  // TYPES
  lazy val `type`: PackratParser[Res[Ty]] = arrowType
  lazy val aType: PackratParser[Res[Ty]] =
    "(" ~> `type` <~ ")" |
      ucid ^^ { tn => ctx: Context => TyId(tn) } |
      "Bool" ^^ { _ => ctx: Context => TyBool } |
      "Nat" ^^ { _ => ctx: Context => TyNat }

  lazy val fieldTypes: PackratParser[Res[List[(String, Ty)]]] =
    repsep(fieldType, ",") ^^ { fs => ctx: Context => fs.zipWithIndex.map { case (ft, i) => ft(ctx, i + 1) } }

  lazy val fieldType: PackratParser[(Context, Int) => (String, Ty)] =
    lcid ~ (":" ~> `type`) ^^ { case id ~ ty => (ctx: Context, i: Int) => (id, ty(ctx)) } |
      `type` ^^ { ty => (ctx: Context, i: Int) => (i.toString, ty(ctx)) }

  lazy val arrowType: PackratParser[Res[Ty]] =
    (aType <~ "->") ~ arrowType ^^ { case t1 ~ t2 => ctx: Context => TyArr(t1(ctx), t2(ctx)) } |
      aType

  lazy val term: PackratParser[Res[Term]] =
    appTerm |
      ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ { case t1 ~ t2 ~ t3 => ctx: Context => TmIf(t1(ctx), t2(ctx), t3(ctx)) } |
      ("lambda" ~> lcid) ~ ("." ~> term) ^^ { case v ~ t => ctx: Context => TmAbs(v, None, t(ctx.addName(v))) } |
      ("lambda" ~> lcid) ~ (":" ~> `type`) ~ ("." ~> term) ^^ { case v ~ ty ~ t => ctx: Context => TmAbs(v, Some(ty(ctx)), t(ctx.addName(v))) } |
      ("lambda" ~ "_") ~> (":" ~> `type`) ~ ("." ~> term) ^^ { case ty ~ t => ctx: Context => TmAbs("_", Some(ty(ctx)), t(ctx.addName("_"))) } |
      ("let" ~> lcid) ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case id ~ t1 ~ t2 => ctx: Context => TmLet(id, t1(ctx), t2(ctx.addName(id))) } |
      ("let" ~ "_") ~> ("=" ~> term) ~ ("in" ~> term) ^^ { case t1 ~ t2 => ctx: Context => TmLet("_", t1(ctx), t2(ctx.addName("_"))) }

  lazy val appTerm: PackratParser[Res[Term]] =
    appTerm ~ aTerm ^^ { case t1 ~ t2 => ctx: Context => TmApp(t1(ctx), t2(ctx)) } |
      "succ" ~> aTerm ^^ { t => ctx: Context => TmSucc(t(ctx)) } |
      "pred" ~> aTerm ^^ { t => ctx: Context => TmPred(t(ctx)) } |
      "iszero" ~> aTerm ^^ { t => ctx: Context => TmIsZero(t(ctx)) } |
      aTerm

  lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> term <~ ")" |
      "true" ^^ { _ => ctx: Context => TmTrue } |
      "false" ^^ { _ => ctx: Context => TmFalse } |
      lcid ^^ { i => ctx: Context => TmVar(ctx.name2index(i), ctx.length) } |
      numericLit ^^ { x => ctx: Context => num(x.toInt) }

  private def num(x: Int): Term = x match {
    case 0 => TmZero
    case _ => TmSucc(num(x - 1))
  }

  lazy val phraseTopLevel: PackratParser[Res1[List[Command]]] = phrase(topLevel)

  def input(s: String): Res1[List[Command]] = phraseTopLevel(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t                 => sys.error(t.toString)
  }

}