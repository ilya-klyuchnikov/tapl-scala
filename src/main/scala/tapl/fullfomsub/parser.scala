package tapl.fullfomsub

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

import Syntax._

object FullFomSubParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.reserved ++= Seq("lambda", "Bool", "true", "false", "if", "then", "else",
    "Nat", "String", "Unit", "Float", "unit", "case", "let", "in", "succ", "pred",
    "as", "of", "fix", "iszero", "letrec", "_", "All", "Some", "Top")
  lexical.delimiters ++= Seq("(", ")", ";", "/", ".", ":", "->", "=", "<", ">", "{", "}", "=>", "==>", ",", "|", "*", "[", "]", "<:")

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
      ucid ~ tyBinder ^^ { case id ~ bind => ctx: Context => (Bind(id, bind(ctx)), ctx.addName(id)) } |
      term ^^ { t => ctx: Context => val t1 = t(ctx); (Eval(t1), ctx) } |
      ("{" ~> ucid <~ ",") ~ (lcid <~ "}") ~ ("=" ~> term) ^^ { case i1 ~ i2 ~ t => ctx: Context => (SomeBind(i1, i2, t(ctx)), ctx.addName(i1).addName(i2)) }

  // BINDERS
  lazy val binder: Parser[Context => Binding] =
    ":" ~> `type` ^^ { t => ctx: Context => VarBind(t(ctx)) } |
      "=" ~> term ^^ { t => ctx: Context => TmAbbBind(t(ctx), None) }

  def addBinders(tyT: Ty, l: List[(String, Kind)]): Ty = l match {
    case Nil              => tyT
    case (tyX, k) :: rest => TyAbs(tyX, k, addBinders(tyT, rest))
  }

  lazy val tyAbbArgs: PackratParser[(List[(String, Kind)], Context) => (List[(String, Kind)], Context)] =
    success({ (b: List[(String, Kind)], ctx: Context) => (b, ctx) }) |
      ucid ~ oKind ~ tyAbbArgs ^^ {
        case id ~ k ~ args => (b: List[(String, Kind)], ctx: Context) =>
          val ctx1 = ctx.addName(id)
          args(b :+ (id, k(ctx)), ctx1)
      }

  lazy val tyBinder: Parser[Context => Binding] =
    tyAbbArgs ~ ("=" ~> `type`) ^^ {
      case args ~ ty => ctx: Context =>
        val (b, ctx1) = args(Nil, ctx)
        TyAbbBind(addBinders(ty(ctx1), b), None)
    } |
      ("<:" ~> `type`) ^^ { ty => ctx: Context => TyVarBind(ty(ctx)) } |
      "::" ~> kind ^^ { k => ctx: Context => TyVarBind(makeTop(k(ctx))) } |
      success({ ctx: Context => TyVarBind(TyTop) })

  // KINDS
  lazy val kind: PackratParser[Res[Kind]] = arrowKind
  lazy val arrowKind: PackratParser[Res[Kind]] =
    (aKind <~ "==>") ~ arrowKind ^^ { case k1 ~ k2 => ctx: Context => KnArr(k1(ctx), k2(ctx)) } |
      aKind
  lazy val aKind: PackratParser[Res[Kind]] =
    "*" ^^ { _ => ctx: Context => KnStar } |
      "(" ~> kind <~ ")"

  // optional Kind
  lazy val oKind: PackratParser[Res[Kind]] =
    "::" ~> kind |
      success({ ctx: Context => KnStar })

  // TYPES
  lazy val `type`: PackratParser[Res[Ty]] =
    arrowType |
      ("All" ~> ucid) ~ oType ~ ("." ~> `type`) ^^ { case id ~ ty1 ~ ty2 => ctx: Context => TyAll(id, ty1(ctx), ty2(ctx.addName(id))) } |
      ("lambda" ~> ucid) ~ oKind ~ ("." ~> `type`) ^^ { case id ~ k ~ ty => ctx: Context => TyAbs(id, k(ctx), ty(ctx.addName(id))) }
  // optional type
  lazy val oType: PackratParser[Res[Ty]] =
    ("<:" ~> `type`) | success({ ctx: Context => TyTop })
  lazy val aType: PackratParser[Res[Ty]] =
    "(" ~> `type` <~ ")" |
      ucid ^^ { tn => ctx: Context => if (ctx.isNameBound(tn)) TyVar(ctx.name2index(tn), ctx.length) else TyId(tn) } |
      "Bool" ^^ { _ => ctx: Context => TyBool } |
      "Top" ^^ { _ => ctx: Context => TyTop } |
      "String" ^^ { _ => ctx: Context => TyString } |
      "Unit" ^^ { _ => ctx: Context => TyUnit } |
      "{" ~> fieldTypes <~ "}" ^^ { ft => ctx: Context => TyRecord(ft(ctx)) } |
      "Nat" ^^ { _ => ctx: Context => TyNat } |
      (("{" ~ "Some") ~> ucid) ~ oType ~ ("," ~> `type` <~ "}") ^^ { case id ~ ty1 ~ ty2 => ctx: Context => TySome(id, ty1(ctx), ty2(ctx.addName(id))) } |
      ("Top" ~ "[") ~> kind <~ "]" ^^ { k => ctx: Context => makeTop(k(ctx)) }

  lazy val fieldTypes: PackratParser[Res[List[(String, Ty)]]] =
    repsep(fieldType, ",") ^^ { fs => ctx: Context => fs.zipWithIndex.map { case (ft, i) => ft(ctx, i + 1) } }

  lazy val fieldType: PackratParser[(Context, Int) => (String, Ty)] =
    lcid ~ (":" ~> `type`) ^^ { case id ~ ty => (ctx: Context, i: Int) => (id, ty(ctx)) } |
      `type` ^^ { ty => (ctx: Context, i: Int) => (i.toString, ty(ctx)) }

  lazy val arrowType: PackratParser[Res[Ty]] =
    (appType <~ "->") ~ arrowType ^^ { case t1 ~ t2 => ctx: Context => TyArr(t1(ctx), t2(ctx)) } |
      appType

  lazy val appType: PackratParser[Res[Ty]] =
    appType ~ aType ^^ { case t1 ~ t2 => ctx: Context => TyApp(t1(ctx), t2(ctx)) } |
      aType

  // TERMS
  lazy val term: PackratParser[Res[Term]] =
    appTerm |
      ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ { case t1 ~ t2 ~ t3 => ctx: Context => TmIf(t1(ctx), t2(ctx), t3(ctx)) } |
      ("lambda" ~> lcid) ~ (":" ~> `type`) ~ ("." ~> term) ^^ { case v ~ ty ~ t => ctx: Context => TmAbs(v, ty(ctx), t(ctx.addName(v))) } |
      ("lambda" ~ "_") ~> (":" ~> `type`) ~ ("." ~> term) ^^ { case ty ~ t => ctx: Context => TmAbs("_", ty(ctx), t(ctx.addName("_"))) } |
      ("let" ~> lcid) ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case id ~ t1 ~ t2 => ctx: Context => TmLet(id, t1(ctx), t2(ctx.addName(id))) } |
      ("let" ~ "_") ~> ("=" ~> term) ~ ("in" ~> term) ^^ { case t1 ~ t2 => ctx: Context => TmLet("_", t1(ctx), t2(ctx.addName("_"))) } |
      {
        ("letrec" ~> lcid) ~ (":" ~> `type`) ~ ("=" ~> term) ~ ("in" ~> term) ^^
          { case id ~ ty ~ t1 ~ t2 => ctx: Context => TmLet(id, TmFix(TmAbs(id, ty(ctx), t1(ctx.addName(id)))), t2(ctx.addName(id))) }
      } |
      {
        (("let" ~ "{") ~> ucid) ~ ("," ~> lcid <~ "}") ~ ("=" ~> term) ~ ("in" ~> term) ^^
          { case id1 ~ id2 ~ t1 ~ t2 => ctx: Context => TmUnPack(id1, id2, t1(ctx), t2(ctx.addName(id1).addName(id2))) }
      } |
      ("lambda" ~> ucid) ~ oType ~ ("." ~> term) ^^ { case id ~ ty ~ t => ctx: Context => TmTAbs(id, ty(ctx), t(ctx.addName(id))) }

  lazy val appTerm: PackratParser[Res[Term]] =
    (appTerm <~ "[") ~ (`type` <~ "]") ^^ { case t ~ ty => ctx: Context => TmTApp(t(ctx), ty(ctx)) } |
      appTerm ~ pathTerm ^^ { case t1 ~ t2 => ctx: Context => TmApp(t1(ctx), t2(ctx)) } |
      "fix" ~> pathTerm ^^ { t => ctx: Context => TmFix(t(ctx)) } |
      "succ" ~> pathTerm ^^ { t => ctx: Context => TmSucc(t(ctx)) } |
      "pred" ~> pathTerm ^^ { t => ctx: Context => TmPred(t(ctx)) } |
      "iszero" ~> pathTerm ^^ { t => ctx: Context => TmIsZero(t(ctx)) } |
      pathTerm

  lazy val ascribeTerm: PackratParser[Res[Term]] =
    aTerm ~ ("as" ~> `type`) ^^ { case t ~ ty => ctx: Context => TmAscribe(t(ctx), ty(ctx)) } |
      aTerm

  lazy val pathTerm: PackratParser[Res[Term]] =
    pathTerm ~ ("." ~> lcid) ^^ { case t1 ~ l => ctx: Context => TmProj(t1(ctx), l) } |
      pathTerm ~ ("." ~> numericLit) ^^ { case t1 ~ l => ctx: Context => TmProj(t1(ctx), l) } |
      ascribeTerm

  lazy val termSeq: PackratParser[Res[Term]] =
    term ~ (";" ~> termSeq) ^^ { case t ~ ts => ctx: Context => TmApp(TmAbs("_", TyUnit, ts(ctx.addName("_"))), t(ctx)) } |
      term

  lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> termSeq <~ ")" |
      ("inert" ~ "[") ~> `type` <~ "]" ^^ { ty => ctx: Context => TmInert(ty(ctx)) } |
      "true" ^^ { _ => ctx: Context => TmTrue } |
      "false" ^^ { _ => ctx: Context => TmFalse } |
      lcid ^^ { i => ctx: Context => TmVar(ctx.name2index(i), ctx.length) } |
      stringLit ^^ { l => ctx: Context => TmString(l) } |
      "unit" ^^ { _ => ctx: Context => TmUnit } |
      "{" ~> fields <~ "}" ^^ { fs => ctx: Context => TmRecord(fs(ctx)) } |
      numericLit ^^ { x => ctx: Context => num(x.toInt) } |
      (("{" ~ "*") ~> `type`) ~ ("," ~> term <~ "}") ~ ("as" ~> `type`) ^^ { case ty1 ~ t ~ ty2 => ctx: Context => TmPack(ty1(ctx), t(ctx), ty2(ctx)) }

  lazy val fields: PackratParser[Res[List[(String, Term)]]] =
    repsep(field, ",") ^^ { fs => ctx: Context => fs.zipWithIndex.map { case (ft, i) => ft(ctx, i + 1) } }
  lazy val field: PackratParser[(Context, Int) => (String, Term)] =
    lcid ~ ("=" ~> term) ^^ { case id ~ t => (ctx: Context, i: Int) => (id, t(ctx)) } |
      term ^^ { t => (ctx: Context, i: Int) => (i.toString, t(ctx)) }

  private def num(x: Int): Term = x match {
    case 0 => TmZero
    case _ => TmSucc(num(x - 1))
  }

  lazy val phraseTopLevel: PackratParser[Res1[List[Command]]] = phrase(topLevel)

  def input(s: String) = phraseTopLevel(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t                 => sys.error(t.toString)
  }

}