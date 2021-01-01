package tapl.fullsimple

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import util.{Loc, Range}

object FullSimpleParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
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
    "fix",
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

  private def pos[T](p: => Parser[Range => T]): Parser[T] =
    Parser { in1 =>
      p(in1) match {
        case Success(t, in2) =>
          val loc1 = Loc(in1.pos.line, in1.pos.column)
          val loc2 = Loc(in2.pos.line, in2.pos.column)
          Success(t(Range(loc1, loc2)), in2)
        case Error(msg, next)   => Error(msg, next)
        case Failure(msg, next) => Failure(msg, next)
      }
    }

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
        ctx: Context =>
          val (cmd1, ctx1) = f(ctx)
          val (cmds, ctx2) = g(ctx1)
          (cmd1 :: cmds, ctx2)
    } | success { ctx: Context => (List(), ctx) }

  private lazy val command: PackratParser[Res1[Command]] =
    lcid ~ binder ^^ { case id ~ bind => ctx: Context => (Bind(id, bind(ctx)), ctx.addName(id)) } |
      ucid ~ tyBinder ^^ {
        case id ~ bind => ctx: Context => (Bind(id, bind(ctx)), ctx.addName(id))
      } |
      term ^^ { t => ctx: Context =>
        val t1 = t(ctx); (Eval(t1), ctx)
      }

  private lazy val binder: Parser[Context => Binding] =
    ":" ~> typ ^^ { t => ctx: Context => VarBind(t(ctx)) } |
      "=" ~> term ^^ { t => ctx: Context => TmAbbBind(t(ctx), None) }
  private lazy val tyBinder: Parser[Context => Binding] =
    ("=" ~> typ) ^^ { ty => ctx: Context => TyAbbBind(ty(ctx)) } |
      success({ ctx: Context => TyVarBind })

  private lazy val typ: PackratParser[Res[Ty]] =
    arrowType
  private lazy val aType: PackratParser[Res[Ty]] =
    "(" ~> typ <~ ")" |
      ucid ^^ { tn => ctx: Context =>
        if (ctx.isNameBound(tn)) TyVar(ctx.name2index(tn), ctx.length) else TyId(tn)
      } |
      "Bool" ^^ { _ => ctx: Context => TyBool } |
      "<" ~> fieldTypes <~ ">" ^^ { ft => ctx: Context => TyVariant(ft(ctx)) } |
      "String" ^^ { _ => ctx: Context => TyString } |
      "Unit" ^^ { _ => ctx: Context => TyUnit } |
      "{" ~> fieldTypes <~ "}" ^^ { ft => ctx: Context => TyRecord(ft(ctx)) } |
      "Nat" ^^ { _ => ctx: Context => TyNat }

  private lazy val fieldTypes: PackratParser[Res[List[(String, Ty)]]] =
    repsep(fieldType, ",") ^^ { fs => ctx: Context =>
      fs.zipWithIndex.map { case (ft, i) => ft(ctx, i + 1) }
    }

  private lazy val fieldType: PackratParser[(Context, Int) => (String, Ty)] =
    lcid ~ (":" ~> typ) ^^ { case id ~ ty => (ctx: Context, i: Int) => (id, ty(ctx)) } |
      typ ^^ { ty => (ctx: Context, i: Int) => (i.toString, ty(ctx)) }

  private lazy val arrowType: PackratParser[Res[Ty]] =
    (aType <~ "->") ~ arrowType ^^ { case t1 ~ t2 => ctx: Context => TyArr(t1(ctx), t2(ctx)) } |
      aType

  // TERMS
  private lazy val term: PackratParser[Res[Term]] =
    appTerm |
      pos(("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ {
        case t1 ~ t2 ~ t3 => r: Range => ctx: Context => TmIf(t1(ctx), t2(ctx), t3(ctx))(r)
      }) |
      pos(("case" ~> term) ~ ("of" ~> cases) ^^ {
        case t ~ cs => r: Range => ctx: Context => TmCase(t(ctx), cs(ctx))(r)
      }) |
      pos((("lambda" | "\\") ~> (lcid | "_")) ~ (":" ~> typ) ~ ("." ~> term) ^^ {
        case v ~ ty ~ t => r: Range => ctx: Context => TmAbs(v, ty(ctx), t(ctx.addName(v)))(r)
      }) |
      pos(("let" ~> (lcid | "_")) ~ ("=" ~> term) ~ ("in" ~> term) ^^ {
        case id ~ t1 ~ t2 =>
          r: Range => ctx: Context => TmLet(id, t1(ctx), t2(ctx.addName(id)))(r)
      }) | {
      pos(("letrec" ~> lcid) ~ (":" ~> typ) ~ ("=" ~> term) ~ ("in" ~> term) ^^ {
        case id ~ ty ~ t1 ~ t2 =>
          r: Range =>
            ctx: Context =>
              val t11 = t1(ctx.addName(id))
              TmLet(id, TmFix(TmAbs(id, ty(ctx), t11)(t11.r))(t11.r), t2(ctx.addName(id)))(r)
      })
    }

  private lazy val appTerm: PackratParser[Res[Term]] =
    pos(appTerm ~ pathTerm ^^ {
      case t1 ~ t2 => r: Range => ctx: Context => TmApp(t1(ctx), t2(ctx))(r)
    }) |
      pos("fix" ~> pathTerm ^^ { t => r: Range => ctx: Context => TmFix(t(ctx))(r) }) |
      pos("succ" ~> pathTerm ^^ { t => r: Range => ctx: Context => TmSucc(t(ctx))(r) }) |
      pos("pred" ~> pathTerm ^^ { t => r: Range => ctx: Context => TmPred(t(ctx))(r) }) |
      pos("iszero" ~> pathTerm ^^ { t => r: Range => ctx: Context => TmIsZero(t(ctx))(r) }) |
      pathTerm

  private lazy val ascribeTerm: PackratParser[Res[Term]] =
    pos(aTerm ~ ("as" ~> typ) ^^ {
      case t ~ ty => r: Range => ctx: Context => TmAscribe(t(ctx), ty(ctx))(r)
    }) |
      aTerm

  private lazy val pathTerm: PackratParser[Res[Term]] =
    pos(pathTerm ~ ("." ~> lcid) ^^ {
      case t1 ~ l => r: Range => ctx: Context => TmProj(t1(ctx), l)(r)
    }) |
      pos(pathTerm ~ ("." ~> numericLit) ^^ {
        case t1 ~ l => r: Range => ctx: Context => TmProj(t1(ctx), l)(r)
      }) |
      ascribeTerm

  private lazy val termSeq: PackratParser[Res[Term]] =
    pos(term ~ (";" ~> termSeq) ^^ {
      case t ~ ts =>
        r: Range =>
          ctx: Context =>
            val ts1 = ts(ctx.addName("_"))
            TmApp(TmAbs("_", TyUnit, ts1)(ts1.r), t(ctx))(r)
    }) | term

  private lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> termSeq <~ ")" |
      pos("true" ^^ { s: String => r: Range => ctx: Context => TmTrue()(r) }) |
      pos("false" ^^ { s: String => r: Range => ctx: Context => TmFalse()(r) }) |
      pos(("<" ~> lcid) ~ ("=" ~> term <~ ">") ~ ("as" ~> typ) ^^ {
        case l ~ t ~ ty => r: Range => ctx: Context => TmTag(l, t(ctx), ty(ctx))(r)
      }) |
      pos(
        lcid ^^ { i => r: Range => ctx: Context => TmVar(ctx.name2index(i), ctx.length)(r) }
      ) |
      pos(stringLit ^^ { l => r: Range => ctx: Context => TmString(l)(r) }) |
      pos("unit" ^^ { _ => r: Range => ctx: Context => TmUnit()(r) }) |
      pos("{" ~> fields <~ "}" ^^ { fs => r: Range => ctx: Context => TmRecord(fs(ctx))(r) }) |
      pos(numericLit ^^ { x => r: Range => ctx: Context => num(x.toInt, r) })

  private lazy val cases: PackratParser[Res[List[(String, String, Term)]]] =
    rep1sep(`case`, "|") ^^ { cs => ctx: Context => cs.map { c => c(ctx) } }

  private lazy val `case`: PackratParser[Res[(String, String, Term)]] =
    ("<" ~> lcid <~ "=") ~ (lcid <~ ">") ~ ("==>" ~> term) ^^ {
      case l1 ~ l2 ~ t => ctx: Context => (l1, l2, t(ctx.addName(l2)))
    }

  private lazy val fields: PackratParser[Res[List[(String, Term)]]] =
    repsep(field, ",") ^^ { fs => ctx: Context =>
      fs.zipWithIndex.map { case (ft, i) => ft(ctx, i + 1) }
    }

  private lazy val field: PackratParser[(Context, Int) => (String, Term)] =
    lcid ~ ("=" ~> term) ^^ { case id ~ t => (ctx: Context, i: Int) => (id, t(ctx)) } |
      term ^^ { t => (ctx: Context, i: Int) => (i.toString, t(ctx)) }

  private def num(x: Int, r: Range): Term =
    x match {
      case 0 => TmZero()(r)
      case _ => TmSucc(num(x - 1, r))(r)
    }

  private lazy val phraseTopLevel: PackratParser[Res1[List[Command]]] =
    phrase(topLevel)

  def input(s: String): Res1[List[Command]] =
    phraseTopLevel(new lexical.Scanner(s)) match {
      case t if t.successful => t.get
      case t                 => sys.error(t.toString)
    }

}
