package tapl.test.grammars

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.ParseTreeWalker

import java.nio.file.Paths
import tapl.arith.{ArithAstListener, ArithDemo, ArithParsers}
import tapl.bot.BotDemo
import tapl.grammars._

import scala.io.Source

class GrammarSpec extends org.scalatest.funspec.AnyFunSpec {
  describe("Antlr grammars should recognize examples") {
    it("Arith.g4") {
      val stream = CharStreams.fromPath(Paths.get(ArithDemo.defaultExample))
      val lexer = new ArithLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      val parser = new ArithParser(tokens)
      parser.addErrorListener(new FailErrorListener)
      val tree = parser.program()
      val walker = new ParseTreeWalker
      val listener = new ArithAstListener
      walker.walk(listener, tree)
      val antlrProgram = listener.program
      val scalaProgram = ArithParsers.input(Source.fromFile(ArithDemo.defaultExample).mkString(""))

      assert(antlrProgram === scalaProgram)
    }

    it("Bot.g4") {
      val stream = CharStreams.fromPath(Paths.get(BotDemo.defaultExample))
      val lexer = new BotLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      val parser = new BotParser(tokens)
      parser.addErrorListener(new FailErrorListener)
      parser.program()
    }
  }
}

class FailErrorListener extends BaseErrorListener {
  override def syntaxError(
      recognizer: Recognizer[_, _],
      offendingSymbol: Any,
      line: Int,
      charPositionInLine: Int,
      msg: String,
      e: RecognitionException,
  ): Unit =
    sys.error(s"line $line, column $charPositionInLine, $msg")
}
