package tapl.test.grammars

import java.nio.file.{Files, Paths}

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.ParseTreeWalker

import tapl.grammars._

class GrammarSpec extends org.scalatest.funspec.AnyFunSpec {
  import GrammarSpec._

  describe("Antlr grammars should recognize examples") {
    it("Arith.g4") {
      import tapl.arith._

      val input = getInput(ArithDemo.defaultExample)
      val parser =
        new ArithParser(new CommonTokenStream(new ArithLexer(CharStreams.fromString(input))))
      parser.addErrorListener(new FailErrorListener)
      val programContext = parser.program()

      val listener = new ArithAstListener
      (new ParseTreeWalker).walk(listener, programContext)
      val antlrProgram = listener.program

      val scalaProgram = ArithParsers.input(input)
      assert(antlrProgram === scalaProgram)
    }

    it("Bot.g4") {
      import tapl.bot._

      val input = getInput(BotDemo.defaultExample)
      val parser = new BotParser(new CommonTokenStream(new BotLexer(CharStreams.fromString(input))))
      parser.addErrorListener(new FailErrorListener)
      parser.program()
    }
  }
}

object GrammarSpec {
  def getInput(path: String): String =
    new String(Files.readAllBytes(Paths.get(path)))

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
}
