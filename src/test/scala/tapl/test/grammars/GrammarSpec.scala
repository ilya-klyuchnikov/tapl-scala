package tapl.test.grammars

import org.antlr.v4.runtime._
import java.nio.file.Paths

import tapl.arith.ArithDemo
import tapl.grammars._

class GrammarSpec extends org.scalatest.funspec.AnyFunSpec {
  describe("Antlr grammars should recognize examples") {
    it("Arith.g4") {
      val stream = CharStreams.fromPath(Paths.get(ArithDemo.defaultExample))
      val lexer = new ArithLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      val parser = new ArithParser(tokens)
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
