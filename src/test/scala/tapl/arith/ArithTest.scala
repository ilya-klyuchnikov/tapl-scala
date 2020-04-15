package tapl.arith

import org.scalatest._

import Evaluator._

class ArithTest extends FlatSpec with org.scalatest.matchers.should.Matchers {
  def testInterpreter(name: String, eval: String => Term): Unit = {
    "A %s interpreter for arithmetic expressions".format(name) should "not reduce values any further" in {
      val progs = List("0", "succ 0", "succ (succ 0)", "true", "false")
      for(prog <- progs)
        eval(prog) should be (parse(prog))
    }

    it should "reduce boolean expressions to a value" in {
      eval("iszero 0") should be (parse("true"))
      eval("iszero (succ 0)") should be (parse("false"))
      eval("if (iszero 0) then true else false") should be (parse("true"))
      eval("if (iszero (succ 0)) then true else false") should be (parse("false"))
    }

    it should "reduce numeric expressions to a value" in {
      eval("pred (succ 0)") should be (parse("0"))
      eval("pred 0") should be (parse("0"))
      eval("succ (pred 0)") should be (parse("succ 0"))
    }

    it should "get stuck on ill-typed terms" in {
      a [NoRuleApplies] should be thrownBy { eval("succ true") }
      a [NoRuleApplies] should be thrownBy { eval("pred true") }
      a [NoRuleApplies] should be thrownBy { eval("iszero true") }
      a [NoRuleApplies] should be thrownBy { eval("if 0 then true else false") }
    }
  }

  testInterpreter("small-step", Evaluator.eval)
  testInterpreter("big-step", BigStepEvaluator.eval)

  private def parse(prog: String) = ArithParsers.parseTerm(prog)
}
