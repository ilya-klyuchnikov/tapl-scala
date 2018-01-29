package tapl.untyped

import org.scalacheck.{ Properties, Prop }

object SyntaxSpec extends Properties("Untyped") {
  import Prop._
  import TermGen.terms

  property("parsingAndPrinting") = forAll(terms) { t1 =>
    collect(t1.getClass) {
      val text1 = t1.prettyString()
      val t2 = UntypedParsers.inputTerm(text1)(Context())
      //println(text1)
      t1 == t2
    }
  }

}