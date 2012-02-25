package tapl.untyped

import org.scalacheck.{ Properties, Prop }

object SyntaxSpec extends Properties("Untyped") {
  import Prop.forAll
  import TermGen.terms

  property("parsing and printing coordination") = forAll(terms) { t1 =>
    val text1 = t1.prettyString()
    val t2 = UntypedParsers.inputTerm(text1)(Context())
    println(text1)
    t1 == t2
  }

}