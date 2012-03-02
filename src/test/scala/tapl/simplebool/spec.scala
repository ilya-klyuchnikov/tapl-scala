package tapl.simplebool

import org.scalacheck.{ Properties, Prop }

object SyntaxSpec extends Properties("SimpleBool") {
  import Prop._
  import TermGen.terms

  property("parsingAndPrinting") = forAll(terms) { t1 =>

    val text1 = t1.prettyString()
    println(text1)
    val t2 = SimpleBoolParsers.inputTerm(text1)(Context())
    
    (t1 == t2) :| ("evidence: " + t1 + " != " + t2)
  }

}