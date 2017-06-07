package util

import scala.text.Document
import scala.text.Document._
import java.io.StringWriter
import scala.language.implicitConversions

object Print {
  implicit def text2doc(s: String): Document = text(s)
  def g0(doc: Document) = group(doc)
  def g2(doc: Document) = group(nest(2, doc))
  
  def print(d: Document, w: Int = 90): String = {
    val sw = new StringWriter()
    d.format(w, sw)
    sw.toString
  }
}