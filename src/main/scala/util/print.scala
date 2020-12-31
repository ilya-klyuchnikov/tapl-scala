package util

import java.io.StringWriter
import scala.language.implicitConversions
import Document._

import scala.annotation.tailrec

object Print {
  implicit def text2doc(s: String): Document = text(s)
  def g0(doc: Document): Document = group(doc)
  def g2(doc: Document): Document = group(nest(2, doc))

  def print(d: Document, w: Int = 90): String = {
    val sw = new StringWriter()
    d.format(w, sw)
    sw.toString
  }
}

import java.io.Writer

case object DocNil extends Document
case object DocBreak extends Document
case class DocText(txt: String) extends Document
case class DocGroup(doc: Document) extends Document
case class DocNest(indent: Int, doc: Document) extends Document
case class DocCons(hd: Document, tl: Document) extends Document

/** scala.text https://www.scala-lang.org/api/2.12.12/scala/text/index.html
  */
sealed abstract class Document {
  def ::(hd: Document): Document = DocCons(hd, this)
  def ::(hd: String): Document = DocCons(DocText(hd), this)
  def :/:(hd: Document): Document = hd :: DocBreak :: this
  def :/:(hd: String): Document = hd :: DocBreak :: this

  /**
    * Format this document on `writer` and try to set line
    * breaks so that the result fits in `width` columns.
    */
  def format(width: Int, writer: Writer): Unit = {
    type FmtState = (Int, Boolean, Document)

    @tailrec
    def fits(w: Int, state: List[FmtState]): Boolean =
      state match {
        case _ if w < 0 =>
          false
        case Nil =>
          true
        case (_, _, DocNil) :: z =>
          fits(w, z)
        case (i, b, DocCons(h, t)) :: z =>
          fits(w, (i, b, h) :: (i, b, t) :: z)
        case (_, _, DocText(t)) :: z =>
          fits(w - t.length(), z)
        case (i, b, DocNest(ii, d)) :: z =>
          fits(w, (i + ii, b, d) :: z)
        case (_, false, DocBreak) :: z =>
          fits(w - 1, z)
        case (_, true, DocBreak) :: z =>
          true
        case (i, _, DocGroup(d)) :: z =>
          fits(w, (i, false, d) :: z)
      }

    def spaces(n: Int): Unit = {
      var rem = n
      while (rem >= 16) { writer write "                "; rem -= 16 }
      if (rem >= 8) { writer write "        "; rem -= 8 }
      if (rem >= 4) { writer write "    "; rem -= 4 }
      if (rem >= 2) { writer write "  "; rem -= 2 }
      if (rem == 1) { writer write " " }
    }

    @tailrec
    def fmt(k: Int, state: List[FmtState]): Unit =
      state match {
        case List() => ()
        case (_, _, DocNil) :: z =>
          fmt(k, z)
        case (i, b, DocCons(h, t)) :: z =>
          fmt(k, (i, b, h) :: (i, b, t) :: z)
        case (i, _, DocText(t)) :: z =>
          writer write t
          fmt(k + t.length(), z)
        case (i, b, DocNest(ii, d)) :: z =>
          fmt(k, (i + ii, b, d) :: z)
        case (i, true, DocBreak) :: z =>
          writer write "\n"
          spaces(i)
          fmt(i, z)
        case (i, false, DocBreak) :: z =>
          writer write " "
          fmt(k + 1, z)
        case (i, b, DocGroup(d)) :: z =>
          val fitsFlat = fits(width - k, (i, false, d) :: z)
          fmt(k, (i, !fitsFlat, d) :: z)
        case _ =>
          ()
      }

    fmt(0, (0, false, DocGroup(this)) :: Nil)
  }
}

object Document {

  /** The empty document */
  def empty: Document = DocNil

  /** A break, which will either be turned into a space or a line break */
  def break: Document = DocBreak

  /** A document consisting of some text literal */
  def text(s: String): Document = DocText(s)

  /**
    * A group, whose components will either be printed with all breaks
    * rendered as spaces, or with all breaks rendered as line breaks.
    */
  def group(d: Document): Document = DocGroup(d)

  /** A nested document, which will be indented as specified. */
  def nest(i: Int, d: Document): Document = DocNest(i, d)
}
