package util

case class Loc(line: Int, column: Int)
case class Range(start: Loc, end: Loc) {
  def !(that: Range): Range = Range(this.start, that.end)
}
