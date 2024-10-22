package saki.util

case class SourceSpan(start: Int, end: Int) {
  def contains(other: SourceSpan): Boolean = {
    start <= other.start && end >= other.end
  }

  def overlaps(other: SourceSpan): Boolean = {
    (start < other.end && end > other.start) || (other.start < end && other.end > start)
  }

  def length: Int = end - start

  def merge(other: SourceSpan): Option[SourceSpan] = {
    if overlaps(other) || contains(other) || other.contains(this) then
      Some(SourceSpan(math.min(start, other.start), math.max(end, other.end)))
    else None
  }

  def toSourcePositions(code: String): (SourcePosition, SourcePosition) = {
    (SourcePosition.from(code, start), SourcePosition.from(code, end))
  }
}

case class SourcePosition(line: Int, column: Int) {
  def isEqual(other: SourcePosition): Boolean = {
    line == other.line && column == other.column
  }

  def moveBy(columns: Int): SourcePosition = {
    copy(column = column + columns)
  }

  def toCharPos(code: String): Int = {
    val lines = code.split("\n")
    var charPoint = 0
    for (i <- 0 until line - 1) {
      charPoint += lines(i).length + 1 // Add 1 for the newline character
    }
    charPoint + column
  }
}

object SourcePosition {

  def from(code: String, bytePos: Int): SourcePosition = {
    val lines = code.split('\n').foldLeft((0, 0, 0)) {
      case ((line, _, pos), currentLine) => {
        if pos + currentLine.length + 1 > bytePos then (line, bytePos - pos, pos)
        else (line + 1, 0, pos + currentLine.length + 1)
      }
    }
    SourcePosition(lines._1, lines._2)
  }

  def fromLines(code: String): List[SourcePosition] = {
    code.split('\n').zipWithIndex.flatMap { case (line, lineIndex) =>
      line.zipWithIndex.map { case (_, colIndex) => SourcePosition(lineIndex, colIndex) }
    }.toList
  }

}
