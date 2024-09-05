package saki

import org.antlr.v4.runtime.ParserRuleContext

case class SourceSpan(start: Int, end: Int) {
  override def toString: String = s"(span $start $end)"
}

extension (self: ParserRuleContext) {
  def span: SourceSpan = {
    val start = (self.start.getLine, self.start.getCharPositionInLine)
    val stop = (self.stop.getLine, self.stop.getCharPositionInLine)
    SourceSpan(self.getSourceInterval.a, self.getSourceInterval.b)
  }
}
