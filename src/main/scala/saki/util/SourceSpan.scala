package saki.util

import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.misc.Interval

case class SourceSpan(start: Int, end: Int) {
  override def toString: String = s"(span $start $end)"
  def context = new VirtualParserRuleContext(start, end)
}

extension (self: ParserRuleContext) {
  def span: SourceSpan = SourceSpan(self.getSourceInterval.a, self.getSourceInterval.b)
}

class VirtualParserRuleContext(start: Int, end: Int) extends ParserRuleContext {
  override def getSourceInterval = new Interval(start, end)
}
