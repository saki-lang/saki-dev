import java.nio.file.Path
import scala.annotation.{tailrec, targetName}

package object util {
  
  case class Position(line: Int, col: Int) {
    @targetName("eq")
    def ==(other: Position): Boolean = {
      line == other.line && col == other.col
    }
    
    def isDefault: Boolean = {
      line == defaultPosition.line && col == defaultPosition.col
    }
  }
  
  private val defaultPosition: Position = Position(0, 0)
  
  case class Location(file: Option[Path], start: Position, end: Position)
  
  private val defaultLocation: Location = Location(None, defaultPosition, defaultPosition)
  
  case class Located[T](data: T, location: Location) {
    @targetName("eq")
    infix def ==(other: Located[T]): Boolean = this.data == other.data  
  }
  
  object Located {
    def apply[T](value: T, file: Option[Path], start: Position, end: Position): Located[T] = {
      Located(value, Location(file, start, end))
    }
    
    def apply[T](value: T): Located[T] = {
      Located(value, defaultLocation)
    }
  }
}
