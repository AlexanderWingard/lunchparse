package code
package snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import code.lib._
import Helpers._

import com.lunchparse.Lunch

class LunchSnippet {
  def showLunch(in : NodeSeq) : NodeSeq =
    Lunch.menu
}

