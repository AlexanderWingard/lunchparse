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
    <h2>Restaurang GotaAlv</h2>
    <pre>{Lunch.gotaAlv.mkString}</pre>
    <h2>Gothia</h2>
    <pre>{Lunch.gothia.mkString}</pre>
}

