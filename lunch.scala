#!/bin/sh
L=`dirname $0`/lib
cp=`echo $L/*.jar|sed 's/ /:/g'`
exec scala -Dfile.encoding=UTF-8 -savecompiled -classpath $cp $0 $@
!#
// /* */ */

import scala.io.Source
import scala.xml._
import java.net.URL
import scala.collection
import scala.collection.JavaConversions._

System.setProperty("http.proxyHost", "www-proxy.ericsson.se")
System.setProperty("http.proxyPort","8080")

val g = Gothia.menu.iterator
val k = Koop.menu.iterator
List("Mandag", "Tisdag", "Onsdag", "Torsdag", "Fredag").foreach((dag) => {
  println("=== " + dag + " ===")
  println("== Kooperativet ==")
  k.next.foreach(println)
  println("== Gothia ==")
  g.next.foreach(println)
  println
})

object Gothia {
  var menu : List[List[String]] = List()
  val xml = Web.get("http://www.restauranggothia.com/lunch.htm")
  val sub = xml \\ "table"
  var iter = Source.fromString(sub.text).getLines
  iter = iter.dropWhile(!_.contains("ndag"))
  iter = iter.takeWhile(!_.contains("Veckans"))
  for (line <- iter) {
    if (line.trim.endsWith("dag")) {
      menu = List() :: menu
    } else if (line.trim != "") {
      val hd :: menutl = menu
      menu = (line.trim :: hd) :: menutl
    }
  }
  menu = menu.map(_.reverse).reverse
}

object Koop {
  val ignore = Set("80:-", "Inkl sallad", "TAKE AWAY", "Dagens Meny")
  var menu : List[List[String]] = List()
  for (i <- 1 to 5) {
    val xml = Web.get("http://www.kooperativet.se/printversion.php?p=meny&d=" + i)
    val sub = xml \ "body" \ "div" \ "_"
    menu = (for(node <- sub
	       if node.text != "" &&
	          !ignore.exists(node.text.contains(_)))
	       yield node.text.trim).toList :: menu
  }
  menu = menu.reverse
}

object Web {
  val parserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
  val parser = parserFactory.newSAXParser()
  val adapter = new scala.xml.parsing.NoBindingFactoryAdapter

  def get(url: String) = {
    val source = new org.xml.sax.InputSource(url)
    adapter.loadXML(source, parser)
  }
}

object Cmd {
  def apply(cmd : List[String]) = {
    val process = new ProcessBuilder(cmd).start
    val source = Source.fromInputStream(process.getInputStream)
    source.getLines.foreach(println)
  }
}

