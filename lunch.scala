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

Koop
Gothia

object Gothia {
  var menu : List[List[String]] = List()
  val xml = Web.get("http://www.restauranggothia.com/lunch.htm")
  val sub = xml \\ "table"
  trav(sub).foreach(println)
}

object Koop {
  val ignore = Set("80:-", "Inkl sallad", "TAKE AWAY", "Dagens Meny")
  var menu : List[List[String]] = List()
  for (i <- 1 to 5) {
    val xml = Web.get("http://www.kooperativet.se/printversion.php?p=meny&d=" + i)
    val sub = xml \ "body" \ "div"
    trav(sub).foreach(println)
  }
}

 def trav(nodes : NodeSeq, acc : List[String] = List()) : Seq[String] = {
    nodes.flatMap((node) => node match {
      case Text(text) =>
	val format = text.replaceAll("\240", " ").lines.map(_.trim).mkString
      format :: acc
      case _ =>
	trav(node.child, acc)
  }).filter(!_.isEmpty)
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

