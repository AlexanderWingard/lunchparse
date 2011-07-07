#!/bin/sh
L=`dirname $0`/lib
cp=`echo $L/*.jar|sed 's/ /:/g'`
exec scala -savecompiled -classpath $cp $0 $@
!#
// /* */ */

import scala.io.Source
import scala.xml._
import java.net.URL
import scala.collection
import scala.collection.JavaConversions._

System.setProperty("http.proxyHost", "www-proxy.ericsson.se")
System.setProperty("http.proxyPort","8080")

println(Koop.menu)

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

