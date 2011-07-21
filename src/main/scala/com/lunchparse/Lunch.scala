package com.lunchparse

import scala.io.Source
import scala.io.Codec
import scala.xml._
import java.net.URL
import scala.collection
import scala.collection.JavaConversions._

object Lunch {
  System.setProperty("http.proxyHost", "www-proxy.ericsson.se")
  System.setProperty("http.proxyPort", "8080")

  def gotaAlv = {
    val xml = Web.get("http://www.kvartersmenyn.se/start/rest/11579")
    val lineBreaks = Set("p", "h2", "strong", "u")
    trav(xml \\ "table", lineBreaks).reverse
  }

  def gothia = {
    val xml = Web.get("http://www.restauranggothia.com/lunch.htm")
    trav(xml \\ "table", Set("p", "br")).reverse
  }

  private def trav(nodes: NodeSeq, lineBreaks: Set[String], acc: List[String] = List()): List[String] = {
    nodes.foldLeft(acc)((acc, node) => (node match {
      case Text(text) =>
        val format = text.replaceAll("\240", " ").lines.map(_.trim).mkString + " "
        if (format.trim != "")
          format :: acc
        else
          acc
      case _ if lineBreaks.contains(node.label) =>
        acc match {
          case "\n" :: rest =>
            trav(node.child, lineBreaks, acc)
          case other =>
            trav(node.child, lineBreaks, "\n" :: acc)
        }
      case _ =>
        trav(node.child, lineBreaks, acc)
    }))
  }
}

object Koop {
  for (i <- 1 to 5) {
    val xml = Web.get("http://www.kooperativet.se/printversion.php?p=meny&d=" + i)
    val sub = xml \ "body" \ "div"
  }
}

object Web {
  val parserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
  val parser = parserFactory.newSAXParser()
  val adapter = new scala.xml.parsing.NoBindingFactoryAdapter

  def get(url: String) = {
    val source = new org.xml.sax.InputSource(url)
    source.setEncoding("UTF-8")
    adapter.loadXML(source, parser)
  }
}

object Cmd {
  def apply(cmd: List[String]) = {
    val process = new ProcessBuilder(cmd).start
    val source = Source.fromInputStream(process.getInputStream)
    source.getLines.foreach(println)
  }
}
