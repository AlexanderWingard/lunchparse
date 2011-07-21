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
  val days = List("M\345ndag ", "Tisdag ", "Onsdag ", "Torsdag ", "Fredag ")

  def menu = {
    days.foldLeft((gotaAlv, gothia, NodeSeq.Empty))((acc, day) => {
      val (gota :: gotatl, goth :: gothtl, res) = acc
      val res2 = res ++ <div><h2>{day}</h2><h3>Ericsson Restaurant</h3><pre>{gota.mkString("\n")}</pre><h3>Gothia</h3><pre>{goth.mkString("\n")}</pre></div>
      (gotatl, gothtl, res2)
    })._3
  }

  def gotaAlv = {
    val xml = Web.get("http://www.kvartersmenyn.se/start/rest/11579")
    val lineBreaks = Set("p", "h2", "strong", "u")
    val str = trav(xml \\ "table", lineBreaks).reverse
    val lines = Source.fromString(str.dropWhile(!days.contains(_)).takeWhile(!_.contains("Varje dag")).mkString).getLines
    group(lines)
  }

  def gothia = {
    val xml = Web.get("http://www.restauranggothia.com/lunch.htm")
    val str = trav(xml \\ "table", Set("p", "br")).reverse
    val lines = Source.fromString(str.dropWhile(!days.contains(_)).takeWhile(!_.startsWith("Veckans")).mkString).getLines
    group(lines)
  }

  private def group(lines : Iterator[String]) = {
    lines.foldLeft(List() : List[List[String]])((acc, line) => {
      if(days.contains(line)) {
	List() :: acc
      } else {
	val hd :: tl = acc
	(line :: hd) :: tl
      }
    }).map(_.reverse).reverse
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

