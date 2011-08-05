package com.lunchparse

import scala.io.Source
import scala.io.Codec
import scala.xml._
import java.net.URL
import scala.collection
import scala.collection.JavaConversions._

import java.util.{ TimeZone, Calendar }

object Lunch {
  System.setProperty("http.proxyHost", "www-proxy.ericsson.se")
  System.setProperty("http.proxyPort", "8080")
  val days = List("M\345ndag ", "Tisdag ", "Onsdag ", "Torsdag ", "Fredag ")

  def date(week: Int, year: Int) = {
    val tz = TimeZone.getTimeZone("Europe/Stockholm")
    val cal = Calendar.getInstance(tz)
    cal.setWeekDate(year, week, Calendar.MONDAY)
    cal
  }

  def menu = {
    days.foldLeft((gotaAlv, gothia, aran, NodeSeq.Empty))((acc, day) => {
      val (gota :: gotatl, goth :: gothtl, ar :: artl, res) = acc
      val res2 = res ++ <div><h2>{ day }</h2><h3>Ericsson Restaurant</h3><pre>{ gota.mkString("\n").toLowerCase }</pre><h3>Gothia</h3><pre>{ goth.mkString("\n").toLowerCase }</pre><h3>Aran</h3><pre>{ ar.mkString("\n").toLowerCase }</pre></div>
      (gotatl, gothtl, artl, res2)
    })._4
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

  def bistrot = {
    val xml = Web.get("http://www.lindholmen.se/sv/dagens-lunch?keys=&field_restaurant_nid=166&date_filter%5Bvalue%5D%5Byear%5D=2011&date_filter%5Bvalue%5D%5Bmonth%5D=6&date_filter%5Bvalue%5D%5Bday%5D=1")
    //val str = trav(xml \\ "table", Set("td")).reverse
    val table = (xml \\ "tbody")(0) \ "tr"
    val tds = table.flatMap((x) => (x \ "td")(1) ++ <br/>)
    trav(tds, Set("br")).reverse
  }

  def aran = {
    val xml = Web.get("http://www.rams.se/index.php?page=mod_matsedel", "ISO-8859-1")
    val str = trav(xml \\ "table", Set("tr")).reverse
    val lines = Source.fromString(str.dropWhile(!days.contains(_)).takeWhile(!_.startsWith("Dagens Pasta")).mkString).getLines
    group(lines)
  }

  private def group(lines: Iterator[String]) = {
    lines.foldLeft(List(): List[List[String]])((acc, line) => {
      if (days.contains(line)) {
        List() :: acc
      } else {
        val hd :: tl = acc
        (line :: hd) :: tl
      }
    }).map(_.reverse).reverse
  }

  def trav(nodes: NodeSeq, lineBreaks: Set[String]): List[String] = trav(nodes, lineBreaks, true, List())._1.reverse
  private def trav(nodes: NodeSeq, lineBreaks: Set[String], broken: Boolean, acc: List[String]): (List[String], Boolean) = {
    nodes.foldLeft((acc, broken))((accin, node) => {
      val (acc, broken) = accin
      node match {
        case Text(text) =>
          val format = text.replaceAll("\240", " ").lines.map(_.trim).mkString(" ").trim
          if (format != "") {
            if (acc.length > 0) {
              val hd :: tl = acc
              ((format + hd) :: tl, false)
            } else {
              (List(format), false)
            }
          } else
            accin
        case _ if lineBreaks.contains(node.label) && !broken =>
          trav(node.child, lineBreaks, true, "" :: acc)
        case _ =>
          trav(node.child, lineBreaks, false, acc)
      }
    })
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

  def get(url: String, encoding: String = "UTF-8") = {
    val source = new org.xml.sax.InputSource(url)
    source.setEncoding(encoding)
    adapter.loadXML(source, parser)
  }
}

