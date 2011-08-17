package com.lunchparse

import scala.io.Source
import scala.io.Codec
import scala.xml._
import java.net.URL
import scala.collection
import scala.collection.JavaConversions._
import scala.util.matching.Regex

import java.util.{ TimeZone, Calendar }
import java.text.SimpleDateFormat

object Lunch {
  System.setProperty("http.proxyHost", "www-proxy.ericsson.se")
  System.setProperty("http.proxyPort", "8080")
  val days = List("M\345ndag", "Tisdag", "Onsdag", "Torsdag", "Fredag")

  def getDates(lines: Iterable[String], regex: String) = {
    val weekExp = new Regex(regex)
    val week = lines.find(weekExp.findFirstMatchIn(_).isDefined)
    val sdf = new SimpleDateFormat("dd/MM")
    val toString = (in : Calendar) =>  { sdf.format(in.getTime()) }
    week match {
      case Some(weekExp(w, y)) =>
	date(Some(w.toInt), Some(y.toInt), toString)
      case Some(weekExp(w)) =>
	date(Some(w.toInt), None, toString)
      case None =>
	days.map((_, "xx/xx"))
    }
  }

  def date(week: Option[Int], year: Option[Int], resulter : (Calendar) => String) = {
    val tz = TimeZone.getTimeZone("Europe/Stockholm")
    val cal = Calendar.getInstance(tz)
    year.map(cal.set(Calendar.YEAR, _))
    week.map((w : Int) => cal.set(Calendar.WEEK_OF_YEAR, w + 1))
    cal set(Calendar.DAY_OF_WEEK, Calendar.MONDAY)
    days.map((day) => {
      val str = resulter(cal)
      cal.add(Calendar.DAY_OF_WEEK, 1)
      (day,str)
    })
  }

  def menu = {
    days.foldLeft((gotaAlv, gothia, aran, lskitch, bistrot, NodeSeq.Empty))((acc, day) => {
      val (gota :: gotatl, goth :: gothtl, ar :: artl, ls :: lstl, bi :: bitl, res) = acc
      val res2 = res ++
<div>
      <h2>{ day }</h2>
      <h3>Ericsson Restaurant</h3>
      { gota.map((line) => {<div>{capitalize(line)}</div> })}
      <h3>Gothia</h3>
      { goth.map((line) => {<div>{capitalize(line)}</div> })}
      <h3>Aran</h3>
      { ar.map((line) => {<div>{capitalize(line)}</div> })}
      <h3>L's Kitchen</h3>
      { ls.map((line) => {<div>{capitalize(line)}</div> })}
      <h3>Bistrot</h3>
      { bi.map((line) => {<div>{capitalize(line)}</div> })}
</div>
      (gotatl, gothtl, artl, lstl, bitl, res2)
    })._6
  }

  def capitalize(str : String) : String = {
    if(str.length > 1)
	str.head.toString.toUpperCase + str.tail.toLowerCase	
    else
      str
  }

  def gotaAlv = {
    val xml = Web.get("http://www.kvartersmenyn.se/start/rest/11579")
    val lineBreaks = Set("p", "h2", "strong", "u")
    val str = trav(xml \\ "table", lineBreaks)
    val dates = getDates(str, """Vecka (\d+),\s+(\d+)""")
    val lines = str.dropWhile(!days.contains(_)).takeWhile(!_.contains("Varje dag"))
    group(lines, dates)
  }

  def gothia = {
    val xml = Web.get("http://www.restauranggothia.com/lunch.htm")
    val str = trav(xml \\ "table", Set("p", "br"))
    val dates = getDates(str, """Meny V\. (\d+)""")
    val lines = str.dropWhile(!days.contains(_)).takeWhile(!_.startsWith("Veckans"))
    group(lines, dates)
  }

  def bistrot = {
    val urlGen = (cal: Calendar) => {
      val year = cal.get(Calendar.YEAR)
      val month = cal.get(Calendar.MONTH) + 1
      val day = cal.get(Calendar.DAY_OF_MONTH)
      "http://www.lindholmen.se/sv/dagens-lunch?keys=&field_restaurant_nid=166&date_filter%5Bvalue%5D%5Byear%5D=" + year +"&date_filter%5Bvalue%5D%5Bmonth%5D="+ month +"&date_filter%5Bvalue%5D%5Bday%5D=" + day
    }
    
    val wholeMenu = date(None, None, urlGen).flatMap((in: (String, String)) => {
      val (day, url) = in
      val xml = Web.get(url)
      val table = (xml \\ "tbody")(0) \ "tr"
      val tds = table.flatMap((x) => (x \ "td")(1) ++ <br/>)
      day :: trav(tds, Set("br"))
    })
    val sdf = new SimpleDateFormat("dd/MM")
    val dates = date(None, None, (cal : Calendar) => sdf.format(cal.getTime))
    group(wholeMenu, dates)
  }

  def aran = {
    val xml = Web.get("http://www.rams.se/index.php?page=mod_matsedel", "ISO-8859-1")
    val str = trav(xml \\ "table", Set("tr"))
    val dates = getDates(str, """.* vecka (\d+)""")
    val lines = str.dropWhile(!days.contains(_)).takeWhile(!_.startsWith("Dagens Pasta"))
    group(lines, dates)
  }

  def lskitch = {
    val xml = Web.get("http://www.chalmerskonferens.se/sv/Lindholmen/Restauranger-Cafeer/Veckans-luncher")
    val str = trav(xml, Set("tr", "div"))
    val dates = getDates(str, """Meny v\.(\d+)""")
    val lines = str.dropWhile(!days.contains(_)).takeWhile(_ != "NYHETER")
    group(lines, dates)
  }

  def group(lines: Iterable[String], dates : List[(String, String)]) = {
    lines.foldLeft((List(): List[List[String]], dates))((acc, line) => {
      val (lst, date) = acc
      if (date.exists(_._1 == line)) {
        ((List(date.head._2) :: lst), date.tail)
      } else {
        val hd :: tl = lst
        ((line :: hd) :: tl, date)
      }
    })._1.map(_.reverse).reverse
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
              ((hd + " " + format).trim :: tl, false)
            } else {
              (List(format), false)
            }
          } else {
	    accin
	  }
        case _ if lineBreaks.contains(node.label) && !broken =>
          trav(node.child, lineBreaks, true, "" :: acc)
        case _ =>
          trav(node.child, lineBreaks, broken, acc)
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

