package com.lunchparse.tests


import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import com.lunchparse._
import java.util.Calendar

class LunchTest extends Spec with ShouldMatchers {

  describe("Date Parsing") {
    it("Should get the correct dates from week") {
      val result = Lunch.date(35, 2011)
      val expected = List(("Måndag", "29/08"),
			  ("Tisdag", "30/08"),
			  ("Onsdag", "31/08"),
			  ("Torsdag", "01/09"),
			  ("Fredag", "02/09"))
      result should equal (expected)
    }
  }
  describe("HTML to text parsing") {
    it("Should filter out linebreaks") {
      val in = <div>
		Måndag<br/>
		<p>Måndagsgris</p>
		<p><br/><div>apan</div></p>
		<p>Tisdag</p>
		<p></p>
		<p><p></p></p>
		<p>Tisdagsko</p>
	       </div>
      val result = Lunch.trav(in, Set("p", "br"))
      val expected = List("Måndag", "Måndagsgris", "apan", "Tisdag", "Tisdagsko")
      result should equal (expected)
    }

    it("Should linebreak at different levels") {
      val in = <div><em><u>Tisdag</u></em>
	       <strong>HELSTEKT KOTLETTRAD</strong>
	       m. timjansky och stekt potatis
	       <strong>CHILI CON CARNE </strong>m. ris<br/>
	       </div>
      val result = Lunch.trav(in, Set("em", "strong"))
      result should equal (List("Tisdag", "HELSTEKT KOTLETTRAD m. timjansky och stekt potatis", "CHILI CON CARNE m. ris"))
    }

    it("Should replace xml whitespace with a space") {
      val in = <div>Hello
	       World</div>
      val result = Lunch.trav(in, Set())
      val expected = List("Hello World")
      result should equal (expected)
    }
    
    it("Should group days correctly") (pending) /* {
      val in = List("Måndag",
		    "apa",
		    "Tisdag",
		    "Gris",
		    "Onsdag",
		    "Kossa",
		    "Torsdag",
		    "Fisk",
		    "Fredag",
		    "Aj")
      val in2 = Lunch.date(32, 2011)
      Lunch.group(in) should equal (List())
    } */
  }
}
