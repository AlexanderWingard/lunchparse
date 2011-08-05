package com.lunchparse.tests


import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import com.lunchparse._
import java.util.Calendar

class LunchTest extends Spec with ShouldMatchers {

  describe("Date Parsing") {
    it("Should get the correct dates from week") {
      val result = Lunch.date(32, 2011)
      val expected = List(("Måndag", "08/08"),
			  ("Tisdag", "09/08"),
			  ("Onsdag", "10/08"),
			  ("Torsdag", "11/08"),
			  ("Fredag", "12/08"))
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

    it("Should replace xml whitespace with a space") {
      val in = <div>Hello
	       World</div>
      val result = Lunch.trav(in, Set())
      val expected = List("Hello World")
      result should equal (expected)
    }
  }
}
