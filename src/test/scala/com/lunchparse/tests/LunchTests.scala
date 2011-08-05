package com.lunchparse.tests


import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import com.lunchparse._
import java.util.Calendar

class LunchTest extends Spec with ShouldMatchers {

  describe("Date Parsing") {
    it("Should get the correct dates from week") {
      val res = Lunch.date(31, 2011)
      res.get(Calendar.YEAR) should equal (2011)
      res.get(Calendar.MONTH) should equal (Calendar.AUGUST)
      res.get(Calendar.DAY_OF_MONTH) should equal (1)
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
  }
}
