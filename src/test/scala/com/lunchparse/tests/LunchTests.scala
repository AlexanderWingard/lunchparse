package com.lunchparse.tests


import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import com.lunchparse._
import java.util.Calendar

class LunchTest extends Spec with ShouldMatchers {

  describe ("Date Parsing") {
    it("Should get the correct dates from week") {
      val res = Lunch.date(31, 2011)
      res.get(Calendar.YEAR) should equal (2011)
      res.get(Calendar.MONTH) should equal (Calendar.AUGUST)
      res.get(Calendar.DAY_OF_MONTH) should equal (1)
    }

    it("Should return text version correctly") {
      val in = <div>
		<p>Måndag</p>
		<p>Måndagsgris</p>
		<p>Tisdag</p>
		<p>Tisdagsko</p>
	       </div>
      val result = Lunch.trav(in, Set("p"))
      val expected = List("Måndag", "\n", "Måndagsgris", "\n", "Tisdag", "\n", "Tisdagsko")
      result should equal (expected)
    }
  }
}
