package com.algorithms.numeric

import org.scalatest.{Matchers, WordSpec}

/**
 * Created by arvange on 03/02/14.
 */
class NumericCalculationsSpec extends WordSpec with Matchers  {

  import NumericCalculations._

  "Finding the GDC of two numbers" should {
    "return 12 in 60,24" in {
      gdc(60,24) should equal (12)
    }

    "return 12 in 24,60" in {
      gdc(24,60) should equal (12)
    }

    "return 231 in 4851,3003" in {
      gdc(4851,3003) should equal (231)
    }

    "return 1 in 53, 11" in {
      gdc(53,11) should equal (1)
    }
  }

}
