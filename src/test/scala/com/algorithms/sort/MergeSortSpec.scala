package com.algorithms.sort

import org.scalatest.{Matchers, WordSpec}

/**
 * Created by arvange on 17/12/13.
 */
class MergeSortSpec extends WordSpec with Matchers {
  import MergeSort._

  "Merge sort " should {
    "sort array in place" in {
      val arr = Array(2,4,3,1)
      mergeSortArrayOfInt(arr)
      println(arr)
      arr should equal (Array(1,2,3,4))
    }
  }

}
