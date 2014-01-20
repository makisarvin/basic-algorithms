package com.algorithms.sort

import org.scalatest.{Matchers, WordSpec}

/**
 * Description of quicksort pending.
 */
class QuickSortSpec extends WordSpec with Matchers {

  "Quicksort" should {
    "sort an array of integers" in {
      val array = Array(5,3,4,1,2,7,6)
      QuickSort.qSort1(array, 0, array.length)

      array should equal (Array(1,2,3,4,5,6,7))
    }

    "sort another array of integers " in {
      val array = Array(1,3,4,5,2,7,6)
      QuickSort.qSort1(array, 0, array.length)

      array should equal (Array(1,2,3,4,5,6,7))
    }

    "sort third array of integers " in {
      val array = Array(5,3,4,6,2,7,1)
      QuickSort.qSort1(array, 0, array.length)

      array should equal (Array(1,2,3,4,5,6,7))
    }
  }

}
