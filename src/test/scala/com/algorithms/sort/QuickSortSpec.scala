package com.algorithms.sort

import org.scalatest.{Matchers, WordSpec}

/**
 * Description of quicksort pending.
 */
class QuickSortSpec extends WordSpec with Matchers {

  import QuickSort._

  "Quicksort in place" should {
    "sort an array of integers" in {
      val array = Array(5,3,4,1,2,7,6)
      qSort(array)

      array should equal (Array(1,2,3,4,5,6,7))
    }

    "sort another array of integers " in {
      val array = Array(1,3,4,5,2,7,6)
      qSort(array)

      array should equal (Array(1,2,3,4,5,6,7))
    }

    "sort third array of integers " in {
      val array = Array(5,3,4,6,2,7,1)
      qSort(array)

      array should equal (Array(1,2,3,4,5,6,7))
    }
  }

  "quicksort functional approach" should {
    " sort a list of integers" in {
      val lst = List(3,4,5,6,8,2,1)
      val result = quicksort[Int](lst)(_<_)

      result should equal (List(1,2,3,4,5,6,8))
    }
  }

}
