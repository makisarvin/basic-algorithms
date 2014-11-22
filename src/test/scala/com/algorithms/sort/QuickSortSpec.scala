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

  "RSelect "should {
    val lst = List(2,3,4,2,1,45,6,78,3,6,356,2,646,3,656,7,4,67,56,64,56,7657,2,5,345,6)

    "find the element in 1st order" in {
      val array = lst.toArray
      select(array, 1) should equal(1)
    }

    "find the element in 5th order" in {
      val array = lst.toArray
      select(array, 5) should equal(2)
    }

    "find the element in 10th order" in {
      val array = lst.toArray
      select(array, 10) should equal(4)
    }

  }

}
