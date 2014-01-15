package com.algorithms.sort

import org.scalatest.{Matchers, WordSpec}

/**
 * Tests for merge sort algorithm and applications of it.
 */
class MergeSortSpec extends WordSpec with Matchers {
  import MergeSort._

  "Testing the mergesort algorithm" when {

    "Merge sort " should {
      val lst = List(2,4,3,1)

      "sort list in ascending order" in {
        val sortedLst = mergesort[Int](_ < _)(lst)
        sortedLst should equal (List(1,2,3,4))
      }

      "sort list in descending order" in {
        val sortedLst = mergesort[Int](_ > _)(lst)
        sortedLst should equal (List(4,3,2,1))
      }

    }

    "Merge sort tail recursive" should {
      val lst = List(6,2,4,3,5,1,7)

      "sort list in ascending order" in {
        val sortedLst = msort[Int](_ < _)(lst)
        sortedLst should equal (List(1,2,3,4,5,6,7))
      }

      "sort list in descending order" in {
        val sortedLst = msort[Int](_ > _)(lst)
        sortedLst should equal (List(7,6,5,4,3,2,1))
      }

    }

    "Merge sort Arrays" should {
      "sort an array of integers of odd length" in {
        val arr = Array(3,2,1,4,5)
        mergesortArrays(arr) should equal( Array(1,2,3,4,5) )
      }

      "sort an array of integers of even length" in {
        val arr = Array(6,3,2,1,4,5)
        mergesortArrays(arr) should equal( Array(1,2,3,4,5,6) )
      }

      "return empty array in the case we sort the empty array" in {
        val arr = Array.empty[Int]
        mergesortArrays(arr) should equal (Array.empty[Int])
      }

      "return the same array in the case we sort the single value array" in {
        val arr = Array(1)
        mergesortArrays(arr) should equal (Array(1))
      }

    }
  }

  "Testing mergesort applications" when {
    "Counting inversions " should {
      "return 0 on 1,2,3,4,5" in {
        val xs = List(1,2,3,4,5)
        assert(countingInversions[Int](_<_)(xs) == 0)
      }

      "return 1 on 1,2,4,3,5" in {
        val xs = List(1,2,4,3,5)
        assert(countingInversions[Int](_<_)(xs) == 1)
      }

      "return 3 on 1,4,3,2,5" in {
        val xs = List(1,4,3,2,5)
        assert(countingInversions[Int](_<_)(xs) == 3)
      }
    }

  }

}
