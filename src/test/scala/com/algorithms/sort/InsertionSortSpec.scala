package com.algorithms.sort

import org.scalatest.{WordSpec, Matchers, FlatSpec}

/**
 * Testing the insertion sort algorithm
 */
class InsertionSortSpec extends WordSpec with Matchers {
  import InsertionSort._

  "Insertion Sort" should {
    "insert values in order " in {
      val unsortedArray = Array(5,3,6,2,7,1,9)
      val sortedArray = insertionSortArraysOfInt(unsortedArray)
      sortedArray should equal(Array(1,2,3,5,6,7,9))
    }

    "insert values the functional way" in {
      val unsortedList = List(3,4,2,1,5,7,6)
      val sortedList = insertionSort[Int](_ > _)(unsortedList)
      println(sortedList)
      sortedList should equal(List(1,2,3,4,5,6,7))
    }
  }

}
