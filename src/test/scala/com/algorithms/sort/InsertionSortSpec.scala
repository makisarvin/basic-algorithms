package com.algorithms.sort

import org.scalatest.{Matchers, FlatSpec}

/**
 * Testing the insertion sort algorithm
 */
class InsertionSortSpec extends FlatSpec with Matchers {
  import InsertionSort._

  "Insertion Sort" should "insert values in order " in {
    val unsortedArray = Array(5,3,6,2,7,1,9)
    val sortedArray = insertionSortArraysOfInt(unsortedArray)
    sortedArray should equal(Array(1,2,3,5,6,7,9))
  }


}
