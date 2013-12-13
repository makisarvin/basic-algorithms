package com.algorithms.sort

object InsertionSort {
		
	def insertionSortArraysOfInt(arr: Array[Int]) = {
    for(i <- 1 until arr.length) {
      val item = arr(i)
      var index = i

      while( index > 0 && arr(index - 1) > item) {
        arr(index) = arr(index - 1)
        index = index - 1
      }
      arr(index) = item
    }
    arr
  }

}