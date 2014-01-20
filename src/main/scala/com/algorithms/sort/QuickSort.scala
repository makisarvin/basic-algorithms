package com.algorithms.sort

/**
 * Test cases for quicksort algorithm
 */
object QuickSort {

  def qSort1(array: Array[Int], start: Int, end: Int): Unit = {
    if (end - start < 2) array
    else {
      val pivot = array(start)
      val split_position: Int = partition(array, start, end, pivot)
      qSort1( array, start, split_position )
      qSort1( array, split_position, end )
    }

    def partition(array: Array[Int], start: Int, end: Int, pivot: Int): Int = {
      var i = start + 1
      for ( j <- start + 1 until end) {
        if (array(j) < pivot) {
          swap(array, i,j)
          i += 1
        }
      }
      swap(array,start,i-1)
      i
    }

  }

  def swap(array: Array[Int], i: Int, j: Int) = {
    val temp = array(i)
    array(i) = array(j)
    array(j) = temp
  }

}
