package com.algorithms.sort

/**
 * Created by arvange on 17/12/13.
 */
object MergeSort {

  def mergeSortArrayOfInt(arr: Array[Int]) = {

    def mergeSort(low: Int, high: Int): Unit = {
      if (low < high) {
        val middle = low + (high - low) / 2
        mergeSort(low, middle)
        mergeSort(middle + 1, high)

        merge(low, middle, high)
      }
    }

    def merge( low: Int, middle: Int, high: Int) = {

      val n1 = middle - low + 1
      val n2 = high - middle

      val left = new Array[Int](n1)
      val right = new Array[Int](n2)

      for (i <- 0 until n1) {
        left(i) = arr(low + i)
      }
      for( j <- 0 until n2) {
        right(j) = arr(middle + j + 1)
      }
      println(s"low, middle, high = $low, $middle, $high")
      println(s"n1,n2 = ($n1,$n2)")
      println(s"left = ${left.mkString(",")}, right=${right.mkString(",")} arr = ${arr.mkString(",")}")
      var i = 0
      var j = 0
      for (k <- low until high) {
        println(s"k=$k, i=$i and j=$j")
        if (left(i) <= right(j)) {
          arr(k) = left(i)
          i += 1
        } else {
          arr(k) = right(j)
          j += 1
        }
      }

    }

    mergeSort(0,arr.length)

  }

}
