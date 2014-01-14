package com.algorithms.sort

import scala.annotation.tailrec

/**
 * Implementation of merge sort
 */
object MergeSort {

  def mergesort[T](comp: (T, T) => Boolean)(xs: List[T]): List[T] = {

    def merge(left: List[T], right: List[T]): List[T] = (left, right) match {
      case (Nil, _) => right
      case (_, Nil) => left
      case (l :: ls, r :: rs) if comp(l,r) => l :: merge(ls, right)
      case (l :: ls, r :: rs) if !comp(l,r) => r :: merge(left, rs)
    }

    if (xs.length < 2) xs
    else {
      val (left, right) = xs splitAt(xs.length / 2)
      merge(mergesort(comp)(left), mergesort(comp)(right))
    }

  }

  /**
   * Tail recursive implementation of merge sort.
   * @param comp
   * @param xs
   * @tparam T
   * @return
   */
  def msort[T](comp: (T, T) => Boolean)(xs: List[T]): List[T] = {
    @tailrec
    def merge(acc: List[T], left: List[T], right: List[T]): List[T] = (left, right) match {
      case (Nil, _) => acc.reverse ::: right
      case (_, Nil) => acc.reverse ::: left
      case (l :: ls, r :: rs) if comp(l,r) => merge(l :: acc, ls, right)
      case (l :: ls, r :: rs) if !comp(l,r) => merge(r :: acc, left, rs)
    }

    if (xs.length < 2) xs
    else {
      val (left, right) = xs splitAt(xs.length / 2)
      merge(List(), msort(comp)(left), msort(comp)(right))
    }
  }

  /**
   * Basic implementation of merge sort using arrays. In this by the book implementation we are copying the array
   * into new arrays each time
   * @param arr
   * @return
   */
  def mergesortArrays(arr: Array[Int]): Array[Int] = {

    def merge(left: Array[Int], right: Array[Int]): Array[Int] = {

      val n = left.length + right.length
      val resArray = Array.ofDim[Int](n)
      var i = 0
      var j = 0

      for (k <- 0 until n) {

        if (i >= left.length) {
          resArray(k) = right(j)
          j += 1
        } else if ( j >= right.length ) {
          resArray(k) = left(i)
          i += 1
        } else if ( left(i) < right(j) ) {
          resArray(k) = left(i)
          i += 1
        }
        else {
          resArray(k) = right(j)
          j += 1
        }
      }

      resArray
    }

    if ( arr.length < 2 ) arr
    else {
      val (left, right) = arr splitAt( arr.length / 2)
      merge(mergesortArrays(left), mergesortArrays(right))
    }

  }

}
