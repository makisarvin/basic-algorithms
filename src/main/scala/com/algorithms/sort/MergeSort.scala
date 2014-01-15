package com.algorithms.sort

import scala.annotation.tailrec

/**
 * Implementation of merge sort, both in "functional" and imperative way.
 * There are two implementations of the algorithm for "functional" style.
 * mergesort is a standard implementation that sorts a list of items
 * msort is the same as mergesort but it is tail recursive.
 *
 * The textbook implementation is on mergesortArrays, where an array of integers is sorted
 * in ascending order.
 */
object MergeSort {

  /**
   * Functional implementation of the merge sort algorithm. If the list has more than two elements then we split the
   * list in half and call the merge on both sub-lists.
   * The merge subroutine takes two lists and merges them according to the compare function
   * @param compare the compare function
   * @param xs the list of elements to sort
   * @tparam T type of the element list
   * @return a List(T) with the elements of xs in sorted order according to the compare function 
   */
  def mergesort[T](compare: (T, T) => Boolean)(xs: List[T]): List[T] = {

    def merge(left: List[T], right: List[T]): List[T] = (left, right) match {
      case (Nil, _) => right
      case (_, Nil) => left
      case (l :: ls, r :: rs) if compare(l,r) => l :: merge(ls, right)
      case (l :: ls, r :: rs) if !compare(l,r) => r :: merge(left, rs)
    }

    if (xs.length < 2) xs
    else {
      val (left, right) = xs splitAt(xs.length / 2)
      merge(mergesort(compare)(left), mergesort(compare)(right))
    }

  }

  /**
   * Tail recursive implementation of merge sort. The implementation is the same as in mergesort but we are using an
   * auxiliary list, for the tail recursion.
   * @param compare the compare function
   * @param xs the list of elements to sort
   * @tparam T type of the element list
   * @return a List(T) with the elements of xs in sorted order according to the compare function
   */
  def msort[T](compare: (T, T) => Boolean)(xs: List[T]): List[T] = {
    @tailrec
    def merge(acc: List[T], left: List[T], right: List[T]): List[T] = (left, right) match {
      case (Nil, _) => acc.reverse ::: right
      case (_, Nil) => acc.reverse ::: left
      case (l :: ls, r :: rs) if compare(l,r) => merge(l :: acc, ls, right)
      case (l :: ls, r :: rs) if !compare(l,r) => merge(r :: acc, left, rs)
    }

    if (xs.length < 2) xs
    else {
      val (left, right) = xs splitAt(xs.length / 2)
      merge(List(), msort(compare)(left), msort(compare)(right))
    }
  }

  /**
   * Textbook implementation of merge sort using arrays. In this by the book implementation we are copying the array
   * into new arrays each time. so merge will create a new array (resArray) with length the length of left and right
   * arrays. Then it will iterate over those two arrays and merge the results in ascending order.
   * @param arr the arrays of integers to sort.
   * @return a new arrays with the elements of the arr in ascending sorted order.
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

  /**
   * Counting inversions. The algorithm is described in the coursera course:
   * Algorithms: Design and Analysis, Part 1 by Tim Roughgarden (https://class.coursera.org/algo-003)
   * To count the inversions, we perform a merge sort and on the merge method we count the inversions when the right
   * list has elements that are smaller( or bigger depending on the compare function ).
   * @param compare the compare function
   * @param xs the list of elements to sort
   * @tparam T type of the element list
   * @return a List(T) with the elements of xs in sorted order according to the compare function
   */
  def countingInversions[T](compare: (T, T) => Boolean)(xs: List[T]): Int = {

    var inversions: Int = 0

    def countAndMerge(xs: List[T]): List[T] = {

      def merge(left: List[T], right: List[T]): List[T] = (left, right) match {
        case (Nil, _) => right
        case (_, Nil) => left
        case (l :: ls, r :: rs) if compare(l,r) => l :: merge(ls, right)
        case (l :: ls, r :: rs) if !compare(l,r) =>
          inversions += left.length
          r :: merge(left, rs)
      }

      if (xs.length < 2) xs
      else {
        val (left, right) = xs.splitAt(xs.length / 2)
        merge(countAndMerge(left), countAndMerge(right))
      }

    }
    countAndMerge(xs)
    inversions
  }

}
