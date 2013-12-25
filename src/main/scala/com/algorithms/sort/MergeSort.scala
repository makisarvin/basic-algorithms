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
   * Tail recursive implementation of merge.
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

}
