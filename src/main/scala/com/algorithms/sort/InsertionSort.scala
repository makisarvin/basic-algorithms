package com.algorithms.sort

/**
 * Classic insertion sort algorithm in both mutable arrays form and immutable lists form.
 * The algorithm says that:
 *  take an element from position j (key) and move backwards until you find an element that is bigger than this key
 *  when you find that element then move it one position up. If you reached the end of the elements or there is no
 *  other element bigger then key then put key in the hole that is there.
 *
 *  For example: Suppose we have the array bellow and j is on position 6 (so key = array[6] = 4)
 *    [1,2,3,5,6,4,7]
 *
 *    now move backwards. While a[i] > key, move a[i] one position up
 *    i = 5, a[i] = 6:  [1,2,3,5, ,6,7]
 *    i = 4. a[i] = 5:  [1,2,3, ,5,6,7]
 *
 *    if you reach the end or a[i] < key then put key in the empty position
 *    i = 3, a[i] = 3: [1,2,3,4,5,6,7]
 *
 * The  pseudo code for the algorithm is:
 *
 * insertionSort(A)
 *  for j = 2 to A.length
 *    key = A[j]
 *    index = j - 1
 *
 *    while  index > 0 and A[index] > key
 *      A[index + 1] = A[index]
 *      index = index - 1
 *    A[index + 1] = key
 */

object InsertionSort {

  /**
   * Classic insertion sort using Array. The array is mutable, so we are swapping the elements in place.
   * This is the classic implementation however is not the safest. The array is sorted in place, so whoever
   * uses the array, he will have the sorted version now. It is important to see that in the following snippet:
   *
   * val arr = Array(5,4,3,6,1)
   * val sortedArr = insertionSortArraysOfInt(arr)
   *
   * // now arr is the same sortedArray e.g.
   * print arr        // prints Array(1,3,4,5,6)
   * print sortedArr  // prints Array(1,3,4,5,6)
   *
   *
   * @param arr an array of integers
   * @return the same array in sorted in place. we could just return Unit here and use the original array.
   */
	def insertionSortArraysOfInt(arr: Array[Int]) = {
    for(i <- 1 until arr.length) {
      val key = arr(i)
      var index = i

      while( index > 0 && arr(index - 1) > key) {
        arr(index) = arr(index - 1)
        index = index - 1
      }
      arr(index) = key
    }
    arr
  }

  /**
   * This is a more functional approach to insertion sort. We are using immutable lists so we will copy the elements
   * to a second list, which is returned. The rational is simple:
   * We will traverse all the elements in the original list and all them in a sorted list. To do that we are using
   * foldLeft. foldLeft takes an empty list and an insert function and adds the elements in the correct order in the
   * List.
   * Insert function has three cases:
   *  * if the compare function is true (e.g. the element in the list is bigger than the item to be inserted) then it
   *    inserts the item in front of that portion of the list (since all the rest items are bigger).
   *  * if the compare function is false then it continues to the next part of the list
   *  * if we reached the end of the list or the list is empty, just add the item there (e.g. it is bigger than the rest
   *    of the items already in the list).
   * @param compf the compare function. the first parameter is the existing item in the list and the second is the item
   *              to be inserted
   * @param lst   the unsorted list
   * @tparam T    type of items e.g. Int
   * @return      a sorted List[T]
   */
  def insertionSort[T](compf: (T, T) => Boolean)(lst: List[T]) = {
    def insert(sorted: List[T], item: T): List[T] = sorted match {
      case x :: xs if compf(x,item) => item :: sorted
      case x :: xs if !compf(x,item) => x :: insert(xs, item)
      case _ => sorted :+ item
    }

    lst.foldLeft(List.empty[T])((ls,item) => insert(ls,item))
  }
}