package com.algorithms.sort

/**
 * Implementation of the quicksort algorithm. The "imparative" way uses an array of integers and performs quicksort
 * using in place swaps.
 * The pseudocode for the algorithm is very simple:
 *
 * quicksort(array, start, finish):
 *  if end - start < 2 return array
 *  else
 *   pivot = selectPivot()
 *   split_position = partition(array, start, end, pivot)
 *   quicksort(array, start, split_position)
 *   quicksoft(array, split_position + 1, end)
 *
 *
 * Quicksort algorithm.
 */
object QuickSort {

  /**
   * Imperative implementation of the quicksort algorithm.
   * Note that partition is traversing the array from start until end. This means that the end element is not taken
   * into account. (otherwise it would be to instead of until). This allows us to write qSort1(array, 0, array.length)
   * and also in the recurcive calls we don't need to add the +1 in the split_position
   * @param array the array to be sorted
   * @param start should be 0
   * @param end should be array.length
   */
  private[this] def qSort1(array: Array[Int], start: Int, end: Int): Unit = {
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

  /**
   * Swaping two alements in an array. Utility method.
   * @param array the array
   * @param i the position of the first element to swap
   * @param j the position of the second element to swap
   */
  private [this] def swap(array: Array[Int], i: Int, j: Int) = {
    val temp = array(i)
    array(i) = array(j)
    array(j) = temp
  }

  /**
   * Calls the qSort algorithms with correct parameters
   * @param array the array to be sorted in ascending order.
   */
  def qSort(array: Array[Int]): Unit = {
    qSort1(array, 0, array.length)
  }


  /**
   * Functional implementation of the quicksort.
   * For simplicity pivot is always the first element.
   * This is a recursive function that states:
   * While there is a pivot and something else:
   *   filter all the elements lower than the pivot and  all the elements bigger that the pivot
   *  Call quicksort on the array lower than the pivot and the array bigger that the pivot
   *  add the lower than pivot sorted array, the pivot and the bigger than pivot sorted array in a list.
   * @param lst the list to be sorted.
   * @param f. The compare function. e.g. (_ < _) for ascending order
   * @return the sorted list.
   */
  def quicksort[T](lst: List[T])(f: (T, T) => Boolean): List[T] = lst match {
    case pivot :: xs =>
      quicksort(xs filter( f(_, pivot) ))(f)++ List(pivot) ++ quicksort(xs filter(f(pivot, _) ))(f)
    case _ => lst
  }

  /**
   * An implementation of select. The purpose of this function is to select the k smallest number in the array.
   * The complexity of this algorithm is 0(n).
   * example:
   *  array: [5,3,1,7,6,4]
   *  k = 2 e.g. find the 3rd minimum number
   *  result should be: 4
   * @param array the unsorted array
   * @param k the k minimum number.
   * @return the value on order k
   */
  def select(array: Array[Int], k : Int): Int  = {

    def rSelect(array: Array[Int], k: Int, start: Int, end: Int): Array[Int] = {

      def partition(array: Array[Int], start: Int, end: Int, pivot: Int): Int = {
        var i = start +1
        for ( j <- start + 1 until end) {
          if (array(j) < pivot) {
            swap(array, i,j)
            i +=1
          }
        }
        swap(array, start, i-1)
        i
      }

      val pivot = array(0)
      val pivot_position = partition(array, start, end, pivot)
      if ( pivot_position > k) {
         rSelect(array, k, start, pivot_position)
      } else if (pivot_position < k ) {
        rSelect(array, k, pivot_position, end)
      } else {
        array
      }
    }

    val pointer = k-1
    val resArray = rSelect(array, pointer, 0, array.length)
    resArray(pointer)
  }


}
