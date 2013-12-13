package com.algorithms.sort

object Sort {
	
	def main(args: Array[String]) {
		val size = readLine().toInt
		val array = readLine().split(" ").toArray.map(_.toInt)
		insertionSort(array)
	}
		
	
	def insertionSort(list: Array[Int]) = 
	for(i <- 1 until list.length) {
		if( list(i - 1) > list(i) ) 
		swap(i, list)
	}
	
	
	def swap(position: Int, list: Array[Int]) = {
		val temp = list(position)
		var tempPlaced = false //not the best solution to break the loop
		
		for ( i <- position until 0 by -1 ) {
			if ( list(i - 1) > temp ) {
				list(i) = list(i - 1)
				println(list.mkString(" "))
			} else if (!tempPlaced){
				list(i) = temp
				tempPlaced = true
				println(list.mkString(" "))
			}
		}
	}
	
}