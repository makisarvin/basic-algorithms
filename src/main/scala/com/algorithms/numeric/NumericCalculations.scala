package com.algorithms.numeric

/**
 * Created by arvange on 03/02/14.
 */
object NumericCalculations {

  /**
   * Finds the greater common divisor between two numbers.
   * This is based on Euklid's algorithm:
   *
   * while b > 0 :
   *   remainder <- a mod b
   *   a <- b
   *   b <- remainder
   * return a
   *
   * @param a the first number
   * @param b the second number
   * @return
   */
  def gdc(a: Int, b: Int): Int = b match {
    case 0 => a
    case _ => gdc(b, a % b)
  }

  /**
   * The exponation function works on the premise of the following facts for exponation:
   * A^(2*M) = (A^M)^2
   * A^(M+N) = A^M * A ^N
   * @param num the number to raise in a power
   * @param exp the poser to raise to
   * @return
   */
  def exponation(num: Int, exp: Int): Double  = {
    def recurExponents( acc: Double, exponent: Int, until: Int): (Int, Double) = exponent match {
      case x if (2 * x > until ) => (x, acc)
      case _ =>
        recurExponents(acc * acc, 2 * exponent, until)
    }

    def iteration(sum: Double, until: Int): Double = {

      val (exp, rsum) = recurExponents(num, 1, until)

      val partialSum = sum * rsum
      val exponent = until - exp

      exponent match {
        case x if x > 1 => iteration(partialSum, exponent)
        case x if x == 1 => partialSum * num
        case _ => partialSum
      }
    }

    exp match {
      case 0 => 1
      case _ => iteration(1, exp)
    }

  }
}