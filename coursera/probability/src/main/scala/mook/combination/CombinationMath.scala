package mook.combination

import mook.MathUtil

/**
  * Created by yujieshui on 2017/2/2.
  */
object CombinationMath extends MathUtil {
  def multiArrangement(n: Long, arr: Long*): Long =
    factorial(n) / arr.map(e => factorial(e)).product

  def reCombination(n: Long, r: Long): Long =
    combination(n + r - 1, r)

  def partition(n: Int, m: Int): Int =
    (m to (n + m)).product

  def notAdjacent(n: Long, r: Long): Long =
    combination(n - r + 1, r)
}
