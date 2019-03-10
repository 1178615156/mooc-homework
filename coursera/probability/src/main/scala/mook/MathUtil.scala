package mook

import scala.math.BigDecimal.RoundingMode
import scala.math.{E, pow}

/**
  * Created by yujieshui on 2017/2/2.
  */
trait MathUtil {
  def sum[A, B >: A](l: Seq[A])(implicit num: scala.Numeric[B]): B = l.sum(num)

  def factorial(n: Long): Long =
    if(n == 0) 1L else n * factorial(n - 1)

  /**
    * 从 n 中选 m 个
    * @param n
    * @param m
    * @return
    */
  def combination(n: Long, m: Long): Long =
    (0L until m map (n - _)).product / (1L to m).product

  def arrangement(n: Long, m: Long): Long =
    factorial(n) / factorial(m)

  def c(n: Long, m: Long): Long = combination(n, m)

  implicit class WithOpts(double: Double) {
    def scaleTo(n: Int): BigDecimal = BigDecimal(double).setScale(n, RoundingMode.HALF_UP)

    def **(n: Double) = math.pow(double, n)
  }


}



