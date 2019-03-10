package mook.probability

import mook.MathUtil

import scala.language.postfixOps
import scala.math.{E, pow}

/**
  * Created by yujieshui on 2017/2/2.
  */
object Probability extends MathUtil {
  def expectation(vs: Seq[(Double, Double)]): Double = {
    (vs map { case (v, p) => v * p }).sum
  }

  def variance(vs: Seq[(Double, Double)]): Double = {
    val m = expectation(vs)
    (vs map { case (v, p) => ((v - m) ** 2) * p }).sum
    //    (vs map { case (v, p) => (v ** 2) * p } sum) - m ** 2
  }

  def average = expectation _
}

object ProbabilityDistribution extends MathUtil {

  object Binomial {
    /**
      *
      * @param n 次试验
      * @param p 试验发生的概率
      * @param i 期望发生的次数
      * @return 概率
      */
    def probability(n: Int, p: Double)(i: Int): Double =
      combination(n, i) * pow(p, i) * pow(1 - p, n - i)

    def expectation(n: Int, p: Double): Double =
      n * p

    def variance(n: Int, p: Double): Double =
      n * p * (1 - p)
  }

  object Geometric {
    def probability(k:Int,p:Double) = ((1 - p) ** (k-1)) * p
    def expectation(p:Double)= 1/ p

  }

  object NegativeBinomial {
    def probability(n: Int, p: Double)(r: Int): Double = {
      if(r == 0)
        (1 - p) ** n
      else
        combination(n - 1, r - 1) *
          (p ** r) *
          ((1 - p) ** (n - r))
    }
  }

  object Poisson {
    /**
      *
      * @param r 频率
      * @param t 时间
      * @param k 期望发生次数
      * @return
      */
    def probability(r: Double, t: Double)(k: Int): Double =
      pow(r * t, k) / factorial(k) / pow(E, r * t)
  }

  object Exponential {
    def pdf(lambda: Double)(x: Double) = lambda * (math.E ** (-1 * lambda * x))

    def cdf(lambda: Double)(x: Double) = 1 - (math.E ** (-1 * lambda * x))

    def expectation(lambda: Double) = 1 / lambda

    def variance(lambda: Double) = 1 / (lambda ** 2)
  }

}
