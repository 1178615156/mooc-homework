package mook.probability
import Probability._
import mook.probability.ProbabilityDistribution.Binomial

/**
  * Created by yujieshui on 2017/1/22.
  */
object Week4 {
  def main(args: Array[String]): Unit = {
    val problem_5 = sum(7 to 10 map Binomial.probability(10, 0.6))
    val problem_6 = sum(0 to 3 map Binomial.probability(100, 0.03))
    println(
      s"""
         |problem_3   0.67
         |problem_4   0.54
         |problem_5   $problem_5
         |problem_6   $problem_6
      """.stripMargin)
  }
}
