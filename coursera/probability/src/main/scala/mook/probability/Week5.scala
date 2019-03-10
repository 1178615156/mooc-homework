package mook.probability

import Probability._
import mook.probability.ProbabilityDistribution.{Binomial, NegativeBinomial, Poisson}

import scala.math.BigDecimal.RoundingMode

/**
  * Created by yujieshui on 2017/2/2.
  */

object Week5 {


  def main(args: Array[String]): Unit = {
    val problem_3 = math.pow(0.75, 3).scaleTo(2)
    val problem_4 = "3*y^2"
    val problem_5 = Poisson.probability(1, 1)(0).scaleTo(2)
    val problem_6 = Poisson.probability(1, 2)(0).scaleTo(2)
    val problem_7 = Poisson.probability(1, 1)(0).scaleTo(2)
    val problem_8 = 1 - sum(0 to 5 map Poisson.probability(2, 3))

    println(
      s"""
         |problem_3  ${problem_3}
         |problem_4  ${problem_4}
         |problem_5  ${problem_5}
         |problem_6  ${problem_6}
         |problem_7  ${problem_7}
         |problem_8  ${problem_8}
         |
      """.stripMargin)
  }
}

