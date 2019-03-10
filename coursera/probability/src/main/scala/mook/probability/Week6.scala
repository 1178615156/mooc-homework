package mook.probability
import Probability._
import mook.probability.ProbabilityDistribution.{Binomial, Exponential, NegativeBinomial, Poisson}


/**
  * Created by yujieshui on 2017/4/16.
  */
object Week6 {
  def main(args: Array[String]): Unit = {
    val p1 = ""
    val p2 = Binomial.expectation(5, 1.0 / 4)
    val p3 = {
      val pd = Binomial.probability(4, 1.0 / 4) _
      val seq = Seq(
        (0 * 4.0 + 4.0) -> pd(0),
        (1 * 4.0 + 4.0) -> pd(1),
        (2 * 4.0 + 4.0) -> pd(2),
        (3 * 4.0 + 4.0) -> pd(3),
        (4 * 4.0 + 4.0) -> pd(4)
      )
      println(seq.mkString("\n"))
      println(Probability.expectation(seq))
      Probability.variance(seq) ** 0.5
    }
    val p4 = ((1 to 9 map (i => i * math.pow(0.6, i) * 0.4)) :+ (10 * math.pow(0.6, 10))).sum

    val p5 = Seq(
      3 * NegativeBinomial.probability(3, 0.6)(3),
      4 * NegativeBinomial.probability(4, 0.6)(3),
      5 * NegativeBinomial.probability(5, 0.6)(3),
      3 * NegativeBinomial.probability(3, 0.4)(3),
      4 * NegativeBinomial.probability(4, 0.4)(3),
      5 * NegativeBinomial.probability(5, 0.4)(3)
    ).sum


    println(

      s"""
         |p2 : ${p2.scaleTo(2)}
         |p3 : ${p3.scaleTo(2)}
         |p4 : ${p4.scaleTo(2)}
         |p5 : ${p5.scaleTo(2)}
      """.stripMargin)
  }
}
