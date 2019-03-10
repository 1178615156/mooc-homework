package mook.probability

/**
  * Created by yujieshui on 2017/2/2.
  */

object Week3 {
  def postman = {
    val litter = 1 to 4
    val result = for {
      a <- litter
      b <- litter.filter(e => e != a)
      c <- litter.filter(e => e != a && e != b)
      d <- litter.filter(e => e != a && e != b && e != c)
    } yield
      a :: b :: c :: d :: Nil

    val all_error =
      result.filter { l =>
        1 to l.size zip l forall { case (i, n) => i != n }
      }
    result.size
  }

  def main(args: Array[String]): Unit = {
    val problem2 = 0.33
    val problem3 = 243100
    val problem4 = "p(c1 / t) = 1 / 3 ;p(c2 | h) = 2 / 5"
    val problem5 = 56
    val problem6 = 9
    val problem7 = postman
    val problem8 =
      0.4 * (0.2 * 1.0 + 0.8 * 3.0 / 4) +
        0.3 * (0.2 * 3.0 / 4 + 0.8 * 3.0 / 4) +
        0.2 * (0.2 * 3.0 / 4 + 0.8 * 3.0 / 4) +
        0.1 * (0.2 * 3.0 / 4 + 0.8 * 1.0)
    println(
      s"""
         |problem2 : $problem2
         |problem3 : $problem3
         |problem4 : $problem4
         |problem5 : $problem5
         |problem6 : $problem6
         |problem7 : $problem7
         |problem8 : $problem8
      """.stripMargin)
  }
}