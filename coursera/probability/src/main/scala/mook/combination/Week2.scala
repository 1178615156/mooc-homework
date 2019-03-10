package mook.combination

import CombinationMath._

/**
  * Created by yujieshui on 2017/2/2.
  */
object Week2 {

  def u1 = {
    def nig(e: Int) = e match {
      case 1  => Seq(e, 7, 2)
      case 7  => Seq(e, 6, 1)
      case ee => Seq(e, e - 1, e + 1)
    }

    val result = for {
      a <- 1 to 7
      b <- 1 to 7 filterNot (e => nig(a).contains(e))
      c <- 1 to 7 filterNot (e => nig(a).contains(e) || nig(b).contains(e))
    } yield
      Seq(a, b, c)

    result.map(_.sorted).distinct
  }

  def main(args: Array[String]): Unit = {

    println(u1.mkString("\n"))
    println(u1.size)


    val H1_1 = 1 until 10000 count (_.toString.contains("1"))
    val H1_2 = 1 until 10000 count (_.toString.contains("0"))

    println(
      s"""
         |H1_1 $H1_1
         |H1_2 $H1_2
         |H2_1 ${5 + 8}  //{(2 + 3) + (3 + 5)}
         |H2_2 ${5 + 8}  //{(2 + 3) + (3 + 4)}
         |H2_3 ${5 + 8}  //{(2 + 3) + (1 + 2) * (2 * 2)}
         |H3   ${reCombination(3, 9)}
         |
         |U1   ${u1.size}
         |U2   ${notAdjacent(10, 5)}
       """.stripMargin)
  }
}
