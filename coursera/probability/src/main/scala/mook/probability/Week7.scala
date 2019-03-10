package mook.probability

import Probability._
import mook.probability.ProbabilityDistribution.{Binomial, Exponential, NegativeBinomial, Poisson}

/**
  * Created by yujieshui on 2017/4/17.
  */
object Week7 {
  def main(args: Array[String]): Unit = {
    val p1 = "Y = 2⋅X+1，則Var[Y]=17"
    val p2 = 10
    val p3 = {
      val l1 = 1.0 / 12
      val l2 = 1.0 / 6
      14
    }
    val p4 = {
      val p = Exponential.cdf(1.0 / 3)(7.0 / 3)
      p * (1 - p)
    }
    val p5 = Exponential.cdf(1.0 / 3)(1.0 / 3)
    println(
      s"""
         |p1 = ${p1}
         |p2 = ${p2}
         |p3 = ${p3.scaleTo(2)}
         |p4 = ${p4.scaleTo(2)}
         |p5 = ${p5.scaleTo(2)}
         |p6 = 0.75
         |p7 = ${3.0 / 8}
      """.stripMargin)
  }
}

object Week8 {
  def main(args: Array[String]): Unit = {
    val p2 = 12
    println(
      s"""
         |p2 : ${p2.scaleTo(2)}
      """.stripMargin)
  }
}

object Week9 {
  def main(args: Array[String]): Unit = {
    //[10.5] derivative ((e^x - e^(7 x))/(6 (1-e^x)))^3 at x=0
    //[119] second derivative ((e^x - e^(7 x))/(6 (1-e^x)))^3 at x=0
    val p1 = (119.0 - (10.5 ** 2)) / (3 * 3)
    val p2 = ""
    val p3 = ""
    //[8] derivative (e^(6 (e^x -1)) - e^(12 (e^x -1 )) ) / (3 (1-e^(2 (e^x -1)))) at x=0
    //[(224.0 / 3)] second derivative (e^(6 (e^x -1)) - e^(12 (e^x -1 )) ) / (3 (1-e^(2 (e^x -1)))) at x=0
    val p4 = (224.0 / 3) - (8 * 8)
    println(
      s"""
         |p1 = ${p1.scaleTo(2)}
         |p4 = ${p4.scaleTo(2)}
         |56= 0.96
      """.stripMargin)
  }
}

object End {

  implicit class With_%%(double: Double) {
    def %% = double / 100
  }

  def main(args: Array[String]): Unit = {
    val p2 = 0.67
    val p3 = (0.3 %%) * (98.0 %%) / (1 - ((2.0 %%) * (0.2 %%) + (98.0 %%) * (99.7 %%)))
    val p9 =
      Poisson.probability(4, 3)(15) * Poisson.probability(3, 3)(10) /
        (0 to 25 map (i => Poisson.probability(4, 3)(i) * Poisson.probability(3, 3)(25 - i))).sum
    println((0 to 25 map (i => Poisson.probability(4, 3)(i) * Poisson.probability(3, 3)(25 - i))).sum)
    //    (0 to 25 map (i => Poisson.probability(12, 1)(i) * Poisson.probability(9, 1)(25 - i))).sum
    val p1_信親 = for {
      a <- 1 to 6
      b <- 1 to 6
      c <- 1 to 6
      d <- 1 to 6
      e <- 1 to 6
      f <- 1 to 6
      if a != 1 && b != 2 && c != 3 && d != 4 && e != 5 && f != 6
      if Set(a, b, c, d, e, f).size == 6
    } yield {
      List(a, b, c, d, e, f)
    }
    println(p1_信親.size)
    println(
      s"""
         |p1  = 生日
         |      no “一寸光陰一寸金”
         |      no Coursera
         |      no 小正同時擲兩個公正六面骰子，骰子點數和大於等於10的機率是1/6
         |      no 學校的工友先生中午要把六封分別寄送給不同老師的信親自送去
         |p2  = $p2
         |p3  = ${p3.scaleTo(2)}; 0.53 error 0.55 errer
         |p4  = 0.54 error
         |p5  = 若有一隨機變數T其MGF為ΦT(s)=11−2s，則Var[T]=8 ${8 - 2 * 2}
         |p6  = FX,Y(4,1)=19
         |      --错误
         |      P (4,1) = 0
         |      P y|x=4(0) = 1/3
         |      p x(4) = 1/12
         |      p y(2) = 4/9
         |      E[X] = 7
         |p7  = 0.25
         |
         |p8  = FX,Y(x,y)={1−e−(x+y),x≥0,y≥00,otherwise 是個符合規定(合法)的 Joint CDF 形式
         |p9  = ${p9.scaleTo(2)} error ;
         |p10 = 小文與阿霆同時搭到公車的機率為 0，這也代表兩人同時搭到公車的事件不可能發生
         |p11 = 9.6
         |p12 = 若X1∼Exp(λ1)且X2∼Exp(λ2) ，若X1,X2獨立，則 Y=X1+X2 的分佈是 Exp(λ1+λ2)
         |p13 = 0.97
         |p14 = PN|W=3(n)=14,n∈{3,4,5,6}
      """.stripMargin)
    4
    9
    13
    15
  }
}