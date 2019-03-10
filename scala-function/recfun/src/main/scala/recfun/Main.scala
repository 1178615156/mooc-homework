package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  def factorial(i: BigInt): BigInt =
    if (i == 0) 1
    else
      i * factorial(i - 1)

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): BigInt = {
    val n = c
    val m = r
    (factorial(m) / (factorial(n) * factorial(m - n))).toInt
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean =
    chars.foldLeft(Option(0)) { (l, r) => r match {
      case '(' => l map (_ + 1)
      case ')' => l filter (_ > 0) map (_ - 1)
      case _   => l
    }
    }.contains(0)

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else
      coins.sorted.reverse.filter(_ <= money) match {
        case Nil           => 0
        case head :: other =>
          val newWaitCount = 0 to (money / head) map (_ * head) map (money - _)
          (newWaitCount map (m => countChange(m, other))).sum
      }
  }
}
