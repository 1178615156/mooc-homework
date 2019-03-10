import Solution._

object KnapsackTest {


  def main(args: Array[String]): Unit = {
//    val (capacity, items) = readInput("D:/mook-homework/dp/knapsack/data/ks_30_0") //99798
//    val (capacity, items) = readInput("D:/mook-homework/dp/knapsack/data/ks_50_0")//142156
//        val (capacity, items) = readInput("D:/mook-homework/dp/knapsack/data/ks_400_0")
//    val (capacity, items) = readInput("D:/mook-homework/dp/knapsack/data/ks_1000_0")
    val (capacity, items) = readInput("D:/mook-homework/dp/knapsack/data/ks_10000_0")

    val (value, taken) = solution_tree(capacity, items)
    println((
      capacity,
      (items.zip(taken).collect { case (x, 1) => x }.map(_.weight).sum),
    ))
    println(s"${value}")
    println(taken.mkString(" "))
  }

}
