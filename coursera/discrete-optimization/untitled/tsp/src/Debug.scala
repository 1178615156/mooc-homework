import scala.util.Random

object Debug {
  def readData(fileName: String) = {
    val points = io.Source
      .fromFile(fileName)
      .getLines()
      .toSeq.tail
      .map(e => e.split(" ").toList match {
        case a :: b :: Nil => Point(a.toFloat, b.toFloat)
      }).toArray
    points
  }

  def main(args: Array[String]): Unit = {

    println(Random.nextInt(1))
    println(Random.nextInt(1))
    println(Random.nextInt(1))
    println(Random.nextInt(1))
    println(Random.nextInt(1))
  }
}
