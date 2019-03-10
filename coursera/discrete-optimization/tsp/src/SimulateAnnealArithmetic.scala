import java.io.PrintWriter

import scala.util.Random

object SimulateAnnealArithmetic {

  final case class Point(x: Float, y: Float)

  type Points = Array[Point]

  def distance(p1: Point, p2: Point): Float = math.sqrt(
    (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y)
  ).toFloat

  def totalDistance(points: Points): Float = {
    var sum = 0.0F
    var start = points.last
    for (pointIndex <- points.indices) {
      val point = points(pointIndex)
      sum += distance(start, point)
      start = point
    }
    sum
  }

  def reverse(array: Points, a: Int, b: Int): Unit = if(a < b) {
    val t = array(a)
    array(a) = array(b)
    array(b) = t
    reverse(array, a + 1, b - 1)
  }


  final class BestPath(var bestLoss: Float, var bestPoints: Points) {
    def update(path: Path): Unit = if(path.loss < bestLoss) {
      bestLoss = path.loss
      bestPoints = (Array.newBuilder[Point] ++= path.points).result()
    }
  }

  final class Path(val points: Points, _loss: Option[Float] = None) {
    val loss: Float = _loss.getOrElse(totalDistance(points))
    val size: Int   = points.length
  }

  val random = new Random()


  final class Neighbour(status: Path, coffer: Double) {
    val a = random.nextInt(status.points.length - 1)
    val b = a + math.max((random.nextInt(status.points.length - a - 1) * coffer).toInt, 1)


    val loss: Float = {
      val ps = status.points
      val a_before = (a - 1 + status.size) % status.size
      val b_after = (b + 1) % status.size
      (status.loss
        - distance(ps(a_before), ps(a)) - distance(ps(b), ps(b_after))
        + distance(ps(a_before), ps(b)) + distance(ps(a), ps(b_after))
        )
    }
    lazy val points: Points = {
      val result: Array[Point] = Array.ofDim[Point](status.size)
      val ps = status.points
      ps.copyToArray(result)
      reverse(result, a, b)
      result
    }

    lazy val newStatus: Path = new Path(points, Some(loss))
  }

  def accept(delta: Float, tmp: Double): Boolean = {
    delta < 0 || random.nextFloat() < math.exp(-delta / tmp)
  }

  def solution(start_t: Double, end_t: Double, iter_l: Int, initStatus: Path): Path = {
    var currStatus = initStatus
    val dec = 0.999
    var curr_t = start_t
    val bestPath = new BestPath(initStatus.loss, initStatus.points)
    while (curr_t > end_t) {
      for (i <- 0 until iter_l) {
        val neighbour = new Neighbour(currStatus, 1)
        val delta = neighbour.loss - currStatus.loss
        if(accept(delta, curr_t)) {
          currStatus = neighbour.newStatus
          bestPath.update(currStatus)
        }
      }
      curr_t *= dec
      println(s"tmp:$curr_t,loss:${currStatus.loss}")
    }
    new Path(bestPath.bestPoints)
  }

  def readData(fileName: String) = {
    val points = io.Source
      .fromFile(fileName)
      .getLines()
      .toSeq.tail
      .map(e => e.split(" ").toList match {
        case a :: b :: Nil => Point(a.toFloat, b.toFloat)
        case _             => ???
      }).toArray
    points
  }

  def main(args: Array[String]): Unit = {


    //    val filename = args.headOption.getOrElse("./data/tsp_51_1")
    //    val filename = args.headOption.getOrElse("./data/tsp_1889_1")
    val filename = args.headOption.getOrElse("./data/tsp_33810_1")
    val points = readData(filename)
    val result = solution(2000, 0.001, 20000, new Path(Array(points: _*)))
    if(args.isEmpty) {
      println(result.loss)
      //      println(result.points.map(e => points.indexOf(e)).toList)
    }

    val pw = new PrintWriter("result.out")
    pw.println(result.loss.toString + " 0")
    pw.println(result.points.map(e => points.indexOf(e)).mkString(" "))
    pw.close()
  }
}
