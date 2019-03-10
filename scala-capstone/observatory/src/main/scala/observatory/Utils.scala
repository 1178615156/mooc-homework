package observatory

import scala.collection.GenIterable

/**
  * Created by yujieshui on 2017/3/21.
  */
object Utils {

  def average(seq: GenIterable[Double]): Double = {
    val (s, l) = seq.foldLeft((0.0, 0))((t, r) => (t._1 + r, t._2 + 1))
    s / l
  }
}
