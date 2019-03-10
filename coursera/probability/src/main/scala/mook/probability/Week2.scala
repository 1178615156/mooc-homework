package mook.probability

/**
  * Created by yujieshui on 2017/2/2.
  */
object Hmbb {
  type Hmbb = Seq[Seq[Boolean]]
  val hmbb: Hmbb = Seq(
    Seq(true, true, true, true, true),
    Seq(true, false, true, false, true),
    Seq(true, true, true, true, true),
    Seq(true, true, true, true, true)
  )
  val head_hmbb  = hmbb.take(3)

  def updateRow(row: Int)(seq: Seq[Seq[Boolean]]): Seq[Seq[Boolean]] = {
    seq.updated(row, 1 to seq.head.size map (_ => false))
  }

  def updateColumn(column: Int)(seq: Seq[Seq[Boolean]]): Seq[Seq[Boolean]] = {
    seq.transpose.updated(column, 1 to seq.transpose.head.size map (_ => false)).transpose
  }

  def update(x: Int, y: Int)(hmbb: Hmbb) = {
    hmbb.updated(x, hmbb(x).updated(y, false))
  }

  def count(hmbb: Hmbb) = {
    val each =
      (hmbb.indices zip hmbb) map {
        case (x, line) => line.indices zip line collect {
          case (y, true) =>
            val l = updateColumn(y)(updateRow(x)(hmbb))
            val a = l.drop(x)
            val result =
            //              (a.head.drop(y) +: a.tail).flatten.count(e => e)
              updateColumn(y)(updateRow(x)(hmbb)).flatten.count(e => e)
            result
        }
      }
    each.flatten.sum / 2
  }

  def show(hmbb: Hmbb) = hmbb.map(_.map(e => if(e) "-" else "o").mkString(" ")).mkString("\n")

  def main(args: Array[String]): Unit = {
    println(1.0 * count(updateRow(1)(updateColumn(1)(hmbb.take(3)))) / count(updateRow(1)(updateColumn(1)(hmbb))))
    println(1.0 * count(updateRow(1)(updateColumn(2)(hmbb.take(3)))) / count(updateRow(1)(updateColumn(2)(hmbb))))
    println(1.0 * count(updateRow(1)(updateColumn(4)(hmbb.take(3)))) / count(updateRow(1)(updateColumn(4)(hmbb))))

  }
}
object Week2 {

}
