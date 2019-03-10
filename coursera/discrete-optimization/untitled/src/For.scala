final object For {

  import scala.annotation.tailrec

  def test_array(array: Array[Int]): Int = {
//    var i = 0
//    var sum = 0
//    while (i < array.length) {
//      sum += array(i)
//      i += 1
    array.sum
  }
  @tailrec
  def test_list(list: List[Int], result: Int = 0): Int = {
    if(list.isEmpty) result
    else test_list(list.tail, list.head + result)
  }

  def test_indexseq(indexedSeq: IndexedSeq[Int]): Int = {
    var i = 0
    var sum = 0
    while (i < indexedSeq.length) {
      sum += indexedSeq(i)
      i += 1
    }
    sum
  }


  def timer[F](n: Int, f: () => F): Long = {
    val start_time = System.currentTimeMillis()
    for (i <- 0 until n) {
      f()
    }
    val end_time = System.currentTimeMillis()

    end_time - start_time
  }

  def main(args: Array[String]): Unit = {
    val n = 10000
    val m = 500000
    val array = Array.range(0, n)
    val list = List.range(0, n)
    val indexedSeq = Range(0, n)

    (timer(1000, () => test_array(array)))
    (timer(1000, () => test_list(list)))
    (timer(1000, () => test_indexseq(indexedSeq)))
    //    array: 1776
    //    list: 17294
    //    indexedSeq: 17334
    println("array: " + timer(m, () => test_array(array)))
    println("list: " + timer(m, () => test_list(list)))
    println("indexedSeq: " + timer(m, () => test_indexseq(indexedSeq)))

  }

}
