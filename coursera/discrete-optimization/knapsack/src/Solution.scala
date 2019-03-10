import scala.io.Source

object Solution {

  case class Item(index: Int, weight: Int, value: Int) {
    val density: Double = 1.0 * value / weight
  }

  def readInput(file: String): (Int, List[Item]) = {
    val line: List[List[String]] = Source.fromFile(file).getLines().toList.map(_.split(" ").toList)
    val capacity = line.head(1).toInt
    val items = line.tail.zipWithIndex.map {
      case (List(value, weight), index) => Item(index, weight.toInt, value.toInt)
    }
    capacity -> items
  }


  def estimate(capacity: Int, items: Seq[Item], result: Int = 0): Double = {
    if(items.isEmpty || capacity == 0)
      result
    else {
      val item = items.head
      if(item.weight > capacity)
        result + item.density * capacity
      else
        estimate(capacity - item.weight, items.tail, result + item.value)
    }
  }

  def solution_tree(capacity: Int, items: Seq[Item]): (Int, Seq[Int]) = {
    val itemSorted = items.sortBy(-_.density)
    var maxValue = 0.0
    var cache = Map[(Int, Seq[Item]), (Int, List[Int])]()

    def tree_search(curr_capacity: Int, curr_value: Int, curr_items: Seq[Item]): (Int, List[Int]) = {
      lazy val item = curr_items.head
      if(curr_capacity <= 0 || curr_items.isEmpty)
        0 -> List.empty[Int]
      else if(estimate(curr_capacity, curr_items) + curr_value <  math.max(maxValue,curr_value))
        0 -> List.empty[Int]
      else if(cache.contains((curr_capacity, curr_items)))
        cache(curr_capacity -> curr_items)
      else {
        maxValue = math.max(maxValue,curr_value)
        val (contain_value, contain_items) = tree_search(curr_capacity - item.weight, curr_value + item.value, curr_items.tail)
        val (no_contain_value, no_contain_items) = tree_search(curr_capacity, curr_value, curr_items.tail)
        val result =
          if(curr_capacity - item.weight < 0 || contain_value + item.value < no_contain_value)
            (no_contain_value, no_contain_items)
          else
            (contain_value + item.value, item.index +: contain_items)
        cache += (curr_capacity -> curr_items) -> result
        result
      }
    }

    val (value, taken) = tree_search(capacity, 0, itemSorted)
    value -> items.map(e => if(taken.contains(e.index)) 1 else 0)
  }

  def main(args: Array[String]): Unit = {
    val filename = args(0).split("=").last
    val (capacity, items) = readInput(filename)
    val (value, taken) = solution_tree(capacity, items)


    println(s"${value} 0")
    println(taken.mkString(" "))

  }
}
