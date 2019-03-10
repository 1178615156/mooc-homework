import scala.util.Random

object SA {
  val random = new Random()

  case class Point(x: Double, y: Double)

  case class Facility(index: Int, setup_cost: Double, capacity: Int, location: Point)

  case class Customer(index: Int, demand: Int, location: Point)

  def length(point1: Point, point2: Point): Double = math.sqrt(
    (point1.x - point2.x) * (point1.x - point2.x) + (point1.y - point2.y) * (point1.y - point2.y)
  )

  case class Env(customers: Array[Customer], facilities: Array[Facility]) {
    val customer_demand   = customers.map(_.demand)
    val facility_capacity = facilities.map(_.capacity)

    def total_loss(array: Array[Int]): Double = {
      var result = 0.0
      var used = Set.empty[Facility]
      for (i <- customers.indices) {
        val customer = customers(i)
        val facility = facilities(array(i))
        used += facility
        result += length(customer.location, facility.location)
      }
      used.foldLeft(result)(_ + _.setup_cost)
    }

    def genFacilityCapacity(array: Array[Int]): Array[Int] = {
      val rt: Array[Int] = (Array.newBuilder[Int] ++= facility_capacity) result()
      for (i <- array.indices) {
        rt(array(i)) = rt(array(i)) - customer_demand(i)
        assert(rt(array(i)) >= 0)
      }
      rt
    }
  }

  final class BestSolution[@specialized(Int) T](var loss: Double, var values: Array[T]) {
    def update(loss: Double, values: Array[T]): Boolean = {
      if(loss < this.loss) {
        this.loss = loss
        values.copyToArray(this.values)
        true
      } else {
        false
      }
    }
  }

  case class Solution(array: Array[Int])(implicit env: Env) {

    import env._

    val size                               = array.length
    val loss                  : Double     = total_loss(array)
    val used_facility_capacity: Array[Int] = genFacilityCapacity(array)

  }

  case class Neighbor(solution: Solution)(implicit env: Env) {

    import env._

    def legitimate(f: Int) = {
      solution.used_facility_capacity(f) >= customer_demand(customerPoint)
    }

    val customerPoint = random.nextInt(solution.size)
    val customer      = customers(customerPoint)
    val oldFacility   = facilities(solution.array(customerPoint))
    val facilityPoint = {
      var facilityPoint = random.nextInt(facilities.length)
      var i = 0
      var newFacility = facilities(facilityPoint)
      var oldCost = length(customer.location, oldFacility.location)
      var newCost = length(customer.location, newFacility.location)
      while (i < facilities.length && !legitimate(facilityPoint) ) {
        i += 1
        facilityPoint += 1
        facilityPoint %= facilities.length
        newCost = length(customer.location, newFacility.location)
      }
      if(i == facilities.length)
        solution.array(customerPoint)
      else
        facilityPoint

    }
    val loss          = {
      val newFacility = facilities(facilityPoint)

      val a =
        if(oldFacility.capacity == solution.used_facility_capacity(oldFacility.index) + customer.demand) {
          -oldFacility.setup_cost
        } else {
          0
        }
      val b =
        if(newFacility.capacity == solution.used_facility_capacity(newFacility.index)) {
          +facilities(facilityPoint).setup_cost
        } else {
          0
        }
      val c =
        (
          +length(customer.location, newFacility.location)
            - length(customer.location, oldFacility.location)
          )
      solution.loss + a + b + c
    }
    lazy val newSolution = {
      val newArray = (Array.newBuilder[Int] ++= solution.array).result()
      newArray(customerPoint) = facilityPoint
      Solution(newArray)
    }
  }

  def genInitSolution()(implicit env: Env) = {
    import env._
    val array = Array.ofDim[Int](env.customers.length)
    var f = 0
    var f_capacity = facilities(f).capacity
    var i = 0
    while (i < array.length) {
      if(customers(i).demand <= f_capacity) {
        f_capacity -= customers(i).demand
        array(i) = f
        i += 1
        f += 1
        f = f % facilities.length
      } else {
        f += 1
        f = f % facilities.length
        f_capacity = facilities(f).capacity
      }
    }
    Solution(array)
  }

  def accept(delta: Double, tmp: Double): Boolean = {
    delta < 0 || random.nextFloat() < math.exp(-delta / tmp)
  }


  def ea(start_t: Double, end_t: Double, iter_l: Int, dec: Double, initStatus: Solution)(implicit env: Env) = {
    var currStatus = initStatus
    var curr_t = start_t
    val bestPath = new BestSolution[Int](currStatus.loss, currStatus.array)
    var solutionNoChangeNum = 0

    def balance(size: Int) = {
      for (i <- 0 until size) {
        val neighbor = Neighbor(currStatus)
        val delta = neighbor.loss - currStatus.loss
        if(accept(delta, curr_t)) {
          currStatus = neighbor.newSolution
          bestPath.update(currStatus.loss, currStatus.array)
        }
      }
    }

    while (curr_t > end_t) {
      val startTime = System.currentTimeMillis()
      balance(iter_l)
      val endTime = System.currentTimeMillis()

      curr_t *= dec
      //      if(endLoss == startLoss) {
      //        solutionNoChangeNum += 1
      //      } else {
      //        solutionNoChangeNum = 0
      //      }
      //      if(solutionNoChangeNum > 100) {
      //        curr_t = end_t
      //      }
      println(s"tmp:$curr_t, loss:${currStatus.loss}, time:${endTime - startTime}")
    }

    Solution(bestPath.values)

  }

  def main(args: Array[String]): Unit = {
    val inputData = io.Source.fromFile("./data/fl_100_1").getLines().toList.map(_.split(" ").toList.map(_.toDouble))
    val facility_count :: customer_count :: Nil = inputData(0)
    val facilities = inputData.tail.take(facility_count.toInt).zipWithIndex.map {
      case (a :: b :: c :: d :: Nil, index) => Facility(index, a, b.toInt, Point(c, d))
    }.toArray
    val customers = inputData.tail.drop(facility_count.toInt).zipWithIndex.map {
      case (b :: c :: d :: Nil, index) => Customer(index, b.toInt, Point(c, d))
    }.toArray

    implicit val env = Env(customers, facilities)
    val solution = ea(
      start_t = 10000,
      end_t = 1,
      iter_l = 10000,
      dec = 0.999,
      genInitSolution()
    )
    println(solution.loss)
    println(solution.array.mkString(" "))
    println(solution.used_facility_capacity.mkString(" "))

  }

}












