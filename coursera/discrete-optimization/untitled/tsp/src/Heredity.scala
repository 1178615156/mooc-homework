import java.io.PrintWriter

import scala.collection.immutable.HashSet
import scala.math.Ordering
import scala.util.Random

final case class Point(x: Float, y: Float)

final class Env(val points: Array[Point], val populationSize: Int) {
  val p_mutation: Double = 0.01
  val p_crossover        = 0.5
  val gene_size : Int    = points.length
  val random             = new Random()

  def genRandom() = new Random()

  def randomGeneInt() = random.nextInt(gene_size)

  def randomVal() = random.nextFloat()

  def distance(p1: Point, p2: Point): Float = math.sqrt(
    (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y)
  ).toFloat

  val costMatrix: Array[Array[Float]] = {
    val x = Array.fill(gene_size, gene_size)(0.0F)
    for {
      i <- 0 until gene_size
      j <- 0 until i
    } yield {
      x(i)(j) = distance(points(i), points(j))
      x(j)(i) = x(i)(j)
    }
    x
  }

  def totalDistance(nodes: Array[Int]): Double = {
    var sum = 0.0
    var start = nodes.last
    nodes.foreach { i =>
      sum += costMatrix(start)(i)
      start = i
    }
//    sum += costMatrix(start)(nodes.head)
    sum
  }

  def total_distance(nodes: Seq[Int]): Double = {

    var sum = 0.0
    var startPoint = points(nodes.head)

    nodes.tail.foreach { index =>
      sum += distance(startPoint, points(index))
      startPoint = points(index)
    }
    sum += distance(startPoint, points(nodes.head))
    sum
  }


}

object Gene {
  type GeneValue = Array[Int]

  @inline
  def swap(array: GeneValue, a: Int, b: Int): Unit = {
    val tmp = array(a)
    array(a) = array(b)
    array(b) = tmp
  }

  def shuffle(array: GeneValue)(implicit env: Env): Unit = {
    import env._
    for (n <- array.length to 2 by -1) {
      swap(array, n - 1, randomGeneInt())
    }
  }

  def genRandom(size: Int = 0)(implicit env: Env): GeneValue = {
    val array = Array.range(0, if(size == 0) env.gene_size else size)
    shuffle(array)
    array
  }

  def genRange(size: Int = 0)(implicit env: Env): GeneValue =
    Array.range(0, if(size == 0) env.gene_size else size)

  def genFix(size: Int = 0)(implicit env: Env): GeneValue = {
    Array.fill(if(size == 0) env.gene_size else size)(-1)
  }
}

import Gene.GeneValue

final class Gene(val value: Gene.GeneValue) {
  def crossover(gene: Gene)(implicit env: Env): Seq[Gene] = {
//    val a = env.randomGeneInt()
//    val b = env.randomGeneInt()
//    val start = math.min(a, b)
//    val end = math.max(a, b) + 1


    val start = env.randomGeneInt()
    val end = start + env.random.nextInt(env.gene_size - start) + 1


    def do_cross(father: GeneValue, mother: GeneValue): GeneValue = {

      val cutGeneValue = mother.slice(start, end)
      val cutSet: Set[Int] = HashSet(cutGeneValue:_*)
      val newGeneValue = Array.newBuilder[Int]
      var i =0
      father.foreach{value =>
        if (i == start){
          newGeneValue ++= cutGeneValue
          i += 1
        }
        if (!cutSet.contains(value)){
          newGeneValue += value
          i += 1
        }

      }
      if (i == start){
        newGeneValue ++= cutGeneValue
        i += 1
      }
//      val diff = father.filter(e=> !cutSet.contains(e))
//      val diff = father diff cutGeneValue
//      val newGeneValue = Array.newBuilder[Int] ++= diff.slice(0, start) ++= cutGeneValue ++= diff.drop(start)
      val x = newGeneValue.result()
      x
    }

    val father = value
    val mother = gene.value
    List(
      do_cross(father, mother),
      do_cross(mother, father).reverse
    ).map(e => new Gene(e))
  }

  def mutation(p: Double)(implicit env: Env): Gene = {
    import env._
    val newValue = value
    val random = genRandom()
    value.indices.filter(e => random.nextFloat() < p_mutation + p).foreach { index =>
      val b = randomGeneInt()
      Gene.swap(newValue, index, b)
    }
    new Gene(newValue)
  }

  def copy()(implicit env: Env): Gene = {
    val newValue = Gene.genFix()
    value.copyToArray(newValue)
    new Gene(newValue)

  }
}

final class Individual(val gene: Gene) {
  var __fitness = 0.0

  def crossover(individual: Individual)(implicit env: Env): Seq[Individual] = {
    this.gene.crossover(individual.gene).map(e => new Individual(e))
  }

  def mutation(p: Double = 0)(implicit env: Env): Individual = {
    new Individual(gene.mutation(p))
  }

  def fitness()(implicit env: Env): Double = {
    if(__fitness == 0)
      this.__fitness = env.totalDistance(gene.value)
    __fitness
  }

  def copy()(implicit env: Env) = new Individual(gene.copy())
}

object Individual {
  implicit def ordering(implicit env: Env): Ordering[Individual] = Ordering.by(_.fitness)
}

final class Population(val individuals: Array[Individual]) {
  val deliverySize = 12
  def evolution()(implicit env: Env): Population = {
    val delivery = individuals.slice(0, deliverySize)
    val newIndividuals = Array.newBuilder[Individual]
    newIndividuals ++= delivery.map(_.copy())
    newIndividuals ++= delivery

    val children = (deliverySize).until(individuals.length).par.map { i =>
      val father = individuals(i)
      if(0 < deliverySize * 2) {
        val mother1 = individuals(env.random.nextInt(individuals.length))
        val mother2 = individuals(env.random.nextInt(individuals.length))
        val mother3 = individuals(env.random.nextInt(individuals.length))
        List(mother1, mother2,mother3).flatMap(m => father.crossover(m)).map(_.mutation()).seq
      } else {
        val mother = individuals(env.random.nextInt(individuals.length))
        father.crossover(mother).map(_.mutation())

      }
    }.seq
    children.foldLeft(newIndividuals)(_ ++= _) //.foreach(newIndividuals ++= _)
    new Population(competition(newIndividuals.result()))
  }

  def competition(individuals: Array[Individual])(implicit env: Env): Array[Individual] = {
    java.util.Arrays.sort(individuals, Individual.ordering)
    val result = individuals.slice(0, env.populationSize)
    result
  }

  def bastIndividual() = individuals(0)

  def bastFitness()(implicit env: Env) = bastIndividual().fitness()

  def crossover(population: Population)(implicit env: Env) = {
    val a = individuals
    val b = population.individuals
    val new_a = env.random.shuffle(a.indices.toList).flatMap { i =>
      a(i).crossover(b(i))
    }
    val new_b = env.random.shuffle(a.indices.toList).flatMap { i =>
      b(i).crossover(a(i))
    }
    new Population(competition(new_a.toArray)) -> new Population(competition(new_b.toArray))
  }
}

object Heredity {

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
    val filename = args.headOption.getOrElse("./data/tsp_5_1")
    implicit val env: Env = new Env(readData(filename), 512)
    var population = new Population(Array.fill(env.populationSize)(new Individual(new Gene(Gene.genRandom()))))

    (1 to 50).foreach { i =>
      val startTime = System.currentTimeMillis()
      for (i <- 0 until 100) {
        population = population.evolution()
        //        env.p_mutation = 0.0 + env.randomVal() % 0.1
      }
      val afterFitness = population.bastFitness()
      val endTime = System.currentTimeMillis()
      println(s"iter $i , fitness:$afterFitness time:${(endTime - startTime) / 1000.0}")

    }
    val pw = new PrintWriter("result.out")
    pw.println(env.total_distance(population.bastIndividual().gene.value).toString + " 0")
    pw.println(population.bastIndividual().gene.value.mkString(" "))
    pw.close()
    //    Thread.sleep(100000000)
  }
}