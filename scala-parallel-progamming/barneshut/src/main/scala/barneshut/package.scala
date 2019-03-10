import common._
import barneshut.conctrees._

import scala.util.Try

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX

    def massY: Float = centerY

    def mass: Float = 0

    def total: Int = 0

    def insert(b: Body): Quad = Leaf(
      centerX = centerX,
      centerY = centerY,
      size = size,
      bodies = Seq(b)
    )
  }

  case class Fork(nw: Quad, ne: Quad, sw: Quad, se: Quad) extends Quad {
    def ll = List(nw, ne, sw, se)

    def nonEmptyMass = ll.filter(_.mass > 0)

    val centerX: Float = ll.map(_.centerX).sum / 4
    val centerY: Float = ll.map(_.centerY).sum / 4
    val size   : Float = ll.map(_.size).sum / 2
    val mass   : Float = ll.map(_.mass).sum
    val massX  : Float = if (nonEmptyMass.isEmpty) centerX else nonEmptyMass.map(e => Piont(e.massX, e.massY, e.mass)).reduceLeft(avalage).x
    val massY  : Float = if (nonEmptyMass.isEmpty) centerY else nonEmptyMass.map(e => Piont(e.massX, e.massY, e.mass)).reduceLeft(avalage).y
    val total  : Int   = ll.map(_.total).sum

    def rightPointX(b: Quad) = b.centerX + b.size / 2

    def rightPointY(b: Quad) = b.centerY + b.size / 2

    def insert(b: Body): Fork = {
      val point =
        if (b.x < centerX && b.y < centerY) nw
        else if (b.x >= centerX && b.y < centerY) ne
        else if (b.x < centerX && b.y >= centerY) sw
        else if (b.x >= centerX && b.y >= centerY) se
        else ???

      point match {
        case `nw` => Fork(nw.insert(b), ne, sw, se)
        case `ne` => Fork(nw, ne.insert(b), sw, se)
        case `sw` => Fork(nw, ne, sw.insert(b), se)
        case `se` => Fork(nw, ne, sw, se.insert(b))
      }
    }
  }

  case class Piont(x: Float, y: Float, mass: Float)

  def avalage(a: Piont, b: Piont): Piont = {
    val rx = b.x - a.x
    val ry = b.y - a.y
    val s = b.mass / (a.mass + b.mass)
    Piont(s * rx + a.x, s * ry + a.y, a.mass + b.mass)
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
    extends Quad {

    val (mass, massX, massY) = (
      bodies.filter(_.mass > 0).map(_.mass).sum: Float,
      bodies.map(e => Piont(e.x, e.y, e.mass)).reduceLeft(avalage).x: Float,
      bodies.map(e => Piont(e.x, e.y, e.mass)).reduceLeft(avalage).y: Float
      )
    val total: Int           = bodies.size

    def insert(b: Body): Quad =
      if (size <= minimumSize)
        Leaf(centerX, centerY, size, bodies :+ b)
      else {
        val nw: Quad = Empty(centerX - size / 4, centerY - size / 4, size / 2)
        val ne: Quad = Empty(centerX + size / 4, centerY - size / 4, size / 2)
        val sw: Quad = Empty(centerX - size / 4, centerY + size / 4, size / 2)
        val se: Quad = Empty(centerX + size / 4, centerY + size / 4, size / 2)
        (bodies :+ b).foldLeft(Fork(nw, ne, sw, se)) { case (r, b) =>
          r.insert(b)
        }
      }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
        // no force
        case Leaf(_, _, _, bodies) =>
          bodies.foreach(body => addForce(body.mass, body.x, body.y))
        // add force contribution of each body by calling addForce
        case Fork(nw, ne, sw, se) =>
          if (quad.size / distance(x, y, quad.massX, quad.massY) < theta) {
            addForce(quad.mass, quad.massX, quad.massY)
          } else {
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          }

        //          addForce(x.mass,x.massX,x.massY)
        // see if node is far enough from the body,
        // or recursion is needed
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix     = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      val x =
        (if (b.x < boundaries.minX) boundaries.minX + minimumSize
        else if (b.x > boundaries.maxX) boundaries.maxX - minimumSize
        else b.x) - boundaries.minX

      val y =
        (if (b.y < boundaries.minY) boundaries.minY + minimumSize
        else if (b.y > boundaries.maxY) boundaries.maxY - minimumSize
        else b.y) - boundaries.minY
      Try{
        apply(((x) / (sectorSize)).toInt, ((y) / (sectorSize)).toInt) += b
      }.getOrElse(
        println(x -> y )
      )
      this
    }

    def apply(x: Int, y: Int): ConcBuffer[Body] = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      val r = new SectorMatrix(boundaries, sectorPrecision)
      for {
        x <- 0 until sectorPrecision
        y <- 0 until sectorPrecision
      } yield {
        this (x, y) combine that(x, y) foreach (body => r(x, y) += body)
      }
      r
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this (x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
            else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
              )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: => T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None               => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString ("\n")
    }
  }

}
