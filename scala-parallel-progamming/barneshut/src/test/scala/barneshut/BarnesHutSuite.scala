package barneshut

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._
import scala.collection.parallel._
import barneshut.conctrees.ConcBuffer

@RunWith(classOf[JUnitRunner])
class BarnesHutSuite extends FunSuite {

  // test cases for quad tree

  import FloatOps._

  test("empty size") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.size ~= 5)
  }

  test("leaf size") {
    val quad = Empty(2f, 2f, 4)
    assert(quad.size ~= 4)

    quad.insert(new Body(1, 1, 1, 0, 0)) match {
      case x: Leaf =>
        assert(x.size ~= 4)
        x.insert(new Body(3, 3, 3, 0, 0)) match {
          case x: Fork =>
            assert(x.ll.forall(_.size ~= 2))
        }
    }
  }
  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Leaf with 1 body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Fork with 2 empty") {
    val b1 = new Body(mass = 1f, x = 0.5f, y = 0.5f, xspeed = 0f, yspeed = 0f)
    val b3 = new Body(mass = 1f, x = 1.5f, y = 1.5f, xspeed = 0f, yspeed = 0f)
    val nw = Leaf(0.5f, 0.5f, 5f, Seq(b1))
    val ne = Empty(1.5f, 0.5f, 5f)
    val sw = Empty(0.5f, 1.5f, 5f)
    val se = Leaf(1.5f, 1.5f, 5f, Seq(b3))
    val quad = Fork(nw, ne, sw, se)
    println(quad)
    println(quad.massX)
    println(quad.massY)
    assert(quad.mass == 2)
    assert(quad.massX ~= 1)
    assert(quad.massY ~= 1)
  }


  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.total == 1, s"${quad.total} should be 1")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
  }

  test("fork with 4 empty") {
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)
    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 0f, s"${quad.mass} should be 123f")
    assert(quad.total == 0, s"${quad.total} should be 1")
    assert(quad.massX ~= quad.centerX, s"${quad.massX} should be 18f")
    assert(quad.massY ~= quad.centerY, s"${quad.massY} should be 26f")
  }
  test("Leaf.insert(b) should return a new Fork if size > minimumSize") {
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(new Body(1f, 17.5f, 27.5f, 0, 0)))
    quad.insert(new Body(1, 15f, 25f, 0, 0)) match {
      case x@Fork(nw, ne, sw, se) =>
        assert(nw.total === 1)
    }

    quad.insert(new Body(1, 15f, 25f, 0, 0)).insert(new Body(1, 15f, 26f, 0, 0)) match {
      case x@Fork(nw, ne, sw, se) =>
        assert(nw.total === 2)
        assert(nw.asInstanceOf[Fork].total === 2)
    }
  }
  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _                                    =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }
  test("Empty insert") {
    val quad = Empty(2f, 2f, 4f)
    quad.insert(new Body(1, 1, 1, 0, 0)).insert(new Body(1, 3, 3, 0, 0)) match {
      case x: Fork =>
        assert(x.centerX ~= 2)
        assert(x.centerY ~= 2)
        assert(x.mass == 2)
        assert(x.massX ~= 2)
        assert(x.massY ~= 2)
    }
  }


  test("leaf insert 2 ") {
    val quad = Empty(1f, 1f, 2f)
    quad
      .insert(new Body(1, 0.5f, 0.5f, 0, 0))
      .insert(new Body(1, 1.5f, 0.5f, 0, 0))
      //      .insert(new Body(1,0.5f, 1.5f, 0, 0))
      //      .insert(new Body(1,1.5f, 1.5f, 0, 0))
    match {
      case x@Fork(nw, ne, sw, se) =>
        //        assert(
        //          x.ll.forall(_.total > 0)
        //        )
        assert(nw.total > 0)
        assert(ne.total > 0)
        assert(sw.total == 0)
        assert(se.total == 0)
        assert(x.centerX ~= 1)
        assert(x.centerY ~= 1)
        assert(x.massX ~= 1f)
        assert(x.massY ~= 0.5f)
    }
  }
  test("leaf insert 3") {
    val quad = Leaf(2, 2, 4, Seq(new Body(4, 4, 4, 0, 0)))
    quad.insert(new Body(1, 1, 1, 0, 0)) match {
      case x@Fork(nw, ne, sw, se) =>
        assert(nw.total > 0)
    }
  }

  test("leaf insert") {
    val quad = Empty(2f, 2f, 4f)
    quad.insert(new Body(1, 1, 1, 0, 0)) match {
      case x: Leaf =>
        assert(x.bodies.size === 1)
        x.insert(new Body(1, 3, 3, 0, 0)) match {
          case xx@Fork(nw, ne, sw, se) =>
            assert(xx.total === 2)
            assert(nw.asInstanceOf[Leaf].total === 1)
            assert(se.asInstanceOf[Leaf].total === 1)

        }
    }

  }
  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(centerX = 50f, centerY = 60f, size = 5f))

    assert(body.xspeed == 0f)
    assert(body.yspeed == 0f)
  }

  test("Body.updated should take bodies in a Leaf into account") {
    val b1 = new Body(mass = 123f, x = 18f, y = 26f, xspeed = 0f, yspeed = 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  test("body 1") {
    val boundaries = new Boundaries()
    boundaries.minX = 0
    boundaries.minY = 0
    boundaries.maxX = 100
    boundaries.maxY = 100

    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)

    val body1 = new Body(0, 1, 11, 0, 0)
    val body2 = new Body(0, 1, 12, 0, 0)
    val body3 = new Body(0, 1, 13, 0, 0)
    sm += body1
    assert(sm(0, 0).size === 1)
    assert(sm(0, 0).forall(_ === body1))
    sm += body2
    assert(sm(0, 0).size === 2)
    assert(sm(0, 0).exists(_ === body1) && sm(0, 0).exists(_ === body2))
    sm += body3
    assert(sm(0, 1).size === 1)
    assert(sm(0, 1).forall(_ === body3))
  }

  test("body 2") {
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 101
    boundaries.maxY = 101

    val sm = new SectorMatrix(boundaries, 10)

    val body1 = new Body(0, 1, 9, 0, 0)
    val body2 = new Body(0, 1, 10, 0, 0)
    val body3 = new Body(0, 1, 11, 0, 0)
    sm += body1
    assert(sm(0, 0).size === 1)
    assert(sm(0, 0).forall(_ === body1))
    sm += body2
    assert(sm(0, 1).size === 1)
    assert(sm(0, 1).forall(_ === body2))
    sm += body3
    assert(sm(0, 1).size === 2)
    assert(sm(0, 1).exists(_ === body2) && sm(0, 1).exists(_ === body3))
  }
  // test cases for sector matrix
  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 101
    boundaries.maxY = 101

    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)

    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).exists(_ == body)
    assert(res, s"Body not found in the right sector")
  }
  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).exists(_ == body)
    assert(res, s"Body not found in the right sector")
  }
  test(" += 96") {

    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    val body1 = new Body(1, 12, 34, 0, 0)
    val body2 = new Body(1, 23, 45, 0, 0)
    val body3 = new Body(1, 56, 9, 0, 0)
    val body4 = new Body(1, 8, 79, 0, 0)
    val body5 = new Body(1, 5, 99, 0, 0)

    sm += body1
    sm += body2
    sm += body3
    sm += body4
    sm += body5

    assert(sm(0,7).size === 1 && sm(0,7).forall(_ === body5))
  }


  test("comble") {
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 101
    boundaries.maxY = 101
    val sm1 = new SectorMatrix(boundaries, 10)
    val sm2 = new SectorMatrix(boundaries, 10)
    val body1 = new Body(0, 1, 1, 0, 0)
    sm1 += body1
    assert(sm1(0, 0).size === 1 && sm1(0, 0).forall(_ === body1))
    val body2 = new Body(0, 99, 99, 0, 0)
    sm2 += body2
    assert(sm2(9, 9).size === 1 && sm2(9, 9).forall(_ === body2))

    val sm = sm1 combine sm2

    assert(sm(0, 0).size === 1 && sm(0, 0).forall(_ === body1))
    assert(sm(9, 9).size === 1 && sm(9, 9).forall(_ === body2))

    for {
      x <- 0 until 10
      y <- 0 until 10
      if !(x == 0 && y == 0) && !(x == 9 && y == 9)
    } yield {
      println(x -> y)
      assert(sm(x, y).size === 0)
    }

  }
  test("+= 0 10") {
    val boundaries = new Boundaries()
    boundaries.minX = 0
    boundaries.minY = 0
    boundaries.maxX = 100
    boundaries.maxY = 100

    val sm = new SectorMatrix(boundaries, 10)
    ///////////////////////////////////////////////////

    var i = 0
    for {
      x <- 1 to 8
      y <- 1 to 8
    } yield {
      val body = new Body(1, x, y, 0, 0)
      sm += body
      i = i + 1
      assert(sm(0, 0).size === i)
      assert(sm(0, 0).exists(_ === body))
    }
    ///////////////////////////////////////////////////

    i = 0
    for {
      x <- 11 to 19
      y <- 11 to 19
    } yield {
      val body = new Body(1, x, y, 0, 0)
      sm += body
      i = i + 1
      assert(sm(1, 1).size === i)
      assert(sm(1, 1).exists(_ === body))
    }
    ///////////////////////////////////////////////////

    i = 0
    for {
      x <- 1 to 8
      y <- 11 to 19
    } yield {
      val body = new Body(1, x, y, 0, 0)
      sm += body
      i = i + 1
      assert(sm(0, 1).size === i)
      assert(sm(0, 1).exists(_ === body))
    }
    ///////////////////////////////////////////////////
    i = 0
    for {
      x <- 11 to 19
      y <- 1 to 9
    } yield {
      val body = new Body(1, x, y, 0, 0)
      sm += body
      i = i + 1
      assert(sm(1, 0).size === i)
      assert(sm(1, 0).exists(_ === body))
    }
  }

  test("+= body 25,47") {
    val boundaries = new Boundaries()
    boundaries.minX = 0
    boundaries.minY = 0
    boundaries.maxX = 100
    boundaries.maxY = 100
    val sm = new SectorMatrix(boundaries, 10)
    val body = new Body(1, 25, 47, 0.1f, 0.1f)

    sm += body
    assert(sm(2, 4).size == 1 && sm(2, 4).forall(_ === body))
  }
}

object FloatOps {
  private val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }

}

