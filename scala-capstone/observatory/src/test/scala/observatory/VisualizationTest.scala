package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import Visualization._
import com.sksamuel.scrimage.Image
import org.scalacheck.Prop
import org.scalacheck.Test.Parameters
import org.scalatest.prop.Checkers._

import scala.util.Random

class VisualizationTest extends FunSuite with Checkers {

  implicit class With_~==(double: Double) {
    def ~==(other: Double): Boolean =
      math.abs(1 - double / other) < 0.001
  }

  test("greatCircleDistance") {
    val l = Location(40.35, 74.65)
    val r = Location(48.87, -2.33)
    assert(greatCircleDistance(l, r) === 3185.1779271158425)
    assert(greatCircleDistance(Location(0,180),Location(0,-180)) ===0)
  }

  test("predictTemperature") {
    val temps = List(
      Location(1, 1) -> 1.0,
      Location(2, 2) -> 2.0
    )

    assert(predictTemperature(temps, Location(1.5, 1.5)) ~== 1.5)
    assert(predictTemperature(temps, Location(1, 2)) ~== 1.5)

  }

  test("linearInterpolationColor") {
    val points = Seq(
      60.0 -> Color(255, 255, 255),
      32.0 -> Color(255, 0, 0),
      12.0 -> Color(255, 255, 0),
      0.0 -> Color(255, 0, 255),
      -15.0 -> Color(0, 0, 255),
      -27.0 -> Color(255, 0, 255),
      -50.0 -> Color(33, 0, 107),
      -60.0 -> Color(0, 0, 0)
    )
    assert(interpolateColor(points, 60) === Color(255, 255, 255))

    33 to 59 foreach { temp =>
      val color = interpolateColor(points, temp)
      assert(color.red === 255)
      assert(color.green > 0)
      assert(color.blue > 0)
    }
    13 to 31 foreach { temp =>
      val color = interpolateColor(points, temp)
      assert(color.red === 255, s"temp as :${temp} color is ${color}")
      assert(color.green < 255, s"temp as :${temp} color is ${color}")
      assert(color.blue === 0, s"temp as :${temp} color is ${color}")
    }

    -14 to -1 foreach { temp =>
      val color = interpolateColor(points, temp)
      assert(color.red < 255, s"temp as :${temp} color is ${color}")
      assert(color.green === 0, s"temp as :${temp} color is ${color}")
      assert(color.blue === 255, s"temp as :${temp} color is ${color}")
    }

    val random = new Random(100)

    1 to 1000 map (_ => random.nextDouble() * 100) foreach { n =>
      (n -> interpolateColor(points, n))
    }
    1 to 1000 map (_ => -1 * random.nextDouble() * 100) foreach { n =>
      (n -> interpolateColor(points, n))
    }
  }

  test("image") {
    val image = Image(360, 180)
    image.setPixel(100, 100, com.sksamuel.scrimage.Color.White.toPixel)
    image.output("D://image.png")
  }

  test("visualize") {
    val averageTemp = Extraction.locationYearlyAverageRecords(
      Extraction.locateTemperatures(2000, "/stations.csv", "/2000.csv") //.take(100)
    ).par
    val points = Seq(
      60.0 -> Color(255, 255, 255),
      32.0 -> Color(255, 0, 0),
      12.0 -> Color(255, 255, 0),
      0.0 -> Color(0, 255, 255),
      -15.0 -> Color(0, 0, 255),
      -27.0 -> Color(255, 0, 255),
      -50.0 -> Color(33, 0, 107),
      -60.0 -> Color(0, 0, 0)
    )

    visualize(averageTemp,points).output("D://visualize.png")
  }
}
