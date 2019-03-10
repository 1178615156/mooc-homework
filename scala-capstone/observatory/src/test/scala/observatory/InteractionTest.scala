package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

//@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  import Interaction._

  test("tileLocation") {
    assert(tileLocation(1, 0, 0) === Location(85.05112877980659, -180.0))
    println(tileLocation(1, 0, 1))
    println(tileLocation(1, 1, 0))
    println(tileLocation(1, 1, 1))
    assert(tileLocation(1, 2, 2) === Location(-85.05112877980659, 180.0))
  }

  test("interaction") {
    val averageTemp = Extraction.locationYearlyAverageRecords(
      Extraction.locateTemperatures(2000, "/stations.csv", "/2000.csv") //.take(100)
    )
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
    tile(averageTemp, points, 0, 0, 0).output("D://interaction.png")
  }

  test("tile") {
    val temps = List(
      (Location(45.0, -90.0), 20.0),
      (Location(45.0, 90.0), 0.0),
      (Location(0.0, 0.0), 10.0),
      (Location(-45.0, -90.0), 0.0),
      (Location(-45.0, 90.0), 20.0)
    )
    val colors = List(
      (10.0, Color(255, 0, 0)), //r
      (20.0, Color(0, 0, 255)) //b
    )

    tile(temps, colors, 0, 0, 0).output("D://tile.png")
  }
}
