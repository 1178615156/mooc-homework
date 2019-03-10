package observatory

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, WordSpecLike}
import org.scalatest.junit.JUnitRunner

class ExtractionTest extends WordSpecLike {

  import Extraction._

  "string 2 location" must {
    "007025,,," in {
      assert(string2location("007025,,,") === Station(Some("007025"), None, None, None))
    }
    "007026,,+00.000,+000.000" in {
      val expect = Station(
        stn = Some("007026"),
        wban = None,
        lat = Some(java.lang.Double.valueOf("+00.000")),
        log = Some(java.lang.Double.valueOf("+000.000")))
      assert(string2location("007026,,+00.000,+000.000") === expect)
    }
  }

  "string2temperature" must {
    "35831,,5,13,56.2" in {
      val expect = Temperature(
        Some("35831"), None, 5, 13, 56.2
      )
      assert(string2temperature("35831,,5,13,56.2") === expect)
    }
  }

  def timeOf[T](f: => T) = {
    val startTime = System.currentTimeMillis()
    val result = f
    val endTime = System.currentTimeMillis()
    (endTime - startTime)
  }

  "locateTemperatures" must {
    def runOf(year: Int) = {
      val time = timeOf {
        val list = locateTemperatures(year, "/stations.csv", s"/$year.csv")
        assert(list.nonEmpty)
        println(s"$year size : ${list.size}")
      }
      println(time.toDouble / 1000)
    }

    "1975" in {
      runOf(1975)

    }
    "1975 - 1980" in {
      1975 to 1980 foreach (runOf)
    }
    "2000" in {
      val year = 2000
      val list = locateTemperatures(year, "/stations.csv", s"/$year.csv")
      println(list.filter(_._2 === Location(1.0, -1.0)).toList)
    }
  }

  "station" in {
    val s = loadFile("/stations.csv")
      .map(string2location).toList

    //      .filter(e => e.log.nonEmpty && e.lat.nonEmpty)
    //      .map(e => e.lat.get -> e.log.get)
    //      .toList
    //    println(s.sorted.filter(_._1 >= 1))


    val goal = List(
      "038030",
      "085710",
      "408560",
      "477820",
      "723260",
      "723416",
      "724237")

//    println(
//      goal.map(e => s.filter(_.stn==Some(e)))
//        .mkString("\n")
//    )

    println(
      s"""
        |${s.map(e=>e.stn -> e.wban).size}
        |${s.map(e=>e.stn -> e.wban).distinct.size}
      """.stripMargin)


  }


}












