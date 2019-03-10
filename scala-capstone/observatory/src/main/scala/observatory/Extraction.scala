package observatory

import java.time.LocalDate

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {

  case class Station(stn: Option[String], wban: Option[String], lat: Option[Double], log: Option[Double])

  case class Temperature(stn: Option[String], wban: Option[String], month: Int, day: Int, temperature: Double)

  def string2location(s: String): Station = {
    val splitList = (s + ",0").split(',').toList.map(e => if(e.isEmpty) None else Some(e))


    splitList match {
      case stn :: wban :: lat :: log :: _ :: Nil =>
        Station(stn = stn, wban = wban, lat = lat.map(_.toDouble), log = log.map(_.toDouble))
    }
  }

  def string2temperature(s: String): Temperature = {
    val splitList = (s + ",0").split(',').toList.map(e => if(e.isEmpty) None else Some(e))

    splitList match {
      case stn :: wban :: Some(month) :: Some(day) :: Some(temperature) :: _ :: Nil =>
        Temperature(stn = stn, wban = wban, month = month.toInt, day = day.toInt, temperature = temperature.toDouble)
    }
  }

  def loadFile(fileName: String): Iterator[String] =
    Source.fromInputStream(this.getClass.getResourceAsStream(fileName)).getLines()

  private def fahrenheit2celsius(double: Double): Double = {
    5.0 / 9.0 * (double - 32)
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations = loadFile(stationsFile)
      .toSeq
      .map(string2location)
      .filter(e => e.log.nonEmpty && e.lat.nonEmpty)
      .map(e => (e.stn -> e.wban) -> e)
      .toMap
    val temperatures = loadFile(temperaturesFile)
      .toSeq
      .map(string2temperature)
      .filter(_.temperature < 999)
      .map(e => e.copy(temperature = fahrenheit2celsius(e.temperature)))

    temperatures.map { temp =>
      stations.get(temp.stn -> temp.wban).map {
        case Station(_, _, Some(lat), Some(log)) =>
          (LocalDate.of(year, temp.month, temp.day), Location(lat = lat, lon = log), temp.temperature)
      }
    }
      .collect { case Some(e) => e }
  }

  def average(seq: Iterable[Double]): Double = {
    val (s, l) = seq.foldLeft((0.0, 0))((t, r) => (t._1 + r, t._2 + 1))
    s / l
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.groupBy {
      case (date, location, temp) => location -> date.getYear
    }.mapValues(e => average(e.map(_._3)))
  }.map { case ((location, _), temp) => location -> temp }.toSeq

}
