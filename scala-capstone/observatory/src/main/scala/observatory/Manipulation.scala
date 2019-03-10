package observatory

import Utils._

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    import Visualization.predictTemperature
    val gridTemps = (for {
      lat <- -90 to 90
      lon <- -180 to 180
    } yield {
      (lat, lon) -> predictTemperature(temperatures, Location(lat, lon))
    }).toMap

    (lat: Int, lon: Int) => gridTemps(lat -> lon)
  }

  /**
    * @param temperaturess Sequence of known temperatures
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    makeGrid(temperaturess.flatten)
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A sequence of grids containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    val tempGrid = makeGrid(temperatures)

    (lat, lon) => {
      val temp = tempGrid(lat, lon)
      val normal = normals(lat, lon)
      temp - normal
    }
  }


}

