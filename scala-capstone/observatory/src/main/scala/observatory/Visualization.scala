package observatory

import java.lang.Math.{cos, sin, toRadians}

import com.sksamuel.scrimage.{Image, Pixel, RGBColor}

import scala.collection.{GenIterable, IterableLike}
import scala.collection.parallel.ParIterable
import scala.math.BigDecimal
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  def greatCircleDistance(l: Location, r: Location): Double = {
    val angle = Math.acos(
      sin(toRadians(l.lat)) * sin(toRadians(r.lat)) +
        cos(toRadians(l.lat)) * cos(toRadians(r.lat)) * cos(toRadians(l.lon - r.lon))
    )
    Math.toDegrees(angle) * 60
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: GenIterable[(Location, Double)], location: Location): Double = {
    val distance_temp = temperatures.map {
      case (locationPoint, temp) => greatCircleDistance(locationPoint, location) -> temp
    }

    def average(seq: GenIterable[Double]): Double = {
      val (s, l) = seq.foldLeft((0.0, 0))((t, r) => (t._1 + r, t._2 + 1))
      s / l
    }

    if(distance_temp.exists(_._1 == 0))
      average(distance_temp.filter(_._1 == 0).map(_._2))
    else {
      val timeTemp = distance_temp map { case (distance, temp) => temp / (math.pow(distance, 1)) }
      val distance = distance_temp map { case (distance, temp) => 1.0 / (math.pow(distance, 1)) }
      timeTemp.sum / distance.sum
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {

    val (maxTemp, maxColor) = points.maxBy(_._1)
    val (minTemp, minColor) = points.minBy(_._1)

    if(value >= maxTemp) maxColor
    else if(value <= minTemp) minColor
    else if(points.size == 1) points.head._2
    else {

      def linearInterpolationColor(a: Double, up: Color, down: Color) =
        Color(
          red = BigDecimal((a * (up.red - down.red)) + down.red).setScale(0, RoundingMode.HALF_UP).toInt,
          green = BigDecimal((a * (up.green - down.green)) + down.green).setScale(0, RoundingMode.HALF_UP).toInt,
          blue = BigDecimal((a * (up.blue - down.blue)) + down.blue).setScale(0, RoundingMode.HALF_UP).toInt
        )

      def rand(seq: List[(Double, Color)]): Color = {
        seq match {
          case (downTemp, downColor) :: (upTemp, upColor) :: other if downTemp <= value && value <= upTemp =>
            val a = (value - downTemp) / (upTemp - downTemp)
            linearInterpolationColor(a, upColor, downColor)
          case _ :: other                                                                                  =>
            rand(other)
        }
      }

      rand(points.toList.sortBy(_._1))
    }

  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: GenIterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val image = Image(360, 180)
    for {
      x <- (0 until 360).par
      y <- (0 until 180).par
    } {
      val location = Location(90 - y, x - 180)
      val temp = predictTemperature(temperatures, location)
      val color = interpolateColor(colors, temp)
      image.setPixel(x, y, RGBColor(color.red, color.green, color.blue).toPixel)
    }

    image
  }

}

