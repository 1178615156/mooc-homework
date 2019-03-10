package observatory

import com.sksamuel.scrimage.{Image, Pixel, RGBColor}
import Visualization._

import scala.collection.parallel.ParIterable
import scala.collection.parallel.immutable.ParSeq


/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val n = math.pow(2, zoom)
    val lon_deg = x.toDouble / n * 360 - 180
    val lat_rad = math.atan(math.sinh(math.Pi * (1 - 2 * y / n)))
    val lat_deg = math.toDegrees(lat_rad)
    Location(lat = lat_deg, lon = lon_deg)
  }

  def tileCoordinate2location(zoom: Int, x: Int, y: Int, xx: Int, yy: Int) = {
    val start = tileLocation(zoom, x, y)
    val end = tileLocation(zoom, x + 1, y + 1)
    Location(
      lat = start.lat + (yy.toDouble) * (end.lat - start.lat) / 256,
      lon = start.lon + (xx.toDouble) * (end.lon - start.lon) / 256
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param zoom         Zoom level
    * @param x            X coordinate
    * @param y            Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val image = Image(256, 256)
    val start = tileLocation(zoom, x, y)
    val end = tileLocation(zoom, x + 1, y + 1)

    for {
      xx <- (0 until 256).par
      yy <- (0 until 256).par
    } {
      val location = tileCoordinate2location(zoom, x, y, xx, yy)
      val temp = predictTemperature(temperatures, location)
      val color = interpolateColor(colors, temp)

      image.setPixel(xx, yy, RGBColor(color.red, color.green, color.blue, 127).toPixel)
    }
    image
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Int, Data)],
                           generateImage: (Int, Int, Int, Int, Data) => Unit
                         ): Unit = {
    for {
      zoom <- 0 to 3
      x <- 1 to math.pow(2, zoom).toInt map (_ - 1)
      y <- 1 to math.pow(2, zoom).toInt map (_ - 1)
      (year, data) <- yearlyData
    } {
      generateImage(year, zoom, x, y, data)
    }
  }

}
