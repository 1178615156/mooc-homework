package observatory

/**
  * 6th (and last) milestone: user interface polishing
  */
object Interaction2 {

  /**
    * @return The available layers of the application
    */
  def availableLayers: Seq[Layer] = {
    val temp_colors = Seq(
      60.0 -> Color(255, 255, 255),
      32.0 -> Color(255, 0, 0),
      12.0 -> Color(255, 255, 0),
      0.0 -> Color(0, 255, 255),
      -15.0 -> Color(0, 0, 255),
      -27.0 -> Color(255, 0, 255),
      -50.0 -> Color(33, 0, 107),
      -60.0 -> Color(0, 0, 0)
    )
    val deviation_colors: Seq[(Double, Color)] = Seq(
      7.0 -> Color(0, 0, 0),
      4.0 -> Color(255, 0, 0),
      2.0 -> Color(255, 255, 0),
      0.0 -> Color(255, 255, 255),
      -2.0 -> Color(0, 255, 255),
      -7.0 -> Color(0, 0, 255)
    )

    val temp_layer = Layer(LayerName.Temperatures, temp_colors, 1975 to 2015)
    val deviation_layer = Layer(LayerName.Deviations, deviation_colors, 1976 to 2015)
    temp_layer :: deviation_layer :: Nil
  }

  /**
    * @param selectedLayer A signal carrying the layer selected by the user
    * @return A signal containing the year bounds corresponding to the selected layer
    */
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range] = {
    Signal(selectedLayer().bounds)
  }

  /**
    * @param selectedLayer The selected layer
    * @param sliderValue   The value of the year slider
    * @return The value of the selected year, so that it never goes out of the layer bounds.
    *         If the value of `sliderValue` is out of the `selectedLayer` bounds,
    *         this method should return the closest value that is included
    *         in the `selectedLayer` bounds.
    */
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Int]): Signal[Int] = {
    Signal {
      val year_bound = yearBounds(selectedLayer)()
      val slider_value = sliderValue()
      if(slider_value <= 0) year_bound.min
      else if(slider_value >= year_bound.size) year_bound.max
      else slider_value

      if(slider_value <= year_bound.min) year_bound.min
      else if(slider_value >= year_bound.max ) year_bound.max
      else slider_value
    }

  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear  The selected year
    * @return The URL pattern to retrieve tiles
    */
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Int]): Signal[String] = {
    Signal(
      s"target/${selectedLayer().layerName.id}/${selectedYear()}"
    )
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear  The selected year
    * @return The caption to show
    */
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Int]): Signal[String] = {
    Signal {
      val name = selectedLayer().layerName.id
      val new_name = name.head.toUpper + name.tail
      val year = selectedYear()
      s"$new_name ($year)"

    }
  }

}

sealed abstract class LayerName(val id: String)

object LayerName {

  case object Temperatures extends LayerName("temperatures")

  case object Deviations extends LayerName("deviations")

}

/**
  * @param layerName  Name of the layer
  * @param colorScale Color scale used by the layer
  * @param bounds     Minimum and maximum year supported by the layer
  */
case class Layer(layerName: LayerName, colorScale: Seq[(Double, Color)], bounds: Range)

