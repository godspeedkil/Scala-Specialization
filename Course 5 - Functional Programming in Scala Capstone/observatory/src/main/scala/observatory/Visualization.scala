package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import javax.tools.DocumentationTool

import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {

    // great circle formula
    // when dealing with Earth, r approximates 6371 km
    def distTo(other: Location): Double = {
      6371 * math.acos(
        math.sin(location.lat.toRadians) * math.sin(other.lat.toRadians)
        + math.cos(location.lat.toRadians) * math.cos(other.lat.toRadians) * math.cos((location.lon - other.lon).toRadians)
      )
    }

    // inverse distance weighing and summing total
    def weightedSum(power: Double, distTemp: Iterable[(Double, Double)]): Double = {
      val weightedPair = distTemp.map(pair => (1.0 / math.pow(pair._1, power), pair._2/math.pow(pair._1, power)))
      val total = weightedPair.foldLeft((0.0, 0.0))((weights, pair) => (weights._1+ pair._1, weights._2 + pair._2))

      total._2 / total._1
    }

    val distTemp = temperatures.map(pair => (distTo(pair._1), pair._2))

    // avoid problems with inverse exponents of numbers under 1
    distTemp.find(_._1 < 1.0) match {
      case Some((_, temperature)) => temperature
      case None => weightedSum(3.0, distTemp) // 3 works well for most others
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    // interpolate color from known color values
    def interpolatedColor(value: Double): Color = {
      // partition numbers < value and number >= value
      val sortedPartition = points.toList.sortWith((pair1, pair2) => pair1._1 < pair2._1)
        .partition(pair => pair._1 < value)

      if (sortedPartition._1.isEmpty) sortedPartition._2.head._2 // value over max color
      else if (sortedPartition._2.isEmpty) sortedPartition._1.last._2 // value under min color
      else {
        val (lowerPair, upperPair) = (sortedPartition._1.last, sortedPartition._2.head) // get two values to
        // interpolate between
        val difference = upperPair._1 - lowerPair._1
        val midDifference = value - lowerPair._1
        val (lowerColor, upperColor) = (lowerPair._2, upperPair._2)

        val red = math.round((upperColor.red - lowerColor.red) * midDifference / difference + lowerColor.red)
        val green = math.round((upperColor.green - lowerColor.green) * midDifference / difference + lowerColor.green)
        val blue = math.round((upperColor.blue - lowerColor.blue) * midDifference / difference + lowerColor.blue)

        Color(red.toInt, green.toInt, blue.toInt)
      }
    }

    points.find(_._1 == value) match {
      case Some((_, color)) => color
      case None => interpolatedColor(value)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val width = 360
    val height = 180

    def indexToLocation(index: Integer): Location = {
      val x = index % width
      val y = index / width

      Location(90.0D - y * 180.0D / height, x * 360.0D / width - 180.0D)
    }

    val pixels = (0 until width * height).par.map(index => {
      val predictedTemp = predictTemperature(temperatures, indexToLocation(index))
      val predictedColor = interpolateColor(colors, predictedTemp)

      Pixel(predictedColor.red, predictedColor.green, predictedColor.blue, 0)
    }).toArray

    Image(width, height, pixels)
  }

}

