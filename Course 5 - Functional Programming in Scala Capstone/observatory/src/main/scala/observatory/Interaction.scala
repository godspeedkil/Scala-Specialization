package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val n = 1 << tile.zoom
    val lat = atan(sinh(Pi * (1 - 2 * tile.y / n))) * 180.0D / Pi
    val lon = tile.x / n * 360.0D - 180.0D

    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val width = 256
    val height = 256

    val pixels = (0 until width * height).par.map(index => {
      val (pixelX, pixelY) = (
        ((index % width).toDouble / width + tile.x).toInt,
        ((index / width).toDouble / height + tile.y).toInt
      )
      val location = tileLocation(Tile(pixelX, pixelY, tile.zoom))
      val predictedTemp = Visualization.predictTemperature(temperatures, location)
      val predictedColor = Visualization.interpolateColor(colors, predictedTemp)

      Pixel(predictedColor.red, predictedColor.green, predictedColor.blue, 127)
    }).toArray

    Image(width, height, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      x <- 0 until 1 << zoom
      y <- 0 until 1 << zoom
    } generateImage(year, Tile(x, y, zoom), data)
  }

}
