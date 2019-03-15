package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    val r1: Double = point.x * d01 + (1 - point.x) * d00
    val r2: Double = point.x * d11 + (1 - point.x) * d10

    point.y * r2 + (1 - point.y) * r1
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {

    val width = 256
    val height = 256

    val pixels = {
      (0 until (width * height))
        .map(index => Interaction.tileLocation(tile))
        .map(location => {
          val x = location.lon - location.lon.floor.toInt
          val y = location.lat.ceil.toInt - location.lat
          val d00 = grid(GridLocation(location.lat.ceil.toInt, location.lon.floor.toInt))
          val d01 = grid(GridLocation(location.lat.floor.toInt, location.lon.floor.toInt))
          val d10 = grid(GridLocation(location.lat.ceil.toInt, location.lon.ceil.toInt))
          val d11 = grid(GridLocation(location.lat.floor.toInt, location.lon.ceil.toInt))
          bilinearInterpolation(CellPoint(x, y), d00, d01, d10, d11)
        })
        .map(temperature => Visualization.interpolateColor(colors, temperature))
        .map(color => Pixel(color.red, color.green, color.blue, 127))
        .toArray
    }

    Image(width, height, pixels)
  }

}
