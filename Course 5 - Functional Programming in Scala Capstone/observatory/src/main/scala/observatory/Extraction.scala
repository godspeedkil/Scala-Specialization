package observatory

import java.nio.file.Paths
import java.sql.Date
import java.time.LocalDate

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.{Encoders, SparkSession}
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types.StructType

/**
  *
  * 1st milestone: data extraction
  */
object Extraction {

//  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .master("local[*]")
//    .config("spark.driver.memory", "20g")
//    .config("spark.exectuor.memory", "5g")
      .getOrCreate()

  import spark.implicits._
  import observatory.implicits._

  def resourcesPath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {

    val stationsDf = spark.read
      .schema(Encoders.product[StationsRow].schema)
      .csv(resourcesPath(stationsFile))
      .filter($"latitude".isNotNull && $"longitude".isNotNull)
    val temperaturesDf = spark.read
      .schema(Encoders.product[TemperaturesRow].schema)
      .csv(resourcesPath(temperaturesFile))
      .filter($"temperature".notEqual("9999.9"))

    //    stationsDf.show()
    //    temperaturesDf.show()

    val finalDf = stationsDf.join(temperaturesDf,
      stationsDf("stn") <=> temperaturesDf("stn")  &&
        stationsDf("wban") <=> temperaturesDf("wban"))
      .drop(temperaturesDf("stn"))
      .drop(temperaturesDf("wban"))
      .as[FinalRow]

//    val finalRdd = finalDf.rdd
//      .map(row => (
//        LocalDate.of(year, row.month, row.day),
//        Location(row.latitude, row.longitude),
//        row.temperature.toCelsius
//      ))
//
//    finalRdd.collect()

//    finalDf.show()

    finalDf.collect()
      .seq
      .map(row => (
      LocalDate.of(year, row.month, row.day),
      Location(row.latitude, row.longitude),
      row.temperature.toCelsius
    ))
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    spark.sparkContext
      .parallelize(records.toSeq)
      .map(row => (row._1.getYear, row._2, row._3))
      .toDF("year", "location", "temperature")
      .groupBy($"year", $"location")
      .agg($"year", $"location", avg($"temperature").as("temperature"))
      .select($"location".as[Location], $"temperature".as[Double])
      .collect()
  }

  case class StationsRow(
                          stn: String,
                          wban: String,
                          latitude: Double,
                          longitude: Double
                        )

  case class TemperaturesRow(
                              stn: String,
                              wban: String,
                              month: Int,
                              day: Int,
                              temperature: Double
                            )

  case class FinalRow(
                       stn: String,
                       wban: String,
                       latitude: Double,
                       longitude: Double,
                       month: Int,
                       day: Int,
                       temperature: Double
                     )
}