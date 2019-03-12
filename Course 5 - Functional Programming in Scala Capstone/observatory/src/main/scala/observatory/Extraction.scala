package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._

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
      .master("local")
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
      .option("header", value = false)
      .option("mode", "FAILFAST")
      .schema(Station.structType)
      .csv(resourcesPath(stationsFile))
      .as[Station]
      .filter((station: Station) => station.latitude.isDefined && station.longitude.isDefined)
    val temperaturesDf = spark.read
      .option("header", value = false)
      .option("mode", "FAILFAST")
      .schema(Record.structType)
      .csv(resourcesPath(temperaturesFile))
      .as[Record]
      .filter((record: Record) => record.temperature != 9999.9)

//    stationsDs.show()
//    temperaturesDs.show()

//    val stationsSchema = StructType(
//      List(
//      StructField("STN identifier", StringType, nullable = false),
//      StructField("WBAN identifier", StringType, nullable = true),
//      StructField("Latitude", DoubleType, nullable = false),
//      StructField("Longitude", DoubleType, nullable = false)
//      )
//    )
//
//    val temperaturesSchema = StructType(
//      List(
//        StructField("STN identifier", StringType, nullable = false),
//        StructField("WBAN identifier", StringType, nullable = true),
//        StructField("Month", IntegerType, nullable = false),
//        StructField("Day", IntegerType, nullable = false),
//        StructField("Temperature", DoubleType, nullable = false)
//      )
//    )

//    val stationsRdd = spark.sparkContext.textFile(stationsFile)
//    val temperaturesRdd = spark.sparkContext.textFile(temperaturesFile)

    stationsDf.join(temperaturesDf,
      stationsDf("stn") <=> temperaturesDf("stn") &&
      stationsDf("wban") <=> temperaturesDf("wban"))
      .drop(temperaturesDf("stn"))
      .drop(temperaturesDf("wban"))
      .as[FinalRow]
      .map(row => (
        LocalDate.of(year, row.month, row.day),
        Location(row.latitude, row.longitude),
        row.temperature.toCelsius
      ))
      .collect()
//      .seq
//      .map(row => (
//        LocalDate.of(year, row.getAs[Int]("month"), row.getAs[Int]("day")),
//        Location(row.getAs[Double]("latitude"), row.getAs[Double]("longitude")),
//        row.getAs[Double]("temperature").toCelsius
//      ))

//    finalDf.show(100)
//
//    finalDf.map(row => (
//      LocalDate.of(year, row.month, row.day),
//      Location(row.latitude, row.longitude),
//      (row.temperature - 32) / (9 / 5)
//      ))
//      .collect()
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
      .agg($"year", $"lock", avg($"temperature").as("temperature"))
      .select($"location".as[Location], $"temperature".as[Double])
      .collect()
      .seq
  }

//  case class StationsRow(
//                        stn: String,
//                        wban: String,
//                        latitude: Double,
//                        longitude: Double
//                        )
//
//  case class TemperaturesRow(
//                            stn: String,
//                            wban: String,
//                            month: Int,
//                            day: Int,
//                            temperature: Double
//                            )

  case class FinalRow(
                     stn: String,
                     wban: String,
                     latitude: Double,
                     longitude: Double,
                     month: Int,
                     day: Int,
                     temperature: Double
                     )
//
//  case class ResultRow(
//                      date: LocalDate,
//                      location: Location,
//                      temperature: Temperature
//                      )
}
