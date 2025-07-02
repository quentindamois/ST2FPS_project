package utils

import io.circe.*
import io.circe.syntax.*
import io.circe.parser.*
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object JsonUtil {
  def toJson[T: Encoder](value: T): String = value.asJson.spaces2

  def fromJson[T: Decoder](json: String): Either[String, T] =
    decode[T](json).left.map(_.getMessage)

  def saveToFile[T: Encoder](
      value: T,
      filePath: String
  ): Either[String, Unit] = {
    try {
      val json = toJson(value)
      val path = Paths.get(filePath)
      Files.createDirectories(path.getParent)
      Files.write(path, json.getBytes(StandardCharsets.UTF_8))
      Right(())
    } catch {
      case ex: Exception =>
        Left(s"Erreur lors de la sauvegarde: ${ex.getMessage}")
    }
  }

  def loadFromFile[T: Decoder](filePath: String): Either[String, T] = {
    try {
      val content =
        Files.readString(Paths.get(filePath), StandardCharsets.UTF_8)
      fromJson[T](content)
    } catch {
      case ex: Exception => Left(s"Erreur lors du chargement: ${ex.getMessage}")
    }
  }
}
