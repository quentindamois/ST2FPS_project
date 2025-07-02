package utils

import io.circe.*
import io.circe.syntax.*
import io.circe.parser.*

object JsonUtil {
  def toJson[T: Encoder](value: T): String = value.asJson.spaces2

  def fromJson[T: Decoder](json: String): Either[String, T] =
    decode[T](json).left.map(_.getMessage)
}
