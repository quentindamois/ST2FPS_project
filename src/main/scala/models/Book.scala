package models

import java.time.LocalDate
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*
import upickle.default._
import utils.JsonUtil._
import utils.JsonUtil.ReaderWriterLocalDate

/**
 * Représente un livre dans la bibliothèque
 */
case class Book(
  id: String,
  title: String,
  author: String,
  isbn: String,
  publishedDate: LocalDate,
  genre: String,
  totalCopies: Int,
  availableCopies: Int,
  description: Option[String] = None
) derives ReadWriter {
  def isAvailable: Boolean = availableCopies > 0
  
  def borrowCopy: Either[String, Book] = {
    if (availableCopies > 0) {
      Right(this.copy(availableCopies = availableCopies - 1))
    } else {
      Left("Aucun exemplaire disponible")
    }
  }
  
  def returnCopy: Either[String, Book] = {
    if (availableCopies < totalCopies) {
      Right(this.copy(availableCopies = availableCopies + 1))
    } else {
      Left("Tous les exemplaires sont déjà retournés")
    }
  }
}

object Book {
  given Encoder[Book] = deriveEncoder
  given Decoder[Book] = deriveDecoder
}
