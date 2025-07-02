package models

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._

/**
 * Représente un livre dans la bibliothèque
 */
case class Book(
    id: String,
    isbn: String,
    title: String,
    authors: List[String],
    publishYear: Int,
    genre: String,
    availabilityStatus: Int // ou availableCopies
) {
    def isAvailable: Boolean = availabilityStatus > 0
    
    def borrowCopy: Either[String, Book] = {
        if (availabilityStatus > 0) {
            Right(this.copy(availabilityStatus = availabilityStatus - 1))
            } else {
                Left("Aucun exemplaire disponible")
            }
        }
    }

object Book {
  given Encoder[Book] = deriveEncoder
  given Decoder[Book] = deriveDecoder
}
