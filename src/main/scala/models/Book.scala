package models

import java.time.LocalDate
import upickle.default._
import utils.JsonUtil._
import utils.JsonUtil.ReaderWriterLocalDate
import utils.JsonUtil.ReaderWriterID
import utils.CustomTypes._
import utils.Id

/**
 * Représente un livre dans la bibliothèque
 */
case class Book(
  id: Id,
  title: String,
  author: String,
  isbn: Id,
  publishedDate: LocalDate,
  genre: String,
  totalCopies: Int,
  availableCopies: Int,
  description: Description = None
) derives ReadWriter {
  def isAvailable: Boolean = availableCopies > 0
  
  def borrowCopy: Result[Book] = {
    if (availableCopies > 0) {
      Right(this.copy(availableCopies = availableCopies - 1))
    } else {
      Left("Aucun exemplaire disponible")
    }
  }
  
  def returnCopy: Result[Book] = {
    if (availableCopies < totalCopies) {
      Right(this.copy(availableCopies = availableCopies + 1))
    } else {
      Left("Tous les exemplaires sont déjà retournés")
    }
  }
}

