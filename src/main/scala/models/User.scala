package models

import java.time.LocalDate
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*
import upickle.default._
import utils.JsonUtil.ReaderWriterLocalDate
import utils.JsonUtil.ReaderWriterID
import utils.CustomTypes._
import utils.Id

/**
 * Représente un utilisateur de la bibliothèque
 */
case class User(
  id: Id,
  firstName: String,
  lastName: String,
  email: String,
  membershipDate: LocalDate,
  userType: UserType,
  borrowedBooks: List[Id] = List.empty,
  maxBorrowLimit: Int = 5
) derives ReadWriter {
  def fullName: String = s"$firstName $lastName"
  
  def canBorrow: Boolean = borrowedBooks.length < maxBorrowLimit
  
  def borrowBook(bookId: Id): Either[String, User] = {
    if (canBorrow && !borrowedBooks.contains(bookId)) {
      Right(this.copy(borrowedBooks = bookId :: borrowedBooks))
    } else if (borrowedBooks.contains(bookId)) {
      Left("Ce livre est déjà emprunté par cet utilisateur")
    } else {
      Left("Limite d'emprunt atteinte")
    }
  }
  
  def returnBook(bookId: Id): Either[String, User] = {
    if (borrowedBooks.contains(bookId)) {
      Right(this.copy(borrowedBooks = borrowedBooks.filterNot(_ == bookId)))
    } else {
      Left("Ce livre n'est pas emprunté par cet utilisateur")
    }
  }
}

enum UserType derives ReadWriter {
  case Student, Faculty, External
}

