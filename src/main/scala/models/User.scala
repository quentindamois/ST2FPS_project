package models

import java.time.LocalDate
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*

/**
 * Représente un utilisateur de la bibliothèque
 */
case class User(
  id: String,
  firstName: String,
  lastName: String,
  email: String,
  membershipDate: LocalDate,
  userType: UserType,
  borrowedBooks: List[String] = List.empty,
  maxBorrowLimit: Int = 5
) {
  def fullName: String = s"$firstName $lastName"
  
  def canBorrow: Boolean = borrowedBooks.length < maxBorrowLimit
  
  def borrowBook(bookId: String): Either[String, User] = {
    if (canBorrow && !borrowedBooks.contains(bookId)) {
      Right(this.copy(borrowedBooks = bookId :: borrowedBooks))
    } else if (borrowedBooks.contains(bookId)) {
      Left("Ce livre est déjà emprunté par cet utilisateur")
    } else {
      Left("Limite d'emprunt atteinte")
    }
  }
  
  def returnBook(bookId: String): Either[String, User] = {
    if (borrowedBooks.contains(bookId)) {
      Right(this.copy(borrowedBooks = borrowedBooks.filterNot(_ == bookId)))
    } else {
      Left("Ce livre n'est pas emprunté par cet utilisateur")
    }
  }
}

enum UserType {
  case Student, Faculty, External
}

object User {
  given Encoder[User] = deriveEncoder
  given Decoder[User] = deriveDecoder
}

object UserType {
  given Encoder[UserType] = deriveEncoder
  given Decoder[UserType] = deriveDecoder
}
