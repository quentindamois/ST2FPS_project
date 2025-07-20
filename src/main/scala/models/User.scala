package models

import java.time.LocalDate
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*
import upickle.default.*
import utils.JsonUtil.ReaderWriterLocalDate
import utils.JsonUtil.ReaderWriterID
import utils.CustomTypes.*
import utils.Id

import scala.annotation.targetName

/**
 * This class represent the user borrowing and returning book
 *
 * @constructor create new User Object
 * @param id an Id corresponding to the id of the user
 * @param firstName a String corresponding to the firstname of the user
 * @param lastName a String corresponding to the lastname of the user
 * @param email a String corresponding to the email of the user
 * @param membershipDate a LocalDate corresponding to the date of creation of the user
 * @param userType a UserType indicating ig the
 * @param borrowedBooks a List[Id] corresponding to the list of id of the Book the user has borrowed
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
)  derives ReadWriter {
  /**
   * Give the fullname of the user
   * 
   * @return A String corresponding to the full name of the user
   * */
  def fullName: String = s"$firstName $lastName"
  /**
   * Check if the user has not reached the maximum number of book he can borrow
   * 
   * @return a Boolean, true if the user can borrow book and false he cannot
   * */
  def canBorrow: Boolean = borrowedBooks.length < maxBorrowLimit
  /**
   * Update the object user to add a new id of Book to the list of book borrowed
   * 
   * @param bookId a Id corresponding to the book that is being borrowed
   * @return an updated object User with the id of the book added the field borrowedBooks
   * */
  def borrowBook(bookId: Id): Either[String, User] = {
    if (canBorrow && !borrowedBooks.contains(bookId)) {
      Right(this.copy(borrowedBooks = bookId :: borrowedBooks))
    } else if (borrowedBooks.contains(bookId)) {
      Left("Ce livre est déjà emprunté par cet utilisateur")
    } else {
      Left("Limite d'emprunt atteinte")
    }
  }
  /**
   * Update the object user to remove the id of Book in the list of book borrowed
   * 
   * @param bookId a Id corresponding to the book that is being returned
   * @return an updated object User with the id of the book removed from the field borrowedBooks
   * */
  def returnBook(bookId: Id): Either[String, User] = {
    if (borrowedBooks.contains(bookId)) {
      Right(this.copy(borrowedBooks = borrowedBooks.filterNot(_ == bookId)))
    } else {
      Left("Ce livre n'est pas emprunté par cet utilisateur")
    }
  }
}


/**
 * An enum used to indicate if the user is a Student, a faculty member or an from outside the university
 * */
enum UserType derives ReadWriter {
  case Student, Faculty, External
}

