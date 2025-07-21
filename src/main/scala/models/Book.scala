package models

import java.time.LocalDate
import upickle.default._
import utils.JsonUtil._
import utils.JsonUtil.ReaderWriterLocalDate
import utils.JsonUtil.ReaderWriterID
import utils.CustomTypes._
import utils.Id

/**
 * The class used to represent a book
 * 
 * @constructor create a Book object
 * @param id an Id associated with the book
 * @param title a String corresponding to the title of the book
 * @param author a String corresponding to the name of the author
 * @param isbn an Id corresponding to the ISBN of the book
 * @param publishedDate a LocalDate corresponding to the date of publication
 * @param genre a String corresponding to the genre of the book
 * @param totalCopies an Int corresponding to the number of copies in the library
 * @param availableCopies an Int corresponding to the number of copy available in the Library
 * @param description a Description corresponding to the book's description
 */
case class Book(
  id: Id,
  title: String,
  author: List[String], //TODO: Change to a List[String]
  isbn: Id,
  publishedDate: LocalDate,
  genre: String,
  totalCopies: Int,
  availableCopies: Int,
  description: Description = None
) derives ReadWriter {
  /**
   * Check to see if the book is available
   * 
   * @return a Boolean, true if the Book is available and false if not
   * */
  def isAvailable: Boolean = availableCopies > 0
  /**
   * Update the Book object to remove one available copies
   * 
   * @return A Result[Book] containing on the left a String indicating that there is no book available, on the Right an updated Book
   * */
  def borrowCopy: Result[Book] = {
    if (availableCopies > 0) {
      Right(this.copy(availableCopies = availableCopies - 1))
    } else {
      Left("No copy is available.")
    }
  }
  /**
   * Update the book to add an available copies
   * 
   * @return Result[Book] containing on the left a String indicating that there is all book are available, on the Right an updated Book
   * */
  def returnCopy: Result[Book] = {
    if (availableCopies < totalCopies) {
      Right(this.copy(availableCopies = availableCopies + 1))
    } else {
      Left("All copy have been returned.")
    }
  }
}

