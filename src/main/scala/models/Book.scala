package models

import java.time.LocalDate
import upickle.default.*
import utils.JsonUtil.*
import utils.JsonUtil.ReaderWriterLocalDate
import utils.JsonUtil.ReaderWriterID
import utils.CustomTypes.*
import utils.Id
import utils.ValidationUtil.validate
import services.ValidationService.bookCondition

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
  author: List[String], 
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
  /**
   * Return the number of same author
   *
   * @param book a Book we compare with the book
   * @return an Int corresponding to the amount of common author
   * */
  def hasNCommonAuthor(book: Book): Int = this.author.count(book.author.contains(_))
  /**
   * Validate the field of an object Book.
   * */
  def validationBook(): Result[Book] = this.validate(bookCondition)
}


object Book:
  /**
   * A wrapped function for the constructor of Book
   *
   * @param id String the id of the book
   * @param title String the title of the book
   * @param isbn String corresponding to the isbn of the book
   * @param publishedDate a localDate corresponding to the date of pucblication of the book
   * @param genre a string corresponding to the genre of the book
   * @param totalCopies the total number of copies
   * @param description the description of the book
   * @return a Book object
   * */
  def createBook(id: String,
                 title: String,
                 author: List[String],
                 isbn: String,
                 publishedDate: LocalDate,
                 genre: String,
                 totalCopies: Int,
                 description: Description = None): Result[Book] = (Book(Id(id), title, author, Id(isbn), publishedDate, genre, totalCopies, totalCopies).validationBook())


