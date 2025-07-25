package models

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*
import upickle.default.*
import utils.Id
import utils.JsonUtil._
import utils.CustomTypes._


/**
 * The catalog of the Library
 *
 * @constructor Create a LibCatalog with Map of Books, a Map of Users and a List of Transaction
 * @param books A LibraryMapIdTo[Book] that contain the Id of the book and the Book Object
 * @param users A LibraryMapIdTo[User] that contain the Id of the user and the User Object
 * @param transactions A List[Transaction] that contain all fo the transaction done by users with the books.
 */
case class LibCatalog (
  books: LibraryMapIdTo[Book] = Map.empty,
  users: LibraryMapIdTo[User] = Map.empty,
  transactions: List[Transaction] = List.empty
) derives ReadWriter {
  /**
    * Add a Book object to the field books.
    *
    * @param book a Book object added to the field books
    * @return a New LibCatalog with the new Book object to the field books
    */
  def addBook(book: Book): LibCatalog = {
    this.copy(books = books + (book.id -> book))
  }
  /**
    * Remove a Book Object from the field Map Books
    *
    * @param bookId an Id corresponding to the Book we are removing
    * @return an object LibCatalog with the Book removed from the field books
    */
  def removeBook(bookId: Id): LibCatalog = {
    this.copy(books = books - bookId)
  }
  /**
    * Get the Book object that has a given Id.
    *
    * @param bookId an Id corresponding to the Book object we want to get
    * @return an object Book with a bookId equal to the Id given as an Input
    */
  def getBook(bookId: Id): Option[Book] = books.get(bookId)
  /**
    * Filter the Map Books and return a list containing every BooK inside the Map whose title contain the title given as an input
    *
    * @param title a String used to filter the objects Book
    * @return A List[Book] containing the all the Books object that contain the title inside their field title
    */
  def findBooksByTitle(title: String): List[Book] = {
    books.values.filter(_.title.toLowerCase.contains(title.toLowerCase)).toList
  }
  /**
    * Filter the Map Books and return a list containing every Book inside the Map who have at least one author in common with the 
    *
    * @param author a String used to filter the objects Book
    * @return A List[Book] containing the all the Books object that contain the name of the author inside their field author
    */
  def findBooksByAuthor(author: String): List[Book] = {
    books.values.filter(_.author.map(oneAuthor => author.toLowerCase.toLowerCase.contains(oneAuthor.toLowerCase)).reduce(_ | _)).toList
  }
  /**
    * Filter the Map Books and return a list containing every Book inside the Map whose field genre contain the genre given as an input
    *
    * @param genre a String used to filter the objects Book
    * @return A List[Book] containing the all the Books object that contain genre inside their field genre
    */
  def findBooksByGenre(genre: String): List[Book] = {
    books.values.filter(_.genre.toLowerCase.contains(genre.toLowerCase)).toList
  }
  
  // Operation on the User
  /**
    * Add an object User to the field users
    *
    * @param user a User object added to the Map users
    * @return a New LibCatalog with the new Users object to the field users
    */
  def addUser(user: User): LibCatalog = {
    this.copy(users = users + (user.id -> user))
  }

  /**
   * Remove a User Object from the field Map Books
   *
   * @param userId an Id corresponding to the User we are removing
   * @return an object LibCatalog with the User removed from the field users
   */
  def removeUser(userId: Id): LibCatalog = {
    this.copy(users = users - userId)
  }
  /**
   * Get the User object that has a given Id.
   *
   * @param userId an Id corresponding to the User object we want to get
   * @return an object User with a userId equal to the Id given as an Input
   */
  def getUser(userId: Id): Option[User] = users.get(userId)
  
  // Operation on the transaction
  /**
    * Add a Transaction object to the field transactions.
    *
    * @param transaction a Transaction object added to the field transactions
    * @return a New LibCatalog with the new Transaction object to the field transactions
    */
  def addTransaction(transaction: Transaction): LibCatalog = {
    this.copy(transactions = transaction :: transactions)
  }

  /**
   * Get all the Transaction Object done by one user.
   *
   * @param userId an Id corresponding to the user borrowing the books.
   * @return A List[Transaction] containing all the Transaction done by the user
   */
  def getTransactionsByUser(userId: Id): List[Transaction] = {
    transactions.filter(_.userId == userId)
  }
  /**
    * Get all the Transaction Object done with one book.
    *
    * @param bookId an Id corresponding to book we are using to filter the List
    * @return A List[Transaction] containing all the Transaction done by the user
    */
  def getTransactionsByBook(bookId: Id): List[Transaction] = {
    transactions.filter(_.bookId == bookId)
  }
  
  // Statistic
  /**
    * Give the number of book in the fields books
    *
    * @return An Int corresponding to the size of the field books.
    */
  def totalBooks: Int = books.size
  /**
    * Give the number of user in the fields users
    *
    * @return An Int corresponding to the size of the field users.
    */
  def totalUsers: Int = users.size
  /**
   * Give the number of user in the fields transactions
   *
   * @return An Int corresponding to the size of the field transactions.
   */
  def totalTransactions: Int = transactions.size
  /**
   * Give the number of user in the fields transactions
   *
   * @return An Int corresponding to the size of the field transactions.
   */
  def availableBooks: List[Book] = books.values.filter(_.isAvailable).toList
  /**
    * Give the List of Books that are borrowed.
    *
    * @return A List of Book that are not Available
    */
  def borrowedBooks: List[Book] = books.values.filter(!_.isAvailable).toList
  /**
    * Give the list of transaction that are overdue
    *
    * @return A List of Transaction that are overdue
    */
  def overdueTransactions: List[Transaction] = {
    transactions.filter(_.isOverdue)
  }
}

object Catalog {
  /**
   * Create an empty LibCatalog.
   * 
   * @return a empty object LibCatalog
   * */
  def empty: LibCatalog = LibCatalog()
}
