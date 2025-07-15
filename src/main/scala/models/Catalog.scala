package models

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*
import upickle.default.*

/**
 * Représente le catalogue de la bibliothèque
 */
case class LibCatalog (
  books: Map[String, Book] = Map.empty,
  users: Map[String, User] = Map.empty,
  transactions: List[Transaction] = List.empty
) derives ReadWriter {
  // Opérations sur les livres
  def addBook(book: Book): LibCatalog = {
    this.copy(books = books + (book.id -> book))
  }
  
  def removeBook(bookId: String): LibCatalog = {
    this.copy(books = books - bookId)
  }
  
  def getBook(bookId: String): Option[Book] = books.get(bookId)
  
  def findBooksByTitle(title: String): List[Book] = {
    books.values.filter(_.title.toLowerCase.contains(title.toLowerCase)).toList
  }
  
  def findBooksByAuthor(author: String): List[Book] = {
    books.values.filter(_.author.toLowerCase.contains(author.toLowerCase)).toList
  }
  
  def findBooksByGenre(genre: String): List[Book] = {
    books.values.filter(_.genre.toLowerCase.contains(genre.toLowerCase)).toList
  }
  
  // Opérations sur les utilisateurs
  def addUser(user: User): LibCatalog = {
    this.copy(users = users + (user.id -> user))
  }
  
  def removeUser(userId: String): LibCatalog = {
    this.copy(users = users - userId)
  }
  
  def getUser(userId: String): Option[User] = users.get(userId)
  
  // Opérations sur les transactions
  def addTransaction(transaction: Transaction): LibCatalog = {
    this.copy(transactions = transaction :: transactions)
  }
  
  def getTransactionsByUser(userId: String): List[Transaction] = {
    transactions.filter(_.userId == userId)
  }
  
  def getTransactionsByBook(bookId: String): List[Transaction] = {
    transactions.filter(_.bookId == bookId)
  }
  
  // Statistiques
  def totalBooks: Int = books.size
  def totalUsers: Int = users.size
  def totalTransactions: Int = transactions.size
  
  def availableBooks: List[Book] = books.values.filter(_.isAvailable).toList
  def borrowedBooks: List[Book] = books.values.filter(!_.isAvailable).toList
  
  def overdueTransactions: List[Transaction] = {
    transactions.filter(_.isOverdue)
  }
}

object Catalog {
  given Encoder[LibCatalog] = deriveEncoder
  given Decoder[LibCatalog] = deriveDecoder
  
  def empty: LibCatalog = LibCatalog()
}
