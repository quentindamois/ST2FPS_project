package models

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*
import upickle.default.*
import utils.Id


/**
 * Représente le catalogue de la bibliothèque
 */
case class LibCatalog (
  books: Map[Id, Book] = Map.empty,
  users: Map[Id, User] = Map.empty,
  transactions: List[Transaction] = List.empty
) derives ReadWriter {
  // Opérations sur les livres
  def addBook(book: Book): LibCatalog = {
    this.copy(books = books + (book.id -> book))
  }
  
  def removeBook(bookId: Id): LibCatalog = {
    this.copy(books = books - bookId)
  }
  
  def getBook(bookId: Id): Option[Book] = books.get(bookId)
  
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
  
  def removeUser(userId: Id): LibCatalog = {
    this.copy(users = users - userId)
  }
  
  def getUser(userId: Id): Option[User] = users.get(userId)
  
  // Opérations sur les transactions
  def addTransaction(transaction: Transaction): LibCatalog = {
    this.copy(transactions = transaction :: transactions)
  }
  
  def getTransactionsByUser(userId: Id): List[Transaction] = {
    transactions.filter(_.userId == userId)
  }
  
  def getTransactionsByBook(bookId: Id): List[Transaction] = {
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
  def empty: LibCatalog = LibCatalog()
}
