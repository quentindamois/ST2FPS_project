package services

import models.*
import utils.LibraryError.*
import utils.LibraryError
import java.time.LocalDateTime
import java.util.UUID

/**
 * Service principal pour la gestion de la bibliothèque
 */
class LibraryService(private var catalog: Catalog = Catalog.empty) {
  type LibraryResult[A] = Either[LibraryError, A]
  
  // Gestion des livres
  def addBook(book: Book): LibraryResult[Unit] = {
    if (catalog.getBook(book.id).isDefined) {
      Left(LibraryError.BookAlreadyExists(book.id))
    } else {
      catalog = catalog.addBook(book)
      Right(())
    }
  }
  
  def searchBooks(query: String, searchType: SearchType): LibraryResult[List[Book]] = {
    val results = searchType match {
      case SearchType.Title => catalog.findBooksByTitle(query)
      case SearchType.Author => catalog.findBooksByAuthor(query)
      case SearchType.Genre => catalog.findBooksByGenre(query)
    }
    Right(results)
  }
  
  // Gestion des utilisateurs
  def addUser(user: User): LibraryResult[Unit] = {
    if (catalog.getUser(user.id).isDefined) {
      Left(LibraryError.UserAlreadyExists(user.id))
    } else {
      catalog = catalog.addUser(user)
      Right(())
    }
  }
  
  // Opérations d'emprunt et de retour
  def borrowBook(userId: String, bookId: String): LibraryResult[Transaction] = {
    for {
      user <- catalog.getUser(userId).toRight(LibraryError.UserNotFound(userId))
      book <- catalog.getBook(bookId).toRight(LibraryError.BookNotFound(bookId))
      _ <- Either.cond(book.isAvailable, (), LibraryError.BookNotAvailable(bookId))
      _ <- Either.cond(user.canBorrow, (), LibraryError.BorrowLimitExceeded(userId))
      updatedUser <- user.borrowBook(bookId).left.map(LibraryError.ValidationError.apply)
      updatedBook <- book.borrowCopy.left.map(LibraryError.ValidationError.apply)
      transaction = Transaction.createBorrow(UUID.randomUUID().toString, userId, bookId)
    } yield {
      catalog = catalog
        .addUser(updatedUser)
        .addBook(updatedBook)
        .addTransaction(transaction)
      transaction
    }
  }
  
  def returnBook(userId: String, bookId: String): LibraryResult[Transaction] = {
    for {
      user <- catalog.getUser(userId).toRight(LibraryError.UserNotFound(userId))
      book <- catalog.getBook(bookId).toRight(LibraryError.BookNotFound(bookId))
      _ <- Either.cond(user.borrowedBooks.contains(bookId), (), LibraryError.BookNotBorrowedByUser(userId, bookId))
      borrowTransaction <- catalog.getTransactionsByUser(userId)
        .find(t => t.bookId == bookId && t.transactionType == TransactionType.Borrow && t.returnDate.isEmpty)
        .toRight(LibraryError.BorrowTransactionNotFound(userId, bookId))
      fine = borrowTransaction.calculateFine()
      updatedUser <- user.returnBook(bookId).left.map(LibraryError.ValidationError.apply)
      updatedBook <- book.returnCopy.left.map(LibraryError.ValidationError.apply)
      transaction = Transaction.createReturn(UUID.randomUUID().toString, userId, bookId, 
        if (fine > 0) Some(fine) else None)
    } yield {
      catalog = catalog
        .addUser(updatedUser)
        .addBook(updatedBook)
        .addTransaction(transaction)
      transaction
    }
  }
  
  // Consultation
  def getCatalog: Catalog = catalog
  
  def getUserBorrowedBooks(userId: String): LibraryResult[List[Book]] = {
    for {
      user <- catalog.getUser(userId).toRight(LibraryError.UserNotFound(userId))
      books = user.borrowedBooks.flatMap(catalog.getBook)
    } yield books
  }
  
  def getOverdueBooks: LibraryResult[List[(User, Book, Transaction)]] = {
    val overdueTransactions = catalog.overdueTransactions
    val result = overdueTransactions.flatMap { transaction =>
      for {
        user <- catalog.getUser(transaction.userId)
        book <- catalog.getBook(transaction.bookId)
      } yield (user, book, transaction)
    }
    Right(result)
  }
}

enum SearchType {
  case Title, Author, Genre
}
