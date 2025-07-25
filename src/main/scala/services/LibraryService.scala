package services

import models.*

import java.time.LocalDateTime
import java.util.UUID
import utils.ErrorHandling.*
import utils.CustomTypes.*
import utils.Id

/** 
  * This class is used to manage the object Library's Catalog
  */
case class LibraryService(private val catalog: LibCatalog = Catalog.empty) {

  // book management
  /**
   * Add a book to the Library's catalog
   * 
   * @param book A object Book that is going to be added to the list of book inside the library catalog book
   * @return An updated Library service with the object Book add the Library's catalog
   * */
  def addBook(book: Book): LibraryResult[LibraryService] = {
    if (catalog.getBook(book.id).isDefined) {
      Left(LibraryError.BookAlreadyExists(book.id))
    } else {
      val newCatalog = catalog.addBook(book)
      Right(this.copy(catalog = newCatalog))
    }
  }
  /**
   * This Function remove book.
   * @param Id the id of the book
   * @return an updated library without the book
   * */
  def removeBook(bookId: Id): LibraryResult[LibraryService] = try {
    val newCatalog = catalog.removeBook(bookId)
    Right(this.copy(catalog = newCatalog))
  } catch {
    case _ => Left(LibraryError.BookNotFound(bookId))
  }
  /**
   * Search the list of book base for a title, author's name or genre
   * 
   * @param query a String corresponding to the title, author's name or genre we are searching
   * @param SearchType a SearchType indicating the what field of book we are querying
   * @return A list of book who statify the query
   * */
  def searchBooks(
      query: String,
      searchType: SearchType
  ): LibraryResult[List[Book]] = {
    val results = searchType match {
      case SearchType.Title  => catalog.findBooksByTitle(query)
      case SearchType.Author => catalog.findBooksByAuthor(query)
      case SearchType.Genre  => catalog.findBooksByGenre(query)
    }
    Right(results)
  }

  // User management
  /**
   * Add a User to the Map of user of the library
   * 
   * @param user the User added the Map of user of the Library
   * @return return a Library result containing a Library Error on the Left if the id of the user already exist and the updated LibraryService on the Right
   * */
  def addUser(user: User): LibraryResult[LibraryService] = {
    if (catalog.getUser(user.id).isDefined) {
      Left(LibraryError.UserAlreadyExists(user.id))
    } else {
      val newCatalog = catalog.addUser(user)
      Right(this.copy(catalog = newCatalog))
    }
  }
  /**
   * Remove a User from the Map of user of the library
   * 
   * @param userId an Id corresponding to the user that we are going to delete
   * @return a new LibraryService without the object User
   * */
  def removeUser(userId: Id): LibraryResult[LibraryService] = try {
      val newCatalog = catalog.removeUser(userId)
      Right(this.copy(catalog = newCatalog))
  } catch {
    case _ => Left(LibraryError.UserNotFound(userId))
  }
  // Borrow and Return Operation
  /**
   * Update the library's catalog to borrow a book
   * 
   * @param userId a Id corresponding to the user borrowing the book
   * @param bookId a Id corresponding to the book the user is borrowing
   * @return LibraryResult containing a LibraryError on the Left or a Tuple containing the updated LibraryService the new Transaction on the Right
   * */
  def borrowBook(
      userId: Id,
      bookId: Id
  ): LibraryResult[(LibraryService, Transaction)] = {
    for {
      user <- catalog.getUser(userId).toRight(LibraryError.UserNotFound(userId))
      book <- catalog.getBook(bookId).toRight(LibraryError.BookNotFound(bookId))
      _ <- Either.cond(
        book.isAvailable,
        (),
        LibraryError.BookNotAvailable(bookId)
      )
      _ <- Either.cond(
        user.canBorrow,
        (),
        LibraryError.BorrowLimitExceeded(userId)
      )
      updatedUser <- user
        .borrowBook(bookId)
        .left
        .map(LibraryError.ValidationError.apply)
      updatedBook <- book.borrowCopy.left.map(
        LibraryError.ValidationError.apply
      )
      transaction = Transaction.createBorrow(
        Id(UUID.randomUUID().toString),
        userId,
        bookId
      )
      newCatalog = catalog
        .addUser(updatedUser)
        .addBook(updatedBook)
        .addTransaction(transaction)
    } yield (this.copy(catalog = newCatalog), transaction)
  }

  /**
   * Update the library's catalog to return a book
   *
   * @param userId a Id corresponding to the user returning the book
   * @param bookId a Id corresponding to the book the user is returning
   * @return LibraryResult containing a LibraryError on the Left or a Tuple containing the updated LibraryService the new Transaction on the Right
   * */
  def returnBook(
      userId: Id,
      bookId: Id
  ): LibraryResult[(LibraryService, Transaction)] = {
    for {
      user <- catalog.getUser(userId).toRight(LibraryError.UserNotFound(userId))
      book <- catalog.getBook(bookId).toRight(LibraryError.BookNotFound(bookId))
      _ <- Either.cond(
        user.borrowedBooks.contains(bookId),
        (),
        LibraryError.BookNotBorrowedByUser(userId, bookId)
      )
      borrowTransaction <- catalog
        .getTransactionsByUser(userId)
        .find(t =>
          t.bookId == bookId && t.transactionType == TransactionType.Borrow && t.returnDate.isEmpty
        )
        .toRight(LibraryError.BorrowTransactionNotFound(userId, bookId))
      fine = borrowTransaction.calculateFine()
      updatedUser <- user
        .returnBook(bookId)
        .left
        .map(LibraryError.ValidationError.apply)
      updatedBook <- book.returnCopy.left.map(
        LibraryError.ValidationError.apply
      )
      transaction = Transaction.createReturn(
        Id(UUID.randomUUID().toString),
        userId,
        bookId,
        if (fine > 0) Some(fine) else None
      )
      newCatalog = catalog
        .addUser(updatedUser)
        .addBook(updatedBook)
        .addTransaction(transaction)
    } yield (this.copy(catalog = newCatalog), transaction)
  }

  // Consultation
  /**
   * Get the library's Catalog.
   * 
   * @return a LibCatalog inside of the
   * */
  def getCatalog: LibCatalog = catalog
  
  /**
   * Get a List of Book containing the Book borrowed by one user
   * 
   * @param userId a Id corresponding to the user we want to get the list of borrowed book
   * @return A LibraryResult with a Library Error on the Left or a List of Book on the Right
   * */
  def getUserBorrowedBooks(userId: Id): LibraryResult[List[Book]] = {
    for {
      user <- catalog.getUser(userId).toRight(LibraryError.UserNotFound(userId))
      books = user.borrowedBooks.flatMap(catalog.getBook)
    } yield books
  }
  /**
   * Get the list of User, Book and Transaction corresponding to user who have overdue books
   * 
   * @retun A LibraryResult with a Library Error on the Left or a List of Tuple containing (User, Book, Transaction) on the Right
   * */
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
  /**
   * Display statistic of the LibCatalog
   *
   * */
  def displayStatistic(): Unit = {
    println("\n--- Statistic ---")
    println(s"Total number of book: ${this.catalog.totalBooks}")
    println(s"total number of user: ${this.catalog.totalUsers}")
    println(s"Total number of transaction (both borrowing and returning): ${this.catalog.totalTransactions}")
    println(s"Available books: ${this.catalog.availableBooks.length}")
  }
}
/**
 * An enum indicating the kind of search that is being performed.
 * */
enum SearchType {
  case Title, Author, Genre
}
