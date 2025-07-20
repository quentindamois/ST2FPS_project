package services

import models.*
import utils.Id

/**
  * An object used to do recommendation
  *
  * @constructor Create an object RecommendationService
  * @param LibraryService a LibraryService we will use to recommend the user inside the book in the Library's catalog
  */
case class RecommendationService(libraryService: LibraryService) {
  /**
   * Recommend a list of book available book to a user based on the genre of the books previously borrowed by hte user
   *
   * @param userID an Id corresponding to the id of user we are recommanding book to
   * @param limit an Int corresponding to the number of book we are recommanding to the user
   * @return A list of Book recommended to the user based on the genre
   * */
  def recommendBooksByGenre(userId: Id, limit: Int = 5): List[Book] = {
    val userBorrowHistory = getUserBorrowHistory(userId)
    val preferredGenres = getPreferredGenres(userBorrowHistory)

    val catalog = libraryService.getCatalog
    val availableBooks = catalog.availableBooks
    
    // Recommending book with the same genre as the book previously borrowed
    val recommendations = availableBooks
      .filter(book => preferredGenres.contains(book.genre))
      .filterNot(book => userBorrowHistory.map(_.id).contains(book.id))
      .take(limit)

    // If there is not enough document we add popular books
    if (recommendations.length < limit) {
      val popularBooks = getPopularBooks(catalog)
        .filterNot(book => userBorrowHistory.map(_.id).contains(book.id))
        .filterNot(book => recommendations.map(_.id).contains(book.id))
        .take(limit - recommendations.length)

      recommendations ++ popularBooks
    } else {
      recommendations
    }
  }
  /**
   * Recommend a list of book available book to a user based on the author of the books previously borrowed by hte user
   *
   * @param userID an Id corresponding to the id of user we are recommending book to
   * @param limit an Int corresponding to the number of book we are recommending to the user
   * @return A list of Book recommended to the user based on the genre
   * */
  def recommendBooksByAuthor(userId: Id, limit: Int = 5): List[Book] = {
    val userBorrowHistory = getUserBorrowHistory(userId)
    val preferredAuthors = getPreferredAuthors(userBorrowHistory)

    val catalog = libraryService.getCatalog
    val availableBooks = catalog.availableBooks

    availableBooks
      .filter(book => preferredAuthors.contains(book.author))
      .filterNot(book => userBorrowHistory.map(_.id).contains(book.id))
      .take(limit)
  }
  /**
   * Get a list of Book that have the same genre or author as the id of the book given as an input
   *
   * @param bookId an Id corresponding to the id of the book we want to find similarities
   * @param limit an Int corresponding to the maximum number of book we want to find
   * @return List of Book that have the same genre or author as the book given as an input
   * */
  def getSimilarBooks(bookId: Id, limit: Int = 5): List[Book] = {
    val catalog = libraryService.getCatalog

    catalog.getBook(bookId) match {
      case Some(targetBook) =>
        catalog.availableBooks
          .filter(book => book.id != bookId)
          .filter(book =>
            book.genre == targetBook.genre || book.author == targetBook.author
          )
          .sortBy(book => calculateSimilarity(targetBook, book))
          .reverse
          .take(limit)
      case None => List.empty
    }
  }
  /**
   * Get the list of books of a user has borrowed
   *
   * @param userID an Id corresponding to the user we want to get the list of borrowed book
   * @return a List of Book that correspond to all the book the user has borrowed
   * */
  private def getUserBorrowHistory(userId: Id): List[Book] = {
    val catalog = libraryService.getCatalog
    val userTransactions = catalog.getTransactionsByUser(userId)

    userTransactions
      .filter(_.transactionType == TransactionType.Borrow)
      .flatMap(t => catalog.getBook(t.bookId))
  }
  /**
   * Get the three most common genre of the book inside a list of book
   *
   * @param book the List of Book we are looking at
   * @return a string corresponding to the list genre the most common genre
   * */
  private def getPreferredGenres(books: List[Book]): List[String] = {
    books
      .groupBy(_.genre)
      .view
      .mapValues(_.length)
      .toList
      .sortBy(-_._2)
      .map(_._1)
      .take(3)
  }
  /**
   * Get the three most common genre of the book inside a list of book
   *
   * @param book the List of Book we are looking at
   * @return a string corresponding to the list genre the most common genre
   * */
  private def getPreferredAuthors(books: List[Book]): List[String] = {
    books
      .groupBy(_.author)
      .view
      .mapValues(_.length)
      .toList
      .sortBy(-_._2)
      .map(_._1)
      .take(3)
  }
  /**
   * Get the most borrowed book from the library's catalog
   * 
   * @param catalog a LibCatalog corresponding to a library's catalog from which we want to get the populare books
   * @return a List of Book containing the Book that are most popular
   * */
  private def getPopularBooks(catalog: LibCatalog): List[Book] = {
    val borrowCounts = catalog.transactions
      .filter(_.transactionType == TransactionType.Borrow)
      .groupBy(_.bookId)
      .view
      .mapValues(_.length)
      .toMap

    catalog.availableBooks
      .sortBy(book => borrowCounts.getOrElse(book.id, 0))
      .reverse
  }
/**
 * Calculate the similarity between two Books
 * 
 * @param book1 the first book we want to compare
 * @param book2 the secodn book we want to compare
 * @return a double corresponding to the similarity between to Books
 * */
  private def calculateSimilarity(book1: Book, book2: Book): Double = {
    var similarity = 0.0

    // Same genre = +0.5
    if (book1.genre == book2.genre) similarity += 0.5

    // Same author = +0.3
    if (book1.author == book2.author) similarity += 0.3

    // Year of publication = +0.2
    val yearDiff =
      math.abs(book1.publishedDate.getYear - book2.publishedDate.getYear)
    if (yearDiff <= 5) similarity += 0.2

    similarity
  }
}
