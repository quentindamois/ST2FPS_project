package services

import models.*

/** Service de recommandation de livres Implémentation immutable conforme aux
  * principes de la programmation fonctionnelle
  */
case class RecommendationService(libraryService: LibraryService) {
  def recommendBooksByGenre(userId: String, limit: Int = 5): List[Book] = {
    val userBorrowHistory = getUserBorrowHistory(userId)
    val preferredGenres = getPreferredGenres(userBorrowHistory)

    val catalog = libraryService.getCatalog
    val availableBooks = catalog.availableBooks

    // Recommander des livres du même genre que ceux empruntés précédemment
    val recommendations = availableBooks
      .filter(book => preferredGenres.contains(book.genre))
      .filterNot(book => userBorrowHistory.map(_.id).contains(book.id))
      .take(limit)

    // Si pas assez de recommandations, ajouter des livres populaires
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

  def recommendBooksByAuthor(userId: String, limit: Int = 5): List[Book] = {
    val userBorrowHistory = getUserBorrowHistory(userId)
    val preferredAuthors = getPreferredAuthors(userBorrowHistory)

    val catalog = libraryService.getCatalog
    val availableBooks = catalog.availableBooks

    availableBooks
      .filter(book => preferredAuthors.contains(book.author))
      .filterNot(book => userBorrowHistory.map(_.id).contains(book.id))
      .take(limit)
  }

  def getSimilarBooks(bookId: String, limit: Int = 5): List[Book] = {
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

  private def getUserBorrowHistory(userId: String): List[Book] = {
    val catalog = libraryService.getCatalog
    val userTransactions = catalog.getTransactionsByUser(userId)

    userTransactions
      .filter(_.transactionType == TransactionType.Borrow)
      .flatMap(t => catalog.getBook(t.bookId))
  }

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

  private def getPopularBooks(catalog: LibCatalog): List[Book] = {
    // Calculer la popularité basée sur le nombre d'emprunts
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

  private def calculateSimilarity(book1: Book, book2: Book): Double = {
    var similarity = 0.0

    // Même genre = +0.5
    if (book1.genre == book2.genre) similarity += 0.5

    // Même auteur = +0.3
    if (book1.author == book2.author) similarity += 0.3

    // Année de publication proche = +0.2
    val yearDiff =
      math.abs(book1.publishedDate.getYear - book2.publishedDate.getYear)
    if (yearDiff <= 5) similarity += 0.2

    similarity
  }
}
