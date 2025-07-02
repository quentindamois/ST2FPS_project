
package utils

sealed trait LibraryError {
  def message: String
}

object LibraryError {
  case class BookNotFound(bookId: String) extends LibraryError {
    override def message: String = s"Le livre avec l'ID $bookId n'a pas été trouvé"
  }

  case class UserNotFound(userId: String) extends LibraryError {
    override def message: String = s"L'utilisateur avec l'ID $userId n'a pas été trouvé"
  }

  case class BookAlreadyExists(bookId: String) extends LibraryError {
    override def message: String = s"Un livre avec l'ID $bookId existe déjà"
  }

  case class UserAlreadyExists(userId: String) extends LibraryError {
    override def message: String = s"Un utilisateur avec l'ID $userId existe déjà"
  }

  case class BookNotAvailable(bookId: String) extends LibraryError {
    override def message: String = s"Le livre avec l'ID $bookId n'est pas disponible"
  }

  case class BorrowLimitExceeded(userId: String) extends LibraryError {
    override def message: String = s"L'utilisateur avec l'ID $userId a atteint sa limite d'emprunts"
  }

  case class BookNotBorrowedByUser(userId: String, bookId: String) extends LibraryError {
    override def message: String = s"Le livre $bookId n'est pas emprunté par l'utilisateur $userId"
  }

  case class BorrowTransactionNotFound(userId: String, bookId: String) extends LibraryError {
    override def message: String = s"Aucune transaction d'emprunt trouvée pour le livre $bookId par l'utilisateur $userId"
  }

  case class ValidationError(msg: String) extends LibraryError {
    override def message: String = msg
  }
}