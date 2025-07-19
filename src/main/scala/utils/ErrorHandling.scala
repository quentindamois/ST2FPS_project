package utils

import models._

object ErrorHandling {
  type Result[T] = Either[String, T]
  type LibraryResult[T] = Either[LibraryError, T]
  //The type for the type of the output for each function
  type fileOperationInnerResult[A] = Either[FileError, A]
  type fileOperationOutput[A] = Either[String, A]

  enum LibraryError(val message: String):
    case BookNotFound(id: String)
        extends LibraryError(s"Book with id $id not found")
    case BookAlreadyExists(id: String)
        extends LibraryError(s"Book with id $id already exists")
    case UserNotFound(id: String)
        extends LibraryError(s"User with id $id not found")
    case UserAlreadyExists(id: String)
        extends LibraryError(s"User with id $id already exists")
    case BookNotAvailable(id: String)
        extends LibraryError(s"Book with id $id is not available")
    case InvalidTransaction(id: String)
        extends LibraryError(s"Invalid transaction with id $id")
    case ValidationError(msg: String) extends LibraryError(msg)
    case BorrowLimitExceeded(userId: String)
        extends LibraryError(s"User $userId has exceeded borrow limit")
    case BookNotBorrowedByUser(userId: String, bookId: String)
        extends LibraryError(s"Book $bookId is not borrowed by user $userId")
    case BorrowTransactionNotFound(userId: String, bookId: String)
        extends LibraryError(
          s"Borrow transaction for user $userId and book $bookId not found"
        )

  // all the possible error
  enum FileError(val message: String):
    case PathError(path: String) extends FileError(s"$path cannot be access")
    case AuthorisationError(file: String) extends FileError(s"$file cannot be edited")
    case ReadingError(file: String) extends FileError(s"$file cannot be read")
    case ConversionError(startingType: String, finalType: String) extends FileError(s"$startingType cannot be converted to $finalType")
}
