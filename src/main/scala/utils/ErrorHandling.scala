package utils

import models._

object ErrorHandling {
  /**
   * An enum used to handle the error that can be caused when calling the function from LibraryService.
   * */
  enum LibraryError(val message: String):
    case BookNotFound(id: Id)
        extends LibraryError(s"Book with id $id not found")
    case BookAlreadyExists(id: Id)
        extends LibraryError(s"Book with id $id already exists")
    case UserNotFound(id: Id)
        extends LibraryError(s"User with id $id not found")
    case UserAlreadyExists(id: Id)
        extends LibraryError(s"User with id $id already exists")
    case BookNotAvailable(id: Id)
        extends LibraryError(s"Book with id $id is not available")
    case InvalidTransaction(id: Id)
        extends LibraryError(s"Invalid transaction with id $id")
    case ValidationError(msg: String) extends LibraryError(msg)
    case BorrowLimitExceeded(userId: Id)
        extends LibraryError(s"User $userId has exceeded borrow limit")
    case BookNotBorrowedByUser(userId: Id, bookId: Id)
        extends LibraryError(s"Book $bookId is not borrowed by user $userId")
    case BorrowTransactionNotFound(userId: Id, bookId: Id)
        extends LibraryError(
          s"Borrow transaction for user $userId and book $bookId not found"
        )
  // all the possible error when using a file
  /**
   * An enum used to handle the error that can be caused when calling the function from JsonUtil.
   * */  
  enum FileError(val message: String):
    case PathError(path: String) extends FileError(s"$path cannot be access")
    case AuthorisationError(file: String) extends FileError(s"$file cannot be edited")
    case ReadingError(file: String) extends FileError(s"$file cannot be read")
    case ConversionError(startingType: String, finalType: String) extends FileError(s"$startingType cannot be converted to $finalType")
  /**
   * An enum used to handle the error that can be caused when calling the function ManualControl
   * */
  enum UserError(val message: String):
    case convertTypeError(input: String, typeOfInput: String) extends UserError(s"$input cannot be converted to $typeOfInput.")
    case wrongLineReading(field: String) extends UserError(s"$field could not be read.")
    case wrongInput() extends UserError("Encountered an error while entering values.")
    case noActionSelected(userInput: String) extends UserError(s"$userInput does not correspond to any input.\n1-search title\n2-search author\n3-search genre\n4-filter availability\n5-add user\n6-add book\n7-borrow Book\n8-return Book\n9-remove Book\n10-remove user\n11-get statistic\n12-get recommendation")
    case idNotFound(typeId: String) extends UserError(s"Id of $typeId could not be found.")
}

