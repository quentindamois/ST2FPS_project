package utils

import utils.ErrorHandling.*

object CustomTypes {
  //These types are use for field of function
  /**
   * An Option[String] used for field description of the Object Bbook
   * */
  type Description = Option[String]
  //These are use to return content from function
  /**
   * An Either[String, T] use for the return type of function with T a wild card.
   * */
  type Result[T] = Either[String, T]
  /**
   * An Either[LibraryError, T] use for the return type of function of the LibraryService package.
   * */
  type LibraryResult[T] = Either[LibraryError, T]
  /**
   * An Either[FileError, T] used for the function used to create the function saveToFile and ReadToFile
   * */
  type fileOperationInnerResult[A] = Either[FileError, A]
  /**
   * A Map[Id, T] used for the field books and users from class LibCatalog.
   * */
  type LibraryMapIdTo[T] = Map[Id, T]
}
