package utils

import utils.ErrorHandling.*

object CustomTypes {
  //These types are use for field of function
  type Description = Option[String]
  //These are use to return content from function
  type Result[T] = Either[String, T]
  type LibraryResult[T] = Either[LibraryError, T]
  type fileOperationInnerResult[A] = Either[FileError, A]
}
