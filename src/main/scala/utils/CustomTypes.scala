package utils
import utils.ErrorHandling._

object CustomTypes {
  type Result[T] = Either[String, T]
  type LibraryResult[T] = Either[LibraryError, T]
  type fileOperationInnerResult[A] = Either[FileError, A]
}
