sealed trait UserType
object UserType {
  case object Student extends UserType
  case object Faculty extends UserType
  case object Librarian extends UserType
}

case class User(
    id: String,
    firstName: String,
    lastName: String,
    email: String,
    userType: UserType,
    borrowedBooks: List[String] = List.empty,
    maxBorrowLimit: Int = 5
) {
  def fullName: String = s"$firstName $lastName"

  def canBorrow: Boolean = borrowedBooks.length < maxBorrowLimit

  def borrowBook(bookId: String): Either[String, User] =
    if (canBorrow && !borrowedBooks.contains(bookId))
      Right(this.copy(borrowedBooks = bookId :: borrowedBooks))
    else if (borrowedBooks.contains(bookId))
      Left("Ce livre est déjà emprunté")
    else
      Left("Limite d'emprunt atteinte")

  def returnBook(bookId: String): Either[String, User] =
    if (borrowedBooks.contains(bookId))
      Right(this.copy(borrowedBooks = borrowedBooks.filterNot(_ == bookId)))
    else
      Left("Ce livre n'est pas emprunté")

  def defaultMaxBorrowLimit(userType: UserType): Int = userType match {
    case UserType.Student   => 5
    case UserType.Faculty   => 10
    case UserType.Librarian => 2
  }
}
