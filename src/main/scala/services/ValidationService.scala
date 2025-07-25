package services
import utils.CustomTypes.*
import utils.Id
import models.{Book, LibCatalog, Transaction, TransactionType, User}
import utils.CustomTypes.ListRequirements
import utils.ValidationUtil.validate

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.time.LocalDate

object ValidationService {
  /**
   * The list of requirement for validation of a book.
   * */
  val bookCondition:ListRequirements[Book] =  List(
    ((book: Book) => (book.isbn.toString.filter(_.isDigit).length == 10) | (book.isbn.toString.filter(_.isDigit).length == 13), "The ISBN should either have 10 or 13 digit."),
    ((book: Book) => book.isbn.toString.nonEmpty, "The ISBN should not be empty"),
    ((book: Book) => """^\w+[\s\d-]*$""".r matches book.genre, "The genre can only contain alpha-numerical and '-'."),
    ((book: Book) => book.genre.nonEmpty, "Genre must not be empty."),
    ((book: Book) => book.author.map("""^([AZERTYUIOPQSDFGHJKLMWXCVBN][azertyuiopqsdfghjklmwxcvbn]*(-[AZERTYUIOPQSDFGHJKLMWXCVBN][azertyuiopqsdfghjklmwxcvbn]*)*\s*)+$""".r matches _).reduce(_ & _), "Every authors' should only contain letter or dash."),
    ((book: Book) => book.author.map(oneAuthor => !("""\d""".r matches oneAuthor)).reduce(_ & _), "The author name must not contain number"),
    ((book: Book) => book.author.map(oneAuthor => !("""\\_`@\)\(\{\}=\+\*/~\.:!\?,;§%¨\^¤#°""".r matches oneAuthor)).reduce(_ & _), "The authors' name should not contain any special character."),
    ((book: Book) => book.author.nonEmpty, "The list of author should not be empty."),
    ((book: Book) => book.author.map(_.nonEmpty).reduce(_ & _), "No authors' name should be empty."),
    ((book: Book) => book.totalCopies > 0, "The total number of copies should be strictly positives."),
    ((book: Book) => book.availableCopies > -1, "The number of available copies should not be a negative number."),
    ((book: Book) => book.totalCopies >= book.availableCopies, "The number of available copies should not be higher than the number of total copies.")
  )
  /**
   * List of requirement for the validation of User.
   * */
  val userCondition: ListRequirements[User] = List(
    ((user: User) => ("""^\w+.?\w*@\w+.\w+$""".r matches user.email), "The email's address has the wrong format"),
    ((user: User) => user.email.count(_ == '@') == 1, "There should be only one '@' in the email address."),
    ((user: User) => user.email.nonEmpty, "The user's email address should not be empty"),
    ((user: User) => """^([AZERTYUIOPQSDFGHJKLMWXCVBN][azertyuiopqsdfghjklmwxcvbn]*(-[AZERTYUIOPQSDFGHJKLMWXCVBN][azertyuiopqsdfghjklmwxcvbn]*)*\s*)+$""".r matches user.firstName, "The user's first name  should only contain letter or dash and start by an Uppercase letter."),
    ((user: User) => !("""\d""".r matches user.firstName), "The user's first name must not contain number"),
    ((user: User) => !("""\\_`@\)\(\{\}=\+\*/~\.:!\?,;§%¨\^¤#°""".r matches user.firstName), "The authors' name should not contain any special character."),
    ((user: User) => user.firstName.nonEmpty, "The user's first name should not be empty."),
    ((user: User) => """^([AZERTYUIOPQSDFGHJKLMWXCVBN][azertyuiopqsdfghjklmwxcvbn]*(-[AZERTYUIOPQSDFGHJKLMWXCVBN][azertyuiopqsdfghjklmwxcvbn]*)*\s*)+$""".r matches user.lastName, "The user's last name should only contain letter or dash and start by an Uppercase letter."),
    ((user: User) => !("""\d""".r matches user.lastName), "The author name must not contain number"),
    ((user: User) => !("""\\_`@\)\(\{\}=\+\*/~\.:!\?,;§%¨\^¤#°""".r matches user.lastName), "The user' name should not contain any special character."),
    ((user: User) => user.lastName.nonEmpty, "The user's last name should not be empty."),
    ((user: User) => user.maxBorrowLimit > 0, "The maximum number of books a user can borrow should be positive."),
    ((user: User) => LocalDate.now().isAfter(user.membershipDate) | LocalDate.now().isEqual(user.membershipDate), "The membership date should not be in the future")
  )
  /**
   * List of requirement for the object Transaction.
   * */
  val transactionCondition: ListRequirements[Transaction] = List(
    ((transaction: Transaction) => transaction.fine.isEmpty | (transaction.fine.isDefined & transaction.fine.get > 0.0), "The fine should be a non null positive integer.")
  )
  /**
   * Validating the type of the input
   * */
  val conversionCondition: ListRequirements[List[((String, String), String => Any)]] = List(
    ((input: List[((String, String), String => Any)]) =>  input.map((individual: ((String, String), String => Any)) =>  (!individual._2.isInstanceOf[String => Int]) | (individual._2.isInstanceOf[String => Int] & individual._1._2.isInteger)).reduce(_ & _), "A String cannot be converted to Integer"),
    ((input: List[((String, String), String => Any)]) =>  input.map((individual: ((String, String), String => Any)) =>  (!individual._2.isInstanceOf[String => Double]) | (individual._2.isInstanceOf[String => Int] & individual._1._2.isDouble)).reduce(_ & _), "A String cannot be converted to Double"),
    ((input: List[((String, String), String => Any)]) => input.map((individual: ((String, String), String => Any)) =>  (!individual._2.isInstanceOf[String => LocalDate]) | (individual._2.isInstanceOf[String => Int] & individual._1._2.isLocalDate)).reduce(_ & _), "A String cannot be converted to a LocalDate.")
  )
  /**
   * Validating a tuple User and LibraryService
   * */
  val addUserToLibCondition: ListRequirements[(User, LibraryService)] = List(
    ((user: User, libraryService: LibraryService) => libraryService.getCatalog.users.values.count(_.email == user.email) == 0, "All email should be unique"),
    ((user: User, libraryService: LibraryService) => libraryService.getCatalog.users.values.count(_.id == user.id) == 0, "All Id should be unique")
  )
  /**
   * Validation a tuple Book and LibraryService
   * */
  val addBookToLibCondition: ListRequirements[(Book, LibraryService)] = List(
    ((book: Book, libraryService: LibraryService) => libraryService.getCatalog.books.values.count(_.isbn == book.isbn) == 0, "All isbn should be unique."),
    ((book: Book, libraryService: LibraryService) => libraryService.getCatalog.books.values.count(_.id == book.id) == 0, "All Id should be unique.")
  )
  val returnBookCondition: ListRequirements[(List[(String, Id)], LibraryService)] = List(
    ((listId: List[(String, Id)], libService: LibraryService) => libService.getCatalog.UserPreviouslyBorrowedBook(listId.toMap.getOrElse("idBook",Id("0")), listId.toMap.getOrElse("idUser",Id("0"))) match {
      case Right(value) => value
      case Left(error) => false
    }, "The user did not borrow the book.")
  )
  /**
   * This function check to see if the String can be converted in a value LocalDate
   * */
  extension(str: String) def isLocalDate: Boolean = try {
    LocalDate.parse(str)
    true
  } catch {
    case _ => false
  }
  /**
   * this function validate to see if a String can be converted to Double
   * */
  extension(str: String) def isDouble: Boolean = try {
    str.toDouble
    true
  } catch {
    case _ => false
  }
  /**
   * This function validate to see if a String can be converted to LocalDate.
   * */
  extension(str: String) def isInteger: Boolean = try {
    str.toInt
    true
  } catch {
    case _ => false 
  }

  /**
   * Find the id of a book's id based on its isbn.
   *
   * @param isbn a String corresponding to the user's email address
   * @return the id of the user
   * */
  extension (libCat: LibCatalog) def UserPreviouslyBorrowedBook(idBook: Id, idUser: Id): Result[Boolean] = try {
    val boolResult = libCat.transactions.count((transaction: Transaction) => transaction.userId.equals(idUser) & transaction.bookId.equals(idBook) & (transaction.transactionType == TransactionType.Borrow)) % 2 == 1
    Right(boolResult)
  } catch {
    case _ => Left("Encoutered an error when looking at the number of transaction.")
  }
}