package services
import utils.CustomTypes.*
import utils.Id
import models.{Book, Transaction, User}
import utils.CustomTypes.ListRequirements
import utils.ValidationUtil.validate

import java.time.LocalDate

object ValidationService {
  /**
   * The list of requirement for validation of a book.
   * */
  val bookCondition:ListRequirements[Book] =  List(
    ((book: Book) => ("""^\d-\d{4}-\d{4}-\d$""".r matches book.isbn.toString) | ("""^\d{3}-\d-\d{4}-\d{4}-\d$""".r matches book.isbn.toString), "The ISBN have the wrong format"),
    ((book: Book) => (book.isbn.toString.filter(_.isDigit).length == 10) | (book.isbn.toString.filter(_.isDigit).length == 13), "The ISBN should either have 10 or 13 digit."),
    ((book: Book) => book.isbn.toString.isEmpty, "The ISBN should not be empty"),
    ((book: Book) => """^\w+[\s\d-]*$""".r matches book.genre, "The genre can only contain alpha-numerical and '-'."),
    ((book: Book) => book.genre.isEmpty, "Genre must not be empty."),
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
    ((user: User) => """@\w.\w$""".r matches user.email, "The domain's address is not correct."),
    ((user: User) => user.email.nonEmpty, "The user's email address should not be empty"),
    ((user: User) => """^([AZERTYUIOPQSDFGHJKLMWXCVBN][azertyuiopqsdfghjklmwxcvbn]*(-[AZERTYUIOPQSDFGHJKLMWXCVBN][azertyuiopqsdfghjklmwxcvbn]*)*\s*)+$""".r matches user.firstName, "Every authors' should only contain letter or dash."),
    ((user: User) => !("""\d""".r matches user.firstName), "The author name must not contain number"),
    ((user: User) => !("""\\_`@\)\(\{\}=\+\*/~\.:!\?,;§%¨\^¤#°""".r matches user.firstName), "The authors' name should not contain any special character."),
    ((user: User) => user.firstName.nonEmpty, "The user's first name should not be empty."),
    ((user: User) => """^([AZERTYUIOPQSDFGHJKLMWXCVBN][azertyuiopqsdfghjklmwxcvbn]*(-[AZERTYUIOPQSDFGHJKLMWXCVBN][azertyuiopqsdfghjklmwxcvbn]*)*\s*)+$""".r matches user.lastName, "Every authors' should only contain letter or dash."),
    ((user: User) => !("""\d""".r matches user.lastName), "The author name must not contain number"),
    ((user: User) => !("""\\_`@\)\(\{\}=\+\*/~\.:!\?,;§%¨\^¤#°""".r matches user.lastName), "The authors' name should not contain any special character."),
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
}