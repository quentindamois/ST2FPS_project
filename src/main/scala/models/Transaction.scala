package models

import java.time.LocalDateTime
import upickle.default.*
import utils.JsonUtil.*
import utils.JsonUtil.ReaderWriterLocalDate
import utils.CustomTypes.*
import utils.Id
import utils.ValidationUtil.validate
import services.ValidationService.transactionCondition

/**
 * Represent a transaction (Borrowing/Return) in the Library
 *
 * @constructor create an object Transaction
 * @param id an Id corresponding to the id of the transaction
 * @param userId an Id corresponding to the id of the user borrowing or returning the book
 * @param bookId an Id corresponding to the id of the book borrowed or Returned
 * @param transactionType a TransactionType indicating if the transaction correspond to borrowing or returning
 * @param timestamp a LocalDateTime corresponding to time when the transaction was done.
 * @param dueDate an Option[LocalDateTime] corresponding to the time when the book must be returned
 * @param returnDate an Option[LocalDateTime] corresponding to the time when the book was returned
 * @param fine an Option[Double] a double corresponding to the fine that the user has to pay
 */
case class Transaction(
  id: Id,
  userId: Id,
  bookId: Id,
  transactionType: TransactionType,
  timestamp: LocalDateTime,
  dueDate: Option[LocalDateTime] = None,
  returnDate: Option[LocalDateTime] = None,
  fine: Option[Double] = None
) derives ReadWriter {
  /**
   * Check if the Borrowed book is overdue
   *
   * @return a Boolean, true if the Borrowed book is overdue and false if not
   * */
  def isOverdue: Boolean = {
    (transactionType, dueDate, returnDate) match {
      case (TransactionType.Borrow, Some(due), None) => LocalDateTime.now().isAfter(due)
      case _ => false
    }
  }
  /**
   * Calculate the fine if the return date for a return or the current date for a borrow is after the due date or if not 0 is returned.
   * if the transaction is a return then we multiply the difference between the dueDate and the ReturnDate by finePerDay
   * if the transaction is a borrow then we multiply the difference between now and the dueDate by finePerDay
   *
   * @param finePerDay a Double corresponding to the fine that have to be paid for each day the book has not been returned
   * @return a double corresponding to the fine the user has to pay when returning the book
   * */
  def calculateFine(finePerDay: Double = 0.5): Double = {
    (transactionType, dueDate, returnDate) match {
      case (TransactionType.Borrow, Some(due), None) if LocalDateTime.now().isAfter(due) =>
        val overdueDays = java.time.Duration.between(due, LocalDateTime.now()).toDays
        overdueDays * finePerDay
      case (TransactionType.Return, Some(due), Some(returned)) if returned.isAfter(due) =>
        val overdueDays = java.time.Duration.between(due, returned).toDays
        overdueDays * finePerDay
      case _ => 0.0
    }
  }
  /**
   * Validate the field of an object Transaction.
   * */
  def validateTransaction(): Result[Transaction] = this.validate(transactionCondition)
}

/**
 * An enum use to represent the type of transaction.
 * */
enum TransactionType derives ReadWriter {
  case Borrow, Return
}

object Transaction {
  /**
   * This function is create a Transaction object corresponding to a Borrow
   *
   * @constructor Create a Transaction object corresponding with the field TransactionType containing the value TransactionType.Borrow
   * @param id an Id corresponding id of the transaction
   * @param userId an Id corresponding to the id of the user
   * @param bookId an Id corresponding to the id of the book
   * @param borrowPeriodDays an Int corresponding to the number of days the book is being borrowed.
   * @return the newly created Transaction object with TransactionType.Borrow inside the field transactionType
   * */
  def createBorrow(id: Id, userId: Id, bookId: Id, borrowPeriodDays: Int = 14): Transaction = {
    val now = LocalDateTime.now()
    Transaction(
      id = id,
      userId = userId,
      bookId = bookId,
      transactionType = TransactionType.Borrow,
      timestamp = now,
      dueDate = Some(now.plusDays(borrowPeriodDays))
    )
  }
  /**
   * This function is create a Transaction object corresponding to a Borrow
   *
   * @constructor Create a Transaction object corresponding with the field TransactionType containing the value TransactionType.Borrow
   * @param id               an Id corresponding id of the transaction
   * @param userId           an Id corresponding to the id of the user
   * @param bookId           an Id corresponding to the id of the book
   * @param fine Option[Double] corresponding to the fine the user returning the book has to pay
   * @return the newly created Transaction object with TransactionType.Return inside the field transactionType
   * */
  def createReturn(id: Id, userId: Id, bookId: Id, fine: Option[Double] = None): Transaction = {
    Transaction(
      id = id,
      userId = userId,
      bookId = bookId,
      transactionType = TransactionType.Return,
      timestamp = LocalDateTime.now(),
      returnDate = Some(LocalDateTime.now()),
      fine = fine
    )
  }
}

