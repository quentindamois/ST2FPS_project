package models

import java.time.LocalDateTime
import upickle.default.*
import utils.JsonUtil.*
import utils.JsonUtil.ReaderWriterLocalDate
import utils.CustomTypes.*
import utils.Id

/**
 * Représente une transaction (emprunt/retour) dans la bibliothèque
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
  def isOverdue: Boolean = {
    (transactionType, dueDate, returnDate) match {
      case (TransactionType.Borrow, Some(due), None) => LocalDateTime.now().isAfter(due)
      case _ => false
    }
  }
  
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
}

enum TransactionType derives ReadWriter {
  case Borrow, Return
}

object Transaction {
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

