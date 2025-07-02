package models

import java.time.LocalDateTime
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*

/**
 * Représente une transaction (emprunt/retour) dans la bibliothèque
 */
case class Transaction(
  id: String,
  userId: String,
  bookId: String,
  transactionType: TransactionType,
  timestamp: LocalDateTime,
  dueDate: Option[LocalDateTime] = None,
  returnDate: Option[LocalDateTime] = None,
  fine: Option[Double] = None
) {
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

enum TransactionType {
  case Borrow, Return
}

object Transaction {
  given Encoder[Transaction] = deriveEncoder
  given Decoder[Transaction] = deriveDecoder
  
  def createBorrow(id: String, userId: String, bookId: String, borrowPeriodDays: Int = 14): Transaction = {
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
  
  def createReturn(id: String, userId: String, bookId: String, fine: Option[Double] = None): Transaction = {
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

object TransactionType {
  given Encoder[TransactionType] = deriveEncoder
  given Decoder[TransactionType] = deriveDecoder
}
