package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import models._
import utils.Id
import java.time.LocalDateTime

class TransactionSpec extends AnyFlatSpec with Matchers {

  val userId: Id = Id("u1")
  val bookId: Id = Id("b1")

  "Transaction.createBorrow" should "create a borrow transaction with due date" in {
    val transaction = Transaction.createBorrow(Id("t1"), userId, bookId, borrowPeriodDays = 7)

    transaction.transactionType shouldBe TransactionType.Borrow
    transaction.userId shouldBe userId
    transaction.bookId shouldBe bookId
    transaction.dueDate.isDefined shouldBe true
    transaction.returnDate shouldBe None
  }

  "Transaction.createReturn" should "create a return transaction without due date" in {
    val transaction = Transaction.createReturn(Id("t2"), userId, bookId)

    transaction.transactionType shouldBe TransactionType.Return
    transaction.userId shouldBe userId
    transaction.bookId shouldBe bookId
    transaction.returnDate.isDefined shouldBe true
    transaction.dueDate shouldBe None
  }

  "isOverdue" should "return true for overdue borrow" in {
    val overdueTransaction = Transaction.createBorrow(Id("t3"), userId, bookId)
      .copy(dueDate = Some(LocalDateTime.now().minusDays(3)))

    overdueTransaction.isOverdue shouldBe true
  }

  it should "return false if transaction is returned or not overdue" in {
    val notOverdue = Transaction.createBorrow(Id("t4"), userId, bookId)
    notOverdue.isOverdue shouldBe false

    val returned = Transaction.createReturn(Id("t5"), userId, bookId)
    returned.isOverdue shouldBe false
  }

  "calculateFine" should "return 0 if book is returned on time" in {
    val dueDate = LocalDateTime.now().minusDays(1)
    val returnDate = LocalDateTime.now().minusDays(1)

    val transaction = Transaction(
      id = Id("t6"),
      userId = userId,
      bookId = bookId,
      transactionType = TransactionType.Return,
      timestamp = LocalDateTime.now(),
      dueDate = Some(dueDate),
      returnDate = Some(returnDate)
    )

    transaction.calculateFine(finePerDay = 1.0) shouldBe 0.0
  }

  it should "calculate fine for overdue borrow transaction" in {
    val dueDate = LocalDateTime.now().minusDays(5)
    val transaction = Transaction.createBorrow(Id("t7"), userId, bookId)
      .copy(dueDate = Some(dueDate))

    val fine = transaction.calculateFine(finePerDay = 2.0)
    fine should be >= 10.0  // 5 jours * 2.0
  }

  it should "calculate fine for overdue return transaction" in {
    val dueDate = LocalDateTime.now().minusDays(5)
    val returnDate = LocalDateTime.now()

    val transaction = Transaction(
      id = Id("t8"),
      userId = userId,
      bookId = bookId,
      transactionType = TransactionType.Return,
      timestamp = LocalDateTime.now(),
      dueDate = Some(dueDate),
      returnDate = Some(returnDate)
    )

    val fine = transaction.calculateFine(finePerDay = 1.0)
    fine should be >= 5.0
  }

  it should "return 0 if dueDate is not defined" in {
    val transaction = Transaction.createBorrow(Id("t9"), userId, bookId)
      .copy(dueDate = None)

    transaction.calculateFine() shouldBe 0.0
  }
}
