package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import models._
import utils.Id
import java.time.{LocalDate, LocalDateTime}

class LibrarySpec extends AnyFlatSpec with Matchers {

  val book1: Book = Book(
    id = Id("b1"),
    title = "Functional Programming in Scala",
    author = List("Paul Chiusano", "Runar Bjarnason"),
    isbn = Id("isbn1"),
    publishedDate = LocalDate.of(2014, 1, 1),
    genre = "Programming",
    totalCopies = 3,
    availableCopies = 3,
    description = Some("A comprehensive guide to functional programming in Scala.")
  )

  val book2: Book = Book(
    id = Id("b2"),
    title = "Scala Cookbook",
    author = List("Alvin Alexander"),
    isbn = Id("isbn2"),
    publishedDate = LocalDate.of(2013, 7, 1),
    genre = "Programming",
    totalCopies = 2,
    availableCopies = 0,
    description = Some("Recipes for common Scala problems.")
  )

  val user1: User = User(
    id = Id("u1"),
    firstName = "Noam",
    lastName = "Damois",
    email = "ND@gmail.com",
    membershipDate = LocalDate.now().minusYears(1),
    userType = UserType.Student
  )

  val user2: User = User(
    id = Id("u2"),
    firstName = "Quentin",
    lastName = "Detournay",
    email = "21chaise@gmail.com",
    membershipDate = LocalDate.now(),
    userType = UserType.Faculty
  )

  val transaction1: Transaction = Transaction.createBorrow(
    id = Id("t1"),
    userId = user1.id,
    bookId = book1.id,
    borrowPeriodDays = 15
  )


  "LibCatalog" should "add and retrieve books" in {
    val catalog = Catalog.empty.addBook(book1).addBook(book2)

    catalog.totalBooks shouldBe 2
    catalog.getBook(book1.id) should contain(book1)
    catalog.findBooksByTitle("Functional") should contain(book1)
    catalog.findBooksByAuthor("Paul Chiusano") should contain(book1)
    catalog.findBooksByGenre("Programming") should contain allOf (book1, book2)
  }

  it should "add and remove users" in {
    val catalog = Catalog.empty.addUser(user1).addUser(user2)
    catalog.totalUsers shouldBe 2

    val updated = catalog.removeUser(user1.id)
    updated.totalUsers shouldBe 1
    updated.getUser(user1.id) shouldBe None
  }

  it should "track transactions correctly" in {
    val catalog = Catalog.empty.addUser(user1).addBook(book1).addTransaction(transaction1)

    catalog.totalTransactions shouldBe 1
    catalog.getTransactionsByUser(user1.id) should contain(transaction1)
    catalog.getTransactionsByBook(book1.id) should contain(transaction1)
  }

  it should "distinguish available and borrowed books" in {
    val catalog = Catalog.empty.addBook(book1).addBook(book2)

    catalog.availableBooks should contain(book1)
    catalog.borrowedBooks should contain(book2)
  }

  it should "filter overdue transactions" in {
    val overdueTransaction = transaction1.copy(
      dueDate = Some(LocalDateTime.now().minusDays(3)) // déjà en retard
    )
    val catalog = Catalog.empty.addTransaction(overdueTransaction)

    catalog.overdueTransactions should contain(overdueTransaction)
  }

  it should "support chained operations" in {
    val catalog = Catalog.empty
      .addUser(user1)
      .addBook(book1)
      .addTransaction(transaction1)

    catalog.totalBooks shouldBe 1
    catalog.totalUsers shouldBe 1
    catalog.totalTransactions shouldBe 1
  }
}
