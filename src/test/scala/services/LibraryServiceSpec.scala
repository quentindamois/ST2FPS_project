package services

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import models.*
import utils.Id

import java.time.LocalDate
import java.util.UUID

class LibraryServiceSpec extends AnyFunSuite with Matchers {

  test("ajouter un livre au catalogue devrait réussir") {
    val libraryService = LibraryService()
    val book = Book(
      id = Id(UUID.randomUUID().toString),
      title = "Test Book",
      author = List["Test Author"],
      isbn = Id("978-0-123456-78-9"),
      publishedDate = LocalDate.of(2023, 1, 1),
      genre = "Test",
      totalCopies = 1,
      availableCopies = 1
    )

    val result = libraryService.addBook(book)
    result shouldBe a[Right[_, _]]

    val updatedService = result.getOrElse(libraryService)
    updatedService.getCatalog.getBook(book.id) shouldBe Some(book)
  }

  test("ajouter un utilisateur devrait réussir") {
    val libraryService = LibraryService()
    val user = User(
      id = Id(UUID.randomUUID().toString),
      firstName = "John",
      lastName = "Doe",
      email = "john.doe@email.com",
      membershipDate = LocalDate.now(),
      userType = UserType.Student
    )

    val result = libraryService.addUser(user)
    result shouldBe a[Right[_, _]]

    val updatedService = result.getOrElse(libraryService)
    updatedService.getCatalog.getUser(user.id) shouldBe Some(user)
  }

  test("emprunter un livre disponible devrait réussir") {
    val initialService = LibraryService()

    val book = Book(
      id = Id(UUID.randomUUID().toString),
      title = "Test Book",
      author = List["Test Author"],
      isbn = Id("978-0-123456-78-9"),
      publishedDate = LocalDate.of(2023, 1, 1),
      genre = "Test",
      totalCopies = 1,
      availableCopies = 1
    )

    val user = User(
      id = Id(UUID.randomUUID().toString),
      firstName = "John",
      lastName = "Doe",
      email = "john.doe@email.com",
      membershipDate = LocalDate.now(),
      userType = UserType.Student
    )

    // Adding data to show immutability
    val serviceWithBook = initialService.addBook(book).getOrElse(initialService)
    val serviceWithUser =
      serviceWithBook.addUser(user).getOrElse(serviceWithBook)

    val result = serviceWithUser.borrowBook(user.id, book.id)
    result shouldBe a[Right[_, _]]

    val (finalService, transaction) = result.getOrElse((serviceWithUser, null))
    val catalog = finalService.getCatalog
    catalog.getBook(book.id).get.availableCopies shouldBe 0
    catalog.getUser(user.id).get.borrowedBooks should contain(book.id)
  }
}
