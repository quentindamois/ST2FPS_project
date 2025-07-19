package services

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import models.*
import java.time.LocalDate
import java.util.UUID

class LibraryServiceSpec extends AnyFunSuite with Matchers {

  test("ajouter un livre au catalogue devrait réussir") {
    val libraryService = new LibraryService()
    val book = Book(
      id = UUID.randomUUID().toString,
      title = "Test Book",
      author = "Test Author",
      isbn = "978-0-123456-78-9",
      publishedDate = LocalDate.of(2023, 1, 1),
      genre = "Test",
      totalCopies = 1,
      availableCopies = 1
    )

    val result = libraryService.addBook(book)
    result shouldBe a[Right[_, _]]

    libraryService.getCatalog.getBook(book.id) shouldBe Some(book)
  }

  test("ajouter un utilisateur devrait réussir") {
    val libraryService = new LibraryService()
    val user = User(
      id = UUID.randomUUID().toString,
      firstName = "John",
      lastName = "Doe",
      email = "john.doe@email.com",
      membershipDate = LocalDate.now(),
      userType = UserType.Student
    )

    val result = libraryService.addUser(user)
    result shouldBe a[Right[_, _]]

    libraryService.getCatalog.getUser(user.id) shouldBe Some(user)
  }

  test("emprunter un livre disponible devrait réussir") {
    val libraryService = new LibraryService()

    val book = Book(
      id = UUID.randomUUID().toString,
      title = "Test Book",
      author = "Test Author",
      isbn = "978-0-123456-78-9",
      publishedDate = LocalDate.of(2023, 1, 1),
      genre = "Test",
      totalCopies = 1,
      availableCopies = 1
    )

    val user = User(
      id = UUID.randomUUID().toString,
      firstName = "John",
      lastName = "Doe",
      email = "john.doe@email.com",
      membershipDate = LocalDate.now(),
      userType = UserType.Student
    )

    libraryService.addBook(book)
    libraryService.addUser(user)

    val result = libraryService.borrowBook(user.id, book.id)
    result shouldBe a[Right[_, _]]

    val catalog = libraryService.getCatalog
    catalog.getBook(book.id).get.availableCopies shouldBe 0
    catalog.getUser(user.id).get.borrowedBooks should contain(book.id)
  }
}
