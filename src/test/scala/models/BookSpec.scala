package models

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.time.LocalDate
import utils.Id


class BookSpec extends AnyFunSuite with Matchers {

  test("An available books should allow borrowing") {
    val book = Book(
      id = Id("1"),
      title = "Test Book",
      author = List[String]("Test Author"),
      isbn = Id("978-0-123456-78-9"),
      publishedDate = LocalDate.of(2023, 1, 1),
      genre = "Test",
      totalCopies = 3,
      availableCopies = 3
    )

    book.isAvailable shouldBe true

    val result = book.borrowCopy
    result shouldBe a[Right[_, _]]
    result.getOrElse(book).availableCopies shouldBe 2
  }

  test(
    "A book without available copy should not allow borrowing"
  ) {
    val book = Book(
      id = Id("2"),
      title = "Test Book",
      author = List[String]("Test Author"),
      isbn = Id("978-0-123456-78-9"),
      publishedDate = LocalDate.of(2023, 1, 1),
      genre = "Test",
      totalCopies = 1,
      availableCopies = 0
    )

    book.isAvailable shouldBe false

    val result = book.borrowCopy
    result shouldBe a[Left[_, _]]
  }

  test("Returning a book should increase the number of copy available.") {
    val book = Book(
      id = Id("3"),
      title = "Test Book",
      author = List[String]("Test Author"),
      isbn = Id("978-0-123456-78-9"),
      publishedDate = LocalDate.of(2023, 1, 1),
      genre = "Test",
      totalCopies = 3,
      availableCopies = 2
    )

    val result = book.returnCopy
    result shouldBe a[Right[_, _]]
    result.getOrElse(book).availableCopies shouldBe 3
  }
}
