package models

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import java.time.LocalDate
import utils.Id

class BookSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

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

  test("A book without available copy should not allow borrowing") {
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

  test("Borrowing should never decrease availableCopies below 0") {
    forAll { (totalCopies: Int, borrowed: Int) =>
      whenever(totalCopies >= 0 && borrowed >= 0) {
        val available = math.max(totalCopies - borrowed, 0)
        val book = Book(
          id = Id("p1"),
          title = "Prop Test",
          author = List("Author"),
          isbn = Id("isbn"),
          publishedDate = LocalDate.of(2023, 1, 1),
          genre = "Test",
          totalCopies = totalCopies,
          availableCopies = available
        )

        val result = book.borrowCopy
        result match {
          case Left(_) => book.availableCopies shouldBe 0
          case Right(updated) => updated.availableCopies should be >= 0
        }
      }
    }
  }

  test("Returning should never exceed totalCopies") {
    forAll { (totalCopies: Int, available: Int) =>
      whenever(totalCopies >= 0 && available >= 0) {
        val cappedAvailable = math.min(available, totalCopies)
        val book = Book(
          id = Id("p2"),
          title = "Prop Test",
          author = List("Author"),
          isbn = Id("isbn"),
          publishedDate = LocalDate.of(2023, 1, 1),
          genre = "Test",
          totalCopies = totalCopies,
          availableCopies = cappedAvailable
        )

        val result = book.returnCopy
        result match {
          case Left(_) => cappedAvailable shouldBe totalCopies
          case Right(updated) => updated.availableCopies should be <= totalCopies
        }
      }
    }
  }

  test("Borrow then return should restore availableCopies to initial value (if possible)") {
    forAll { (totalCopies: Int) =>
      whenever(totalCopies > 0) {
        val book = Book(
          id = Id("p3"),
          title = "Prop Test",
          author = List("Author"),
          isbn = Id("isbn"),
          publishedDate = LocalDate.of(2023, 1, 1),
          genre = "Test",
          totalCopies = totalCopies,
          availableCopies = totalCopies
        )

        val borrowed = book.borrowCopy.getOrElse(book)
        val restored = borrowed.returnCopy.getOrElse(borrowed)

        restored.availableCopies shouldBe book.availableCopies
      }
    }
  }

  test("hasNCommonAuthor should return count of intersecting authors") {
    forAll { (authors1: List[String], authors2: List[String]) =>
      val book1 = Book(
        id = Id("p4"),
        title = "Test",
        author = authors1.distinct,
        isbn = Id("isbn1"),
        publishedDate = LocalDate.of(2023, 1, 1),
        genre = "Test",
        totalCopies = 1,
        availableCopies = 1
      )

      val book2 = Book(
        id = Id("p5"),
        title = "Test",
        author = authors2.distinct,
        isbn = Id("isbn2"),
        publishedDate = LocalDate.of(2023, 1, 1),
        genre = "Test",
        totalCopies = 1,
        availableCopies = 1
      )

      val commonCount = authors1.distinct.count(authors2.distinct.contains)
      book1.hasNCommonAuthor(book2) shouldBe commonCount
    }
  }
}
