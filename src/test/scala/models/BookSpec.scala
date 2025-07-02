package models

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.time.LocalDate

class BookSpec extends AnyFunSuite with Matchers {
  
  test("un livre disponible devrait permettre l'emprunt") {
    val book = Book(
      id = "1",
      title = "Test Book",
      author = "Test Author",
      isbn = "978-0-123456-78-9",
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
  
  test("un livre sans exemplaire disponible ne devrait pas permettre l'emprunt") {
    val book = Book(
      id = "1",
      title = "Test Book",
      author = "Test Author",
      isbn = "978-0-123456-78-9",
      publishedDate = LocalDate.of(2023, 1, 1),
      genre = "Test",
      totalCopies = 1,
      availableCopies = 0
    )
    
    book.isAvailable shouldBe false
    
    val result = book.borrowCopy
    result shouldBe a[Left[_, _]]
  }
  
  test("le retour d'un livre devrait augmenter les exemplaires disponibles") {
    val book = Book(
      id = "1",
      title = "Test Book",
      author = "Test Author",
      isbn = "978-0-123456-78-9",
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
