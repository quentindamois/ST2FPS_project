package utils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import utils.UserInput.buildBook

import java.time.LocalDate

class UserInputSpec extends AnyFunSuite with Matchers {
  test("buildBook should Return a book") {
    val listOfParam: List[(String, Any)] = List(
      ("title", "Test Book"),
      ("author", List[String]("Test Author")),
      ("isbn", "978-9-9655-7878-9"),
      ("publishedDate", LocalDate.of(2023, 1, 1)),
      ("genre", "Test"),
      ("totalCopies", 3),
      ("description", Option[String]("ff"))
    )
    val result = buildBook(listOfParam)
    result shouldBe a[Right[_, _]]
  }
  
  
}
