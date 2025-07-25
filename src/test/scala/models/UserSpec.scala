package models

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import java.time.LocalDate
import utils.Id

class UserSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  val testUser: User = User(
    id = Id("1"),
    firstName = "Theo",
    lastName = "Billionka",
    email = "theo.billionka@gmail.com",
    membershipDate = LocalDate.of(2024, 1, 1),
    userType = UserType.Student
  )

  test("User full name should be correctly formatted") {
    testUser.fullName shouldBe "Theo Billionka"
  }

  test("New user should be able to borrow books") {
    testUser.canBorrow shouldBe true
    testUser.borrowedBooks.length shouldBe 0
  }

  test("User should be able to borrow a book when under limit") {
    val bookId = Id("book1")
    val result = testUser.borrowBook(bookId)

    result shouldBe a[Right[_, _]]
    result.getOrElse(testUser).borrowedBooks should contain(bookId)
  }

  test("User should not be able to borrow the same book twice") {
    val bookId = Id("book1")
    val userWithBook = testUser.borrowBook(bookId).getOrElse(testUser)
    val result = userWithBook.borrowBook(bookId)

    result shouldBe a[Left[_, _]]
    result.left.getOrElse("") should include("déjà emprunté")
  }

  test("User should not be able to borrow books when at limit") {
    val books = (1 to 5).map(i => Id(s"book$i"))
    val userWithMaxBooks = books.foldLeft(testUser) { (u, bookId) =>
      u.borrowBook(bookId).getOrElse(u)
    }

    userWithMaxBooks.canBorrow shouldBe false
    val result = userWithMaxBooks.borrowBook(Id("book6"))
    result shouldBe a[Left[_, _]]
    result.left.getOrElse("") should include("Limite d'emprunt atteinte")
  }

  test("User should be able to return a borrowed book") {
    val bookId = Id("book1")
    val userWithBook = testUser.borrowBook(bookId).getOrElse(testUser)
    val result = userWithBook.returnBook(bookId)

    result shouldBe a[Right[_, _]]
    result.getOrElse(userWithBook).borrowedBooks should not contain bookId
  }

  test("User should not be able to return a book they haven't borrowed") {
    val bookId = Id("nonBorrowedBook")
    val result = testUser.returnBook(bookId)

    result shouldBe a[Left[_, _]]
    result.left.getOrElse("") should include("n'est pas emprunté")
  }

  test("User should maintain correct borrowed books count") {
    val book1 = Id("book1")
    val book2 = Id("book2")

    val userWithBooks = testUser
      .borrowBook(book1)
      .flatMap(_.borrowBook(book2))
      .getOrElse(testUser)

    userWithBooks.borrowedBooks should contain allOf (book1, book2)
    userWithBooks.borrowedBooks.length shouldBe 2
  }

  test("Borrowed books list should remain unchanged on failed borrow attempt") {
    val bookId = Id("book1")
    val userWithBook = testUser.borrowBook(bookId).getOrElse(testUser)
    val failedAttempt = userWithBook.borrowBook(bookId)

    failedAttempt.left.getOrElse("") should include("déjà emprunté")
    failedAttempt.fold(
      _ => userWithBook.borrowedBooks should contain only bookId,
      _ => fail("Borrow should have failed")
    )
  }

  test("Returning a book should not affect other borrowed books") {
    val book1 = Id("book1")
    val book2 = Id("book2")
    val userWithBooks = testUser
      .borrowBook(book1)
      .flatMap(_.borrowBook(book2))
      .getOrElse(testUser)

    val userAfterReturn = userWithBooks.returnBook(book1).getOrElse(userWithBooks)

    userAfterReturn.borrowedBooks should contain only book2
  }
  
  test("Borrowing should never exceed maxBorrowLimit") {
    forAll { (ids: List[String]) =>
      val uniqueIds = ids.distinct.take(10) // éviter doublons
      val userAfterBorrows = uniqueIds.foldLeft(testUser) { (user, rawId) =>
        user.borrowBook(Id(rawId)).getOrElse(user)
      }
      userAfterBorrows.borrowedBooks.length should be <= testUser.maxBorrowLimit
    }
  }

  test("Returning a borrowed book should reduce borrowedBooks size by 1") {
    forAll { (bookId: String) =>
      whenever(bookId.nonEmpty) {
        val borrowed = testUser.borrowBook(Id(bookId)).getOrElse(testUser)
        val returned = borrowed.returnBook(Id(bookId)).getOrElse(borrowed)
        returned.borrowedBooks.size shouldBe borrowed.borrowedBooks.size - 1
      }
    }
  }

  test("Borrow then return a book should restore initial state") {
    forAll { (bookId: String) =>
      whenever(bookId.nonEmpty) {
        val borrowed = testUser.borrowBook(Id(bookId)).getOrElse(testUser)
        val restored = borrowed.returnBook(Id(bookId)).getOrElse(borrowed)
        restored.borrowedBooks shouldBe empty
      }
    }
  }

  test("Borrowing same book twice should always fail second time") {
    forAll { (bookId: String) =>
      whenever(bookId.nonEmpty) {
        val userWithBook = testUser.borrowBook(Id(bookId)).getOrElse(testUser)
        val secondAttempt = userWithBook.borrowBook(Id(bookId))
        secondAttempt shouldBe a[Left[_, _]]
      }
    }
  }
}
