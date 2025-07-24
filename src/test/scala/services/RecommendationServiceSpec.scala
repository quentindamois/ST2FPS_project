package services

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import models.*
import utils.Id

import java.time.LocalDate

class RecommendationServiceSpec extends AnyFunSuite with Matchers {

  def makeBook(idSuffix: String, genre: String, author: String, year: Int): Book = Book(
    id = Id(s"book$idSuffix"),
    title = s"Book $idSuffix",
    author = List(author),
    isbn = Id(s"00000-$idSuffix"),
    publishedDate = LocalDate.of(year, 1, 1),
    genre = genre,
    totalCopies = 2,
    availableCopies = 2
  )

  def makeUser(idSuffix: String): User = User(
    id = Id(s"user$idSuffix"),
    firstName = s"User $idSuffix",
    lastName = "RIAH",
    email = s"user$idSuffix@efrei.net",
    membershipDate = LocalDate.now(),
    userType = UserType.Student
  )


  test("Already borrowed books should not be recommended to the user") {
    val book1 = makeBook("A", "Fantastic", "Dabos", 2020)
    val book2 = makeBook("B", "Polar", "Liggett", 2021)
    val book3 = makeBook("C", "Science-Fiction", "Sherman", 2019)
    val book4 = makeBook("D", "History", "Miller", 2022)
    val book5 = makeBook("E", "BD", "Loisel", 2018)
    val user = makeUser("1")

    val initialService = LibraryService()
    val service1 = initialService.addBook(book1).getOrElse(initialService)
    val service2 = service1.addBook(book2).getOrElse(service1)
    val service3 = service2.addBook(book3).getOrElse(service2)
    val service4 = service3.addBook(book4).getOrElse(service3)
    val service5 = service4.addBook(book5).getOrElse(service4)
    val service6 = service5.addUser(user).getOrElse(service5)

    val (serviceWithBorrow1, _) = service6.borrowBook(user.id, book1.id).getOrElse((service6, null))
    val (serviceWithBorrow2, _) = serviceWithBorrow1.borrowBook(user.id, book2.id).getOrElse((serviceWithBorrow1, null))

    val recommender = RecommendationService(serviceWithBorrow2)
    val recommendationsGenre = recommender.recommendBooksByGenre(user.id, 2)
    val recommendationsAuthor = recommender.recommendBooksByAuthor(user.id, 2)
    val recommendationsAuthorandGenre = recommender.getSimilarBooks(user.id, 2)

    recommendationsGenre.map(_.id).contains(book1.id) shouldBe false
    recommendationsGenre.map(_.id).contains(book2.id) shouldBe false

    recommendationsAuthor.map(_.id).contains(book1.id) shouldBe false
    recommendationsAuthor.map(_.id).contains(book2.id) shouldBe false

    recommendationsAuthorandGenre.map(_.id).contains(book1.id) shouldBe false
    recommendationsAuthorandGenre.map(_.id).contains(book2.id) shouldBe false
  }

  test("Most popular books should be recommended in the absence of information about the user preference") {
    val book1 = makeBook("A", "Romance", "Gorman", 2020)
    val book2 = makeBook("B", "Fantastic", "Yancey", 2021)
    val book3 = makeBook("C", "Fantastic", "Roth", 2019)
    val user1 = makeUser("1")
    val user2 = makeUser("2")

    val initialService = LibraryService()
    val service1 = initialService.addBook(book1).getOrElse(initialService)
    val service2 = service1.addBook(book2).getOrElse(service1)
    val service3 = service2.addBook(book3).getOrElse(service2)
    val service4 = service3.addUser(user1).getOrElse(service3)
    val service5 = service4.addUser(user2).getOrElse(service4)

    val (serviceAfterBorrow1, _) = service5.borrowBook(user1.id, book1.id).getOrElse((service5, null))
    val (serviceAfterBorrow2, _) = serviceAfterBorrow1.borrowBook(user1.id, book2.id).getOrElse((serviceAfterBorrow1, null))

    val recommender = RecommendationService(serviceAfterBorrow2)
    val recommendations = recommender.recommendBooksByGenre(user2.id, 2)

    recommendations.take(2).map(_.id).toSet shouldBe Set(book1.id, book2.id)
  }

  test("Books with the same genre should be recommended") {
    val book1 = makeBook("A", "Fantastique", "Rowling", 2000)
    val book2 = makeBook("B", "Dystopic", "Collins", 1999)
    val book3 = makeBook("C", "BD", "Uderzo", 2012)
    val book4 = makeBook("D", "Fantastique", "Wolf", 1993)
    val user = makeUser("1")

    val initialService = LibraryService()
    val service1 = initialService.addBook(book1).getOrElse(initialService)
    val service2 = service1.addBook(book2).getOrElse(service1)
    val service3 = service2.addBook(book3).getOrElse(service2)
    val service4 = service3.addBook(book4).getOrElse(service3)
    val service5 = service4.addUser(user).getOrElse(service4)

    val (withBorrow, _) = service5.borrowBook(user.id, book1.id).getOrElse((service5, null))

    val recommender = RecommendationService(withBorrow)
    val recommendations = recommender.recommendBooksByGenre(user.id, 1)

    recommendations.headOption.map(_.genre) shouldBe Some("Fantastique")

  }


  test("Books with the same author should be recommended") {
    val book1 = makeBook("A", "Romance", "Black", 1997)
    val book2 = makeBook("B", "Fantastique", "Lewis", 2002)
    val book3 = makeBook("C", "Romance", "Black", 2012)
    val user = makeUser("1")

    val initialService = LibraryService()
    val service1 = initialService.addBook(book1).getOrElse(initialService)
    val service2 = service1.addBook(book2).getOrElse(service1)
    val service3 = service2.addBook(book3).getOrElse(service2)
    val service4 = service3.addUser(user).getOrElse(service3)

    val (withBorrow, _) = service4.borrowBook(user.id, book1.id).getOrElse((service4, null))

    val recommender = RecommendationService(withBorrow)
    val recommendations = recommender.recommendBooksByAuthor(user.id, 1)

    recommendations.headOption.map(_.author) shouldBe Some(List("Black"))
  }

  test("Recommandation should return an empty list if the user or the book do not exist") {
    val service = LibraryService()
    val recommender = RecommendationService(service)
    recommender.recommendBooksByGenre(Id("inexistant"), 3) shouldBe empty
    recommender.getSimilarBooks(Id("inexistant"), 2) shouldBe empty
  }

  test("Books with the same genre and author should be recommended") {
    val book1 = makeBook("A", "Science-fiction", "Lovecraft", 1971)
    val book2 = makeBook("B", "Philosophy", "Plath", 1999)
    val book3 = makeBook("C", "Science-fiction", "Lovecraft", 2010)
    val book4 = makeBook("D", "Romance", "J. Maas", 1991)

    val initialService = LibraryService()
    val service1 = initialService.addBook(book1).getOrElse(initialService)
    val service2 = service1.addBook(book2).getOrElse(service1)
    val service3 = service2.addBook(book3).getOrElse(service2)
    val service4 = service3.addBook(book4).getOrElse(service3)

    val recommender = RecommendationService(service4)
    val similars = recommender.getSimilarBooks(book1.id, 3)

    similars.exists(b => b.genre==book1.genre || b.author==book1.author) shouldBe true
    similars.map(_.id).contains(book1.id) shouldBe false
  }
}