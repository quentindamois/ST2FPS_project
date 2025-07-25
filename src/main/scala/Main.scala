package main

import models.*
import services.*
import utils.{Id, JsonUtil}

import java.time.LocalDate
import java.util.UUID
import scala.io.StdIn.readLine

/**
  * The Main application, it has to be executed to use the code
  */
object Main extends App {

  println("=== Management system of the Library ===")
  println("Welcome to the management system of the library")
  println("To enter command enter 1 to run a demo enter 2")
  print(">")
  var valueEntered: String = readLine()
  while (valueEntered != "1" & valueEntered != "2") {
    println("To enter command enter 1 to run a demo enter 2")
    print(">")
    valueEntered = readLine()
  }
  // Initialisation of the service services (immutables)
  valueEntered match {
    case "1" => {
      ManualControl.mainLoop()
    }
    case "2" => {
      val initialLibraryService = LibraryService()
      val initialRecommendationService = RecommendationService(
        initialLibraryService
      )


      // Demonstration of the systems
      runDemo(initialLibraryService)
    }
  }
  /**
   * This function is used to a Demo showing the programming function
   * */
  def runDemo(libraryService: LibraryService): Unit = {
    println("\n--- Adding book to the catalog ---")

    // Creating book for example
    val books = List(
      Book(
        id = Id(UUID.randomUUID().toString),
        title = "Le Petit Prince",
        author = List[String]("Antoine de Saint-Exupéry"),
        isbn = Id("978-2-07-040848-4"),
        publishedDate = LocalDate.of(1943, 4, 6),
        genre = "Literature",
        totalCopies = 5,
        availableCopies = 5,
        description = Some("A poetic fairy tale.")
      ),
      Book(
        id = Id(UUID.randomUUID().toString),
        title = "1984",
        author = List[String]("George Orwell"),
        isbn = Id("978-0-452-28423-4"),
        publishedDate = LocalDate.of(1949, 6, 8),
        genre = "Science-fiction",
        totalCopies = 3,
        availableCopies = 3,
        description = Some("A dystopic novel about mass surveillance.")
      ),
      Book(
        id = Id(UUID.randomUUID().toString),
        title = "Scala Programming",
        author = List[String]("Martin Odersky"),
        isbn = Id("978-0-981531-64-7"),
        publishedDate = LocalDate.of(2008, 1, 1),
        genre = "Informatics",
        totalCopies = 2,
        availableCopies = 2,
        description = Some("Coding guide on scala.")
      )
    )

    // Adding book to show object immutability
    val serviceWithBooks = books.foldLeft(libraryService) { (service, book) =>
      service.addBook(book) match {
        case Right(updatedService) =>
          println(s"[OK] Book added: ${book.title}")
          updatedService
        case Left(error) =>
          println(s"[ERROR] Error: ${error.message}")
          service
      }
    }

    println("\n--- Adding users ---")

    // Creating user
    val users = List(
      User(
        id = Id(UUID.randomUUID().toString),
        firstName = "Marie",
        lastName = "Dupont",
        email = "marie.dupont@email.com",
        membershipDate = LocalDate.now().minusDays(30),
        userType = UserType.Student
      ),
      User(
        id = Id(UUID.randomUUID().toString),
        firstName = "Jean",
        lastName = "Martin",
        email = "jean.martin@email.com",
        membershipDate = LocalDate.now().minusDays(15),
        userType = UserType.Faculty
      )
    )

    // Adding users to show object immutability
    val serviceWithUsers = users.foldLeft(serviceWithBooks) { (service, user) =>
      service.addUser(user) match {
        case Right(updatedService) =>
          println(s"[OK] User Added: ${user.fullName}")
          updatedService
        case Left(error) =>
          println(s"[ERROR] Error: ${error.message}")
          service
      }
    }

    println("\n--- Books Research ---")

    // Research by title
    serviceWithUsers.searchBooks("Prince", SearchType.Title) match {
      case Right(foundBooks) =>
        println(
          s"The books with 'Prince' in their titles: ${foundBooks.map(_.title).mkString(", ")}"
        )
      case Left(error) => println(s"Encountered an error when searching: ${error.message}")
    }

    // Research by author
    serviceWithUsers.searchBooks("Orwell", SearchType.Author) match {
      case Right(foundBooks) =>
        println(s"Orwell's books: ${foundBooks.map(_.title).mkString(", ")}")
      case Left(error) => println(s"Encountered an error when searching: ${error.message}")
    }

    println("\n--- Borrowing Books ---")

    val catalog = serviceWithUsers.getCatalog
    if (catalog.books.nonEmpty && catalog.users.nonEmpty) {
      val firstBook = catalog.books.values.head
      val firstUser = catalog.users.values.head

      // Showing borrowing with immutability
      serviceWithUsers.borrowBook(firstUser.id, firstBook.id) match {
        case Right((updatedService, transaction)) =>
          println(s"[OK] ${firstUser.fullName} has borrowed '${firstBook.title}'")
          println(s"  Transaction ID: ${transaction.id}")
          println(
            s"  Due Date: ${transaction.dueDate.getOrElse("Not Defined")}"
          )

          // Displaying the book borrowed by the user
          updatedService.getUserBorrowedBooks(firstUser.id) match {
            case Right(borrowedBooks) =>
              if (borrowedBooks.nonEmpty) {
                println(s"The books borrowed by ${firstUser.fullName}:")
                borrowedBooks.foreach(book => println(s"  - ${book.title}"))
              }
            case Left(error) => println(s"Error: ${error.message}")
          }

          val recommendationService = RecommendationService(updatedService)
          val recommendations =
            recommendationService.recommendBooksByGenre(firstUser.id, 3)

          if (recommendations.nonEmpty) {
            println(s"Recommendation for  ${firstUser.fullName}:")
            recommendations.foreach(book =>
              println(s"  - ${book.title} (${book.genre})")
            )
          } else {
            println("No recommendation are available at the moment.")
          }

        case Left(error) =>
          println(s"[ERROR] Encountered an error when borrowing a book: ${error.message}")
      }
    }

    println("\n--- Saving a library's catalog ---")

    // saving in JSON
    JsonUtil.saveToFile(
      serviceWithUsers.getCatalog,
      "data/catalog.json"
    ) match {
      case Right(_) =>
        println("[OK] The catalog was saved inside of data/catalog.json")
      case Left(error) => println(s"[ERROR] Encountered an error when saving: $error")
    }

    println("\n-- Loading a library's catalog ---")
    // Loading from a JSON
    JsonUtil.LoadFromFile(
      "data/catalog.json"
    ) match {
      case Right(contentFile) =>
        println(s"[OK] This is the successfully load Library's catalof\n$contentFile")
      case Left(error) => println(s"[ERROR] Encountered an error when saving: $error")
    }

    val finalCatalog = serviceWithUsers.getCatalog
    println("\n--- Statistic ---")
    println(s"Total number of book: ${finalCatalog.totalBooks}")
    println(s"total number of user: ${finalCatalog.totalUsers}")
    println(s"Total number of transaction (both borrowing and returning): ${finalCatalog.totalTransactions}")
    println(s"Available books: ${finalCatalog.availableBooks.length}")

    println("\n=== Demonstration's end ===")
  }
}
