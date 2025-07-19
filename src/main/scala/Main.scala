package main

import models.*
import services.*
import utils.{Id, JsonUtil}

import java.time.LocalDate
import java.util.UUID

/** Application principale du système de gestion de bibliothèque Implémentation
  * conforme aux principes de la programmation fonctionnelle
  */
object Main extends App {

  println("=== Systeme de Gestion de Bibliotheque ===")
  println("Bienvenue dans le systeme de gestion de bibliotheque ST2FPS!")

  // Initialisation des services (immutables)
  val initialLibraryService = LibraryService()
  val initialRecommendationService = RecommendationService(
    initialLibraryService
  )

  // Démonstration du système
  runDemo(initialLibraryService)

  def runDemo(libraryService: LibraryService): Unit = {
    println("\n--- Ajout de livres au catalogue ---")

    // Création de quelques livres d'exemple
    val books = List(
      Book(
        id = Id(UUID.randomUUID().toString),
        title = "Le Petit Prince",
        author = "Antoine de Saint-Exupéry",
        isbn = Id("978-2-07-040848-4"),
        publishedDate = LocalDate.of(1943, 4, 6),
        genre = "Littérature",
        totalCopies = 5,
        availableCopies = 5,
        description = Some("Un conte philosophique et poétique")
      ),
      Book(
        id = Id(UUID.randomUUID().toString),
        title = "1984",
        author = "George Orwell",
        isbn = Id("978-0-452-28423-4"),
        publishedDate = LocalDate.of(1949, 6, 8),
        genre = "Science-fiction",
        totalCopies = 3,
        availableCopies = 3,
        description = Some("Un roman dystopique sur la surveillance")
      ),
      Book(
        id = Id(UUID.randomUUID().toString),
        title = "Scala Programming",
        author = "Martin Odersky",
        isbn = Id("978-0-981531-64-7"),
        publishedDate = LocalDate.of(2008, 1, 1),
        genre = "Informatique",
        totalCopies = 2,
        availableCopies = 2,
        description = Some("Guide de programmation en Scala")
      )
    )

    // Ajout des livres avec gestion immutable
    val serviceWithBooks = books.foldLeft(libraryService) { (service, book) =>
      service.addBook(book) match {
        case Right(updatedService) =>
          println(s"[OK] Livre ajoute: ${book.title}")
          updatedService
        case Left(error) =>
          println(s"[ERREUR] Erreur: ${error.message}")
          service
      }
    }

    println("\n--- Ajout d'utilisateurs ---")

    // Création d'utilisateurs
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

    // Ajout des utilisateurs avec gestion immutable
    val serviceWithUsers = users.foldLeft(serviceWithBooks) { (service, user) =>
      service.addUser(user) match {
        case Right(updatedService) =>
          println(s"[OK] Utilisateur ajoute: ${user.fullName}")
          updatedService
        case Left(error) =>
          println(s"[ERREUR] Erreur: ${error.message}")
          service
      }
    }

    println("\n--- Recherche de livres ---")

    // Recherche par titre
    serviceWithUsers.searchBooks("Prince", SearchType.Title) match {
      case Right(foundBooks) =>
        println(
          s"Livres trouves avec 'Prince': ${foundBooks.map(_.title).mkString(", ")}"
        )
      case Left(error) => println() //println(s"Erreur de recherche: ${error.message}")
    }

    // Recherche par auteur
    serviceWithUsers.searchBooks("Orwell", SearchType.Author) match {
      case Right(foundBooks) =>
        println(s"Livres de Orwell: ${foundBooks.map(_.title).mkString(", ")}")
      case Left(error) => println()//println(s"Erreur de recherche: ${error.message}")
    }

    println("\n--- Operations d'emprunt ---")

    val catalog = serviceWithUsers.getCatalog
    if (catalog.books.nonEmpty && catalog.users.nonEmpty) {
      val firstBook = catalog.books.values.head
      val firstUser = catalog.users.values.head

      // Emprunt d'un livre avec gestion immutable
      serviceWithUsers.borrowBook(firstUser.id, firstBook.id) match {
        case Right((updatedService, transaction)) =>
          println(s"[OK] ${firstUser.fullName} a emprunte '${firstBook.title}'")
          println(s"  Transaction ID: ${transaction.id}")
          println(
            s"  Date d'echeance: ${transaction.dueDate.getOrElse("Non definie")}"
          )

          // Affichage des livres empruntés par l'utilisateur
          updatedService.getUserBorrowedBooks(firstUser.id) match {
            case Right(borrowedBooks) =>
              if (borrowedBooks.nonEmpty) {
                println(s"Livres empruntes par ${firstUser.fullName}:")
                borrowedBooks.foreach(book => println(s"  - ${book.title}"))
              }
            case Left(error) => println(s"Erreur: ${error.message}")
          }

          // Recommandations avec le service mis à jour
          val recommendationService = RecommendationService(updatedService)
          val recommendations =
            recommendationService.recommendBooksByGenre(firstUser.id, 3)

          if (recommendations.nonEmpty) {
            println(s"Recommandations pour ${firstUser.fullName}:")
            recommendations.foreach(book =>
              println(s"  - ${book.title} (${book.genre})")
            )
          } else {
            println("Aucune recommandation disponible pour le moment.")
          }

        case Left(error) =>
          println(s"[ERREUR] Erreur d'emprunt: ${error.message}")
      }
    }

    println("\n--- Sauvegarde du catalogue ---")

    // Sauvegarde en JSON
    JsonUtil.saveToFile(
      serviceWithUsers.getCatalog,
      "data/catalog.json"
    ) match {
      case Right(_) =>
        println("[OK] Catalogue sauvegarde dans data/catalog.json")
      case Left(error) => println(s"[ERREUR] Erreur de sauvegarde: $error")
    }

    val finalCatalog = serviceWithUsers.getCatalog
    println("\n--- Statistiques ---")
    println(s"Total des livres: ${finalCatalog.totalBooks}")
    println(s"Total des utilisateurs: ${finalCatalog.totalUsers}")
    println(s"Total des transactions: ${finalCatalog.totalTransactions}")
    println(s"Livres disponibles: ${finalCatalog.availableBooks.length}")

    println("\n=== Fin de la demonstration ===")
  }
}
