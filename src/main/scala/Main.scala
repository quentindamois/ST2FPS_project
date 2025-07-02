import models.*
import services.*
import utils.JsonUtil
import java.time.LocalDate
import java.util.UUID

/** Application principale du système de gestion de bibliothèque
  */
object Main extends App {

  println("=== Système de Gestion de Bibliothèque ===")
  println("Bienvenue dans le système de gestion de bibliothèque ST2FPS!")

  // Initialisation du service
  val libraryService = new LibraryService()
  val recommendationService = new RecommendationService(libraryService)

  // Démonstration du système
  runDemo()

  def runDemo(): Unit = {
    println("\n--- Ajout de livres au catalogue ---")

    // Création de quelques livres d'exemple
    val books = List(
      Book(
        id = UUID.randomUUID().toString,
        title = "Le Petit Prince",
        author = "Antoine de Saint-Exupéry",
        isbn = "978-2-07-040848-4",
        publishedDate = LocalDate.of(1943, 4, 6),
        genre = "Littérature",
        totalCopies = 5,
        availableCopies = 5,
        description = Some("Un conte philosophique et poétique")
      ),
      Book(
        id = UUID.randomUUID().toString,
        title = "1984",
        author = "George Orwell",
        isbn = "978-0-452-28423-4",
        publishedDate = LocalDate.of(1949, 6, 8),
        genre = "Science-fiction",
        totalCopies = 3,
        availableCopies = 3,
        description = Some("Un roman dystopique sur la surveillance")
      ),
      Book(
        id = UUID.randomUUID().toString,
        title = "Scala Programming",
        author = "Martin Odersky",
        isbn = "978-0-981531-64-7",
        publishedDate = LocalDate.of(2008, 1, 1),
        genre = "Informatique",
        totalCopies = 2,
        availableCopies = 2,
        description = Some("Guide de programmation en Scala")
      )
    )

    // Ajout des livres
    books.foreach { book =>
      libraryService.addBook(book) match {
        case Right(_)    => println(s"✓ Livre ajouté: ${book.title}")
        case Left(error) => println(s"✗ Erreur: ${error.message}")
      }
    }

    println("\n--- Ajout d'utilisateurs ---")

    // Création d'utilisateurs
    val users = List(
      User(
        id = UUID.randomUUID().toString,
        firstName = "Marie",
        lastName = "Dupont",
        email = "marie.dupont@email.com",
        membershipDate = LocalDate.now().minusDays(30),
        userType = UserType.Student
      ),
      User(
        id = UUID.randomUUID().toString,
        firstName = "Jean",
        lastName = "Martin",
        email = "jean.martin@email.com",
        membershipDate = LocalDate.now().minusDays(15),
        userType = UserType.Faculty
      )
    )

    // Ajout des utilisateurs
    users.foreach { user =>
      libraryService.addUser(user) match {
        case Right(_)    => println(s"✓ Utilisateur ajouté: ${user.fullName}")
        case Left(error) => println(s"✗ Erreur: ${error.message}")
      }
    }

    println("\n--- Recherche de livres ---")

    // Recherche par titre
    libraryService.searchBooks("Prince", SearchType.Title) match {
      case Right(foundBooks) =>
        println(
          s"Livres trouvés avec 'Prince': ${foundBooks.map(_.title).mkString(", ")}"
        )
      case Left(error) => println(s"Erreur de recherche: ${error.message}")
    }

    // Recherche par auteur
    libraryService.searchBooks("Orwell", SearchType.Author) match {
      case Right(foundBooks) =>
        println(s"Livres de Orwell: ${foundBooks.map(_.title).mkString(", ")}")
      case Left(error) => println(s"Erreur de recherche: ${error.message}")
    }

    println("\n--- Opérations d'emprunt ---")

    val catalog = libraryService.getCatalog
    if (catalog.books.nonEmpty && catalog.users.nonEmpty) {
      val firstBook = catalog.books.values.head
      val firstUser = catalog.users.values.head

      // Emprunt d'un livre
      libraryService.borrowBook(firstUser.id, firstBook.id) match {
        case Right(transaction) =>
          println(s"✓ ${firstUser.fullName} a emprunté '${firstBook.title}'")
          println(s"  Transaction ID: ${transaction.id}")
          println(
            s"  Date d'échéance: ${transaction.dueDate.getOrElse("Non définie")}"
          )
        case Left(error) =>
          println(s"✗ Erreur d'emprunt: ${error.message}")
      }

      // Affichage des livres empruntés par l'utilisateur
      libraryService.getUserBorrowedBooks(firstUser.id) match {
        case Right(borrowedBooks) =>
          if (borrowedBooks.nonEmpty) {
            println(s"Livres empruntés par ${firstUser.fullName}:")
            borrowedBooks.foreach(book => println(s"  - ${book.title}"))
          }
        case Left(error) => println(s"Erreur: ${error.message}")
      }
    }

    println("\n--- Recommandations ---")

    if (catalog.users.nonEmpty) {
      val firstUser = catalog.users.values.head
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
    }

    println("\n--- Sauvegarde du catalogue ---")

    // Création du dossier data s'il n'existe pas
    val dataDir = new java.io.File("data")
    if (!dataDir.exists()) {
      dataDir.mkdirs()
    }

    // Sauvegarde en JSON
    JsonUtil.saveToFile(catalog, "data/catalog.json") match {
      case Right(_) => println("✓ Catalogue sauvegardé dans data/catalog.json")
      case Left(error) => println(s"✗ Erreur de sauvegarde: $error")
    }

    println("\n--- Statistiques ---")
    println(s"Total des livres: ${catalog.totalBooks}")
    println(s"Total des utilisateurs: ${catalog.totalUsers}")
    println(s"Total des transactions: ${catalog.totalTransactions}")
    println(s"Livres disponibles: ${catalog.availableBooks.length}")

    println("\n=== Fin de la démonstration ===")
  }
}
