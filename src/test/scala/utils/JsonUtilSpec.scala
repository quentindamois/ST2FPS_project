package utils

import models._
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import java.time.LocalDate
import utils.ErrorHandling.FileError

class JsonUtilSpec extends AsyncFlatSpec with Matchers with BeforeAndAfter {

  val testDate: LocalDate = LocalDate.of(2024, 1, 1)

  val sampleLib: LibCatalog = LibCatalog(
    books = Map(
      Id("b1") -> Book(
        id = Id("b1"),
        title = "Scala Book",
        author = "Maxence Kamionka", // À changer quand on aura une liste
        isbn = Id("isbn1"),
        publishedDate = testDate,
        genre = "Programming",
        totalCopies = 1,
        availableCopies = 1
      )
    ),
    users = Map(
      Id("u1") -> User(
        id = Id("u1"),
        firstName = "Alice",
        lastName = "Smith",
        email = "alice@example.com",
        membershipDate = testDate,
        userType = UserType.Student
      )
    ),
    transactions = List()
  )

  val testFilePath = "test_output/lib_test.json"
  val invalidPath = "invalid/path/test.json"

  before {
    val dir = new File("test_output")
    if (!dir.exists()) dir.mkdir()
    val f = new File(testFilePath)
    if (f.exists()) f.delete()
  }

  after {
    // Supprimer le fichier principal s'il existe
    val f = new File(testFilePath)
    if (f.exists()) f.delete()

    // Supprimer le dossier test_output s'il est vide
    val testOutputDir = new File("test_output")
    if (testOutputDir.exists() && testOutputDir.isDirectory && testOutputDir.list().isEmpty)
      testOutputDir.delete()

    // Supprimer récursivement le dossier "invalid" créé par safeFileOpening
    def deleteRecursively(file: File): Unit = {
      if (file.isDirectory) {
        file.listFiles().foreach(deleteRecursively)
      }
      file.delete()
    }

    val invalidPathRoot = new File("invalid")
    if (invalidPathRoot.exists()) {
      deleteRecursively(invalidPathRoot)
    }

    // Supprimer le fichier "nonexistent.json" s'il a été créé par erreur
    val nonExistent = new File("nonexistent.json")
    if (nonExistent.exists()) nonExistent.delete()
  }


  behavior of "JsonUtil.safeLibConvertingToJson"

  it should "successfully convert a LibCatalog to JSON string" in {
    JsonUtil.safeLibConvertingToJson(sampleLib) match {
      case Right(jsonStr) =>
        jsonStr should include("Scala Book")
        succeed
      case Left(error) =>
        fail(s"Expected successful JSON conversion but got error: $error")
    }
  }

  it should "handle empty LibCatalog correctly" in {
    val emptyLib = LibCatalog(Map(), Map(), List())
    JsonUtil.safeLibConvertingToJson(emptyLib) match {
      case Right(jsonStr) =>
        jsonStr should include("{}")
        succeed
      case Left(error) =>
        fail(s"Expected empty JSON conversion but got error: $error")
    }
  }

  behavior of "JsonUtil.saveToFile"

  it should "save a LibCatalog to a JSON file" in {
    JsonUtil.saveToFile(sampleLib, testFilePath) match {
      case Right(msg) =>
        msg should include("Success")
        new File(testFilePath).exists() shouldBe true
        succeed
      case Left(error) =>
        fail(s"Expected file to be saved successfully but got error: $error")
    }
  }

  behavior of "JsonUtil.LoadFromFile"

  it should "load a LibCatalog from a JSON file" in {
    JsonUtil.saveToFile(sampleLib, testFilePath)

    JsonUtil.LoadFromFile(testFilePath) match {
      case Right(loadedLib) =>
        loadedLib.books(Id("b1")).title shouldEqual "Scala Book"
        loadedLib.books(Id("b1")).genre shouldEqual "Programming"
        loadedLib.users(Id("u1")).firstName shouldEqual "Alice"
        succeed
      case Left(error) =>
        fail(s"Expected successful load but got error: $error")
    }
  }

  it should "handle non-existent file" in {
    JsonUtil.LoadFromFile("nonexistent.json") match {
      case Left(_) => succeed
      case Right(_) => fail("Expected failure when loading nonexistent file")
    }
  }

  behavior of "JsonUtil.safeFileOpening"

  it should "open an existing file" in {
    val result = JsonUtil.safeFileOpening(testFilePath)
    result.isRight shouldBe true
    succeed
  }

  it should "handle invalid paths" in {
    val result = JsonUtil.safeFileOpening(invalidPath)
    result.isRight shouldBe true // Il crée le chemin si nécessaire
    succeed
  }

  behavior of "JsonUtil.safeFileReading and safeFileWriting"

  it should "write and read content correctly" in {
    val content = """{"test":"content"}"""
    val file = new File(testFilePath)

    JsonUtil.safeFileWriting(file, content) match {
      case Right(_) =>
        JsonUtil.safeFileReading(file) match {
          case Right(readContent) =>
            readContent shouldEqual content
            succeed
          case Left(err) => fail(s"Reading failed: $err")
        }
      case Left(err) =>
        fail(s"Writing failed: $err")
    }
  }

  it should "handle reading from non-existent file" in {
    val nonExistentFile = new File("nonexistent.json")
    JsonUtil.safeFileReading(nonExistentFile) match {
      case Left(_: FileError.ReadingError) => succeed
      case _ => fail("Expected ReadingError for nonexistent file")
    }
  }

  behavior of "JsonUtil.safeJsonConvertingToLib"

  it should "convert valid JSON to LibCatalog" in {
    JsonUtil.safeLibConvertingToJson(sampleLib) match {
      case Right(jsonContent) =>
        JsonUtil.safeJsonConvertingToLib(jsonContent) match {
          case Right(lib) =>
            lib.books.size shouldEqual 1
            succeed
          case Left(error) =>
            fail(s"Expected valid LibCatalog but got error: $error")
        }
      case Left(error) =>
        fail(s"Initial JSON conversion failed: $error")
    }
  }

  it should "handle invalid JSON" in {
    JsonUtil.safeJsonConvertingToLib("""{"invalid": "json""") match {
      case Left(_: FileError.ConversionError) => succeed
      case _ => fail("Expected ConversionError for invalid JSON")
    }
  }
}
