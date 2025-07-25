package services

import models.{Book, LibCatalog, Transaction, User}
import services.ManualControl.saveLib
import services.SearchType.{Author, Genre, Title}
import services.ValidationService.{addBookToLibCondition, addUserToLibCondition, returnBookCondition}
import upickle.core.BufferedValue.True
import utils.JsonUtil.*
import utils.CustomTypes.*
import utils.ErrorHandling.{FileError, LibraryError}
import utils.{Id, JsonUtil, ListParam}
import utils.UserInput.{buildBook, buildUser, getInput}

import scala.io.StdIn.readLine
import scala.concurrent.{Future, blocking}
import utils.ValidationUtil.validate
import utils.UserInput.getID

object ManualControl {
  /**
   * Initialise the main loop
   * @param pathFile a String corresponding to the file where the library catalog is stored
   * @return Return a Library's catalog, if the loading succeed it return the load library's catalog if did not work it return a new instance
   * */
  def initLib(pathFile: String): LibCatalog = LoadFromFile(pathFile) match {
      case Left(errorMsg) => {
        println(errorMsg)
        LibCatalog()
      }
      case Right(catalog) => catalog
    }
  def saveLib(libService: LibraryService, pathFile: String): Unit = {
    JsonUtil.saveToFile(libService.getCatalog,
      pathFile
    ) match {
      case Right(successMessage) => println(s"[OK] success : $successMessage")
      case Left(errorMessage) => println(s"[ERROR] $errorMessage")
    }
  }
  def emptyFunct[T](value: T):Result[T] = Right(value)
  def verifyNonemptnyness[T](list: List[T]): Result[List[T]] = try {
    list match {
      case listNotEmpty if listNotEmpty.length > 0 => Right(list)
      case listEmpty if listEmpty.length == 0 => Left("The list is empty")
      case _ => Left("The list has an unexpected length")
    }
  } catch {
    case _ => Left("An error occured when looking at the length of a list.")
  }
  def getFirstElement[T1, T2](result: Result[(T1, T2)]): Result[T1] = result match {
    case Right(tupleContent) => Right(tupleContent._1)
    case Left(error) => Left(error)
  }
  val mapListParameter = List(
    "search title" -> List(("title", "String")),
    "search author" -> List(("author", "List[String]")),
    "search genre" -> List(("genre", "String")),
    "filter availability" -> List[(String, String)](),
    "add user" -> List(("firstName", "String"), ("lastName", "String"), ("email", "String"), ("userType", "UserType")),
    "add book" -> List(("title", "String"), ("author", "List[String]"), ("isbn", "String"), ("publisherDate", "LocalDate"), ("genre", "String"), ("totalCopies", "Integer"), ("description", "Description")),
    "borrow Book" -> List(("email", "String"), ("isbn", "String")),
    "return Book" -> List(("email", "String"), ("isbn", "String")),
    "remove Book" -> List(("isbn", "String")),
    "remove user" -> List(("email", "String")),
    "get statistic" -> List[(String, String)](),
    "get recommendation" -> List(("email", "String")),
    "quit" -> List[(String, String)]()
  ).toMap

  def convertListAnyToId(listValue: List[(String, Any)]): Result[List[(String, Id)]] = try {
    Right(listValue.map((tupleFieldValue) => (tupleFieldValue._1, Id(s"${tupleFieldValue._2}"))))
  } catch {
    case _ => Left("A value could not be converted to Id.")
  }
  val mapListChainFunction = List(
    "search title" -> ((listField:List[(String, String)], libraryService: LibraryService) => getInput(listField).flatMap((listFieldValue: List[(String, Any)]) => executeFonctionWithList(listFieldValue, (query: String) =>  wrapperEither(libraryService.searchBooks(query, Title), (error:LibraryError) => error.message)))),
    "search author" -> ((listField:List[(String, String)], libraryService: LibraryService) => getInput(listField).flatMap((listFieldValue: List[(String, Any)]) => executeFonctionWithList(listFieldValue, (query: String) =>  wrapperEither(libraryService.searchBooks(query, Author), (error:LibraryError) => error.message)))),
    "search genre" -> ((listField:List[(String, String)], libraryService: LibraryService) => getInput(listField).flatMap((listFieldValue: List[(String, Any)]) => executeFonctionWithList(listFieldValue, (query: String) =>  wrapperEither(libraryService.searchBooks(query, Genre), (error:LibraryError) => error.message)))),
    "filter availability" -> ((listField:List[(String, String)], libraryService: LibraryService) => emptyFunct(libraryService.getCatalog.availableBooks)),
    "add user" -> ((listField:List[(String, String)], libraryService: LibraryService) => getInput(listField).flatMap(buildUser).flatMap((user: User) => (user, libraryService).validate(addUserToLibCondition)).flatMap((userAndLibService: (User, LibraryService)) => userAndLibService._2.addUser(userAndLibService._1))),
    "add book" -> ((listField:List[(String, String)], libraryService: LibraryService) => getInput(listField).flatMap(buildBook).flatMap((book: Book) => (book, libraryService).validate(addBookToLibCondition)).flatMap((userAndLibService: (Book, LibraryService)) => userAndLibService._2.addBook(userAndLibService._1))),
    "borrow Book" -> ((listField:List[(String, String)], libraryService: LibraryService) => getInput(listField).flatMap((listValue: List[(String, Any)]) => getID(listValue, libraryService.getCatalog)).flatMap((listValue: List[(String, Any)]) => executeFonctionWithList(listValue, (idUser: Id) => (idBook: Id) => wrapperEither(libraryService.borrowBook(idUser, idBook), (error: LibraryError) => error.message)))),
    "return Book" -> ((listField:List[(String, String)], libraryService: LibraryService) => getInput(listField).flatMap((listValue: List[(String, Any)]) => getID(listValue, libraryService.getCatalog)).flatMap(convertListAnyToId).flatMap((listValue: List[(String, Id)]) => (listValue, libraryService).validate(returnBookCondition)).flatMap((listValue: (List[(String, Any)], LibraryService)) => executeFonctionWithList(listValue._1, (idUser: Id) => (idBook: Id) => wrapperEither(libraryService.returnBook(idUser, idBook), (error: LibraryError) => error.message)))),
    "remove Book" -> ((listField:List[(String, String)], libraryService: LibraryService) => getInput(listField).flatMap((listValue: List[(String, Any)]) => getID(listValue, libraryService.getCatalog)).flatMap((listValue: List[(String, Any)]) => executeFonctionWithList(listValue, (idBook: Id) => wrapperEither(libraryService.removeBook(idBook), (error:LibraryError) => error.message)))),
    "remove user" -> ((listField:List[(String, String)], libraryService: LibraryService) => getInput(listField).flatMap((listValue: List[(String, Any)]) => getID(listValue, libraryService.getCatalog)).flatMap((listValue: List[(String, Any)]) => executeFonctionWithList(listValue, (idUser: Id) => wrapperEither(libraryService.removeBook(idUser), (error:LibraryError) => error.message)))),
    "get statistic" -> ((listField:List[(String, String)], libraryService: LibraryService) => libraryService.displayStatistic()),
    "get recommendation" -> ((listField:List[(String, String)], libraryService: LibraryService) => {
      val recommandationService = RecommendationService(libraryService)
      getInput(listField)
        .flatMap((listValue: List[(String, Any)]) => getID(listValue, libraryService.getCatalog))
        .flatMap(convertListAnyToId)
        .flatMap(
          (listValueForParameter: List[(String, Id)]) => executeFonctionWithList(listValueForParameter, (idUser: Id) => wrapperEither(verifyNonemptnyness(recommandationService.recommendBooksByGenre(idUser, 3)), (error: String) => error)))
        })
  ).toMap
  val mapResultChain = List(
    "search title" -> ((resultFunction: Result[List[Book]]) => executedFunctionWithDisplay(resultFunction, (foundBooks:List[Book]) => foundBooks.map(_.title).mkString(", "))),
    "search author" -> ((resultFunction: Result[List[Book]]) => executedFunctionWithDisplay(resultFunction, (foundBooks:List[Book]) => foundBooks.map(_.title).mkString(", "))),
    "search genre" -> ((resultFunction: Result[List[Book]]) => executedFunctionWithDisplay(resultFunction, (foundBooks:List[Book]) => foundBooks.map(_.title).mkString(", "))),
    "filter availability" -> ((resultFunction: Result[List[Book]]) => executedFunctionWithDisplay(resultFunction, (foundBooks:List[Book]) => foundBooks.map(_.title).mkString(", "), (str: String) => str)),
    "add user" -> ((resultFunction: LibraryResult[LibraryService], libraryService: LibraryService) => executedFunctionUpdateLibraryService(resultFunction  , (libraryService: LibraryService) => "user successfully added" , (error:LibraryError) => error.message, libraryService)),
    "add book" -> ((resultFunction: LibraryResult[LibraryService], libraryService: LibraryService) => executedFunctionUpdateLibraryService(resultFunction, (libraryService: LibraryService) => "book successfully added" , (error:LibraryError) => error.message, libraryService)),
    "borrow Book" -> ((resultFunction: Result[(LibraryService, Transaction)], libraryService: LibraryService) => executedFunctionUpdateLibraryService(getFirstElement(resultFunction), (result) => "Borrowing saved", (error: String) => error, libraryService)),
    "return Book" -> ((resultFunction: Result[(LibraryService, Transaction)], libraryService: LibraryService) => executedFunctionUpdateLibraryService(getFirstElement(resultFunction) , (result) => "Return saved", (error: String) => error, libraryService)),
    "remove Book" -> ((resultFunction: Result[LibraryService], libraryService: LibraryService) => executedFunctionUpdateLibraryService(resultFunction, (result) => "Book removed", (str:String) => str, libraryService)),
    "remove user" -> ((resultFunction: Result[LibraryService], libraryService: LibraryService) => executedFunctionUpdateLibraryService(resultFunction, (result) => "User removed", (str:String) => str, libraryService)),
    "get recommendation" -> ((result: Result[List[Book]]) => executedFunctionWithDisplay(result, (foundBooks:List[Book]) => foundBooks.map(_.title).mkString(", ")))
  ).toMap
  def executeFonctionWithList[S, T](listAllValue: List[(String, Any)], curriedFunction: S => Any): Result[T] = try {
    val listOnlyValue = listAllValue.map(_._2)
    val listParam = ListParam.fromList(listOnlyValue.reverse)
    val result = listParam.foldFromHead(curriedFunction) match {
      case errorMessage: String => Left(errorMessage)
      case success: T => Right(success)
      case _ => Left("The constructor returned an unexpected type.")
    }
    result
  } catch {
    case _ => Left("The wrong parameter where given to the function")
  }
  def wrapperEither[F,S](resultEither: Either[F, S], extractErrorMessage: F => String): S | String = resultEither match {
    case Left(error) => extractErrorMessage(error)
    case Right(success) => success
  }
  def executedFunctionWithDisplay[E, T](executedFunction: Either[E , T], getInfoSuccess: T => String, getInfoFailure: E => String = (str:String) => str, messageSuccess: String = "result", messageFailure: String = "error message"): Unit ={
    executedFunction match {
      case Right(success) => {
        println(s"[OK] $messageSuccess : ${getInfoSuccess(success)}")
      }
      case Left(error) => {
        println(s"[ERROR] $messageFailure : ${getInfoFailure(error)}")
      }
    }
  }
  def executedFunctionUpdateLibraryService[E](executedFunction: Either[E, LibraryService], getInfoSuccess: LibraryService => String, getInfoFailure: E => String, libService: LibraryService, messageSuccess: String = "result", messageFailure: String = "error message"): LibraryService = {
    executedFunction match {
      case Right(success) => {
        println(s"[OK] $messageSuccess : ${getInfoSuccess(success)}")
        success
      }
      case Left(error) => {
        println(s"[ERROR] $messageFailure : ${error}")
        libService
      }
    }
  }
  def mainLoop(): Unit = try {
    var loop: Boolean = true
    var libCatalog = initLib("./data/catalog.json")
    var libraryService = LibraryService(libCatalog)
    val informationCommand: String = "1-search title\n2-search author\n3-search genre\n4-filter availability\n5-add user\n6-add book\n7-borrow Book\n8-return Book\n9-remove Book\n10-remove user\n11-get statistic\n12-get recommendation\n13-quit"
    println(informationCommand)
    while (loop) {
      print(">")
      val valueSelected = readLine()
      var selectedFunction = mapListParameter.getOrElse(valueSelected, informationCommand)
      if valueSelected == "quit" then {
        loop = false
        selectedFunction = informationCommand
        saveLib(libraryService, "./data/catalog.json")
      }
      selectedFunction match {
        case userCommand if userCommand==informationCommand => {
          if valueSelected != "quit" then println(informationCommand)
        }
        case validUserCommand:List[(String, String)] => {
          val fonction = mapListChainFunction.getOrElse(valueSelected, (field: List[(String, String)], library: LibraryService) => "")
          val receptionFunction = mapResultChain.getOrElse(valueSelected, (field: Result[Any]) => "")
          val resultFunction = fonction(validUserCommand, libraryService)
          valueSelected match {
            case result if (List("search title", "search author", "search genre", "filter availability", "get recommendation")).contains(result) => {
              resultFunction match
              {
                case returnListBook:Result[List[Book]] => {
                  receptionFunction match {
                    case takeListBook:(Result[List[Book]] => Unit) => takeListBook(returnListBook)
                    case _ => println("Incorrect reception of Result[List[Book]].")
                  }
                }
                case _ => println("Incorrect casting to Result[List[Book]] the result of the function")
              }
            }
            case result if List("add user", "add book", "remove Book", "remove user").contains(result) => resultFunction  match {
                case returnLibCatalog: Result[LibraryService] => receptionFunction match {
                  case takeOnlyLibCatalog: ((Result[LibraryService], LibraryService) => LibraryService) => libraryService = takeOnlyLibCatalog(returnLibCatalog, libraryService)
                  case _ => println("Incorrect reception of the Result[LibraryService].")
                }
                case _ => println("Incorrect casting to Result[LibraryService] the result of the function")
              }
            case result if List("return Book", "borrow Book").contains(result) => resultFunction match {
              case returnLibServiceTransaction: Result[(LibraryService, Transaction)] => receptionFunction match {
                case takeOnlyLibServiceTransaction: ((Result[(LibraryService, Transaction)], LibraryService) => LibraryService) => libraryService = takeOnlyLibServiceTransaction(returnLibServiceTransaction, libraryService)
                case _ => println("Incorrect reception of the Result[(LibraryService, Transaction)].")
              }
              case _ => println("Incorrect casting to Result[(LibraryService, Transaction)] the result of the function")
            }
            case result if result == "get statistic" => resultFunction match {
              case onlyDisplay:(Unit) => {
                onlyDisplay
              }
              case _ => println("You should not see this.")
            }
            case _ => println("An unexpected type is returneed by the function")
          }
        }
        case _ => println(informationCommand)
      }
    }
  }catch {
    case _ => println("Unexpected closure.")
  }
} 
