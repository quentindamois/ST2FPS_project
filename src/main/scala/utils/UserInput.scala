package utils
import models.Book.{createBook}
import models.User.{createExternal, createFaculty, createStudent}
import models.UserType.External
import models.{Book, LibCatalog, Transaction, TransactionType, User, UserType}
import services.LibraryService
import utils.CustomTypes.{Description, Result, UserInputResult}

import scala.io.StdIn.readLine
import utils.ErrorHandling.UserError
import utils.ErrorHandling.UserError.{convertTypeError, wrongInput, wrongLineReading}
import utils.ValidationUtil.validate
import services.ValidationService.*

import java.time.LocalDate
import java.util.UUID
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.time.LocalDate


/**
 * This package is used to allow the user to select function to run and return a function ready to run
 * */
object UserInput {
  /**
   * Call for the function to ask for a specific type of input from the user
   * @param input a tuple (String, String) corresponding to the name of the field and the type of the field
   * @param convertfunction a function that that take the name of the field and ask for value to the user
   * @return UserInputResult[(String, T)] which either contain a UserError or a tuple (String, T) which correspond to the name of the field and the value associated to it
   * */
  def InputFor[T](input: (String, String), convertFunction: String => UserInputResult[T]): UserInputResult[(String, T)] = try {
    val nameOfField = input._1
    val typeOfField = input._2
    println(s"Enter the $typeOfField for $nameOfField")
    convertFunction(input._2) match {
      case Left(error) => Left(UserError.convertTypeError(input._2, s" type of " + input._1))
      case Right(convertedValue:T) => Right(input._1, convertedValue)
    }
  } catch {
    case _ => Left(UserError.convertTypeError(input._2, s" type of " + input._1))
  }
  /**
   * A map used to select the method to ask the user for a value and convert it to the wright format
   * */
  private val mapToConvert = List(
    ("LocalDate", (nameType: String) => ask(nameType, s"The format of $nameType is YYYY-MM-DD", (inputString: String) => inputString.convertToLocalDate)),
    ("Double", (nameType: String) => ask(nameType, s"The format of $nameType is NNN.NNN with corresponding to a number.", (inputString: String) => inputString.convertToDouble)),
    ("Integer", (nameType: String) => ask(nameType, s"The format of $nameType is NNN with corresponding to a number.", (inputString: String) => inputString.convertToInteger)),
    ("Description", (nameType: String) => ask(nameType, s"The format of $nameType is a normal String if you want to enter a desription or just press enter if you don't want to enter a description.", (inputString: String) => inputString.convertToDescription)),
    ("List[String]", inputList(_)),
    ("UserType", (nameType: String) => ask(nameType, s"For $nameType:\n - For a student enter 1.\n - For a faculty member enter 2.\n - For a external member enter 2.", (inputString: String) => inputString.convertToUserType))
  ).toMap
  /**
   * Ask the user a list of String
   * 
   * @param textInput a String corresponding to the name of the filed
   * @return an UserInputResult which is an Either that either on the left a UserInput error and on the right the List[String] which is the list of Name of the author
   * */
  def inputList(textInput: String): UserInputResult[List[String]] =  try {
    val maxNumberAuthor = 8
    val listAuthor: List[String] = askElement(s"the name of the $textInput", List[String](), 1, 8)
    Right(listAuthor)
  } catch {
    case _ => {
      println("The error happen here.")
      Left(UserError.wrongInput())
    }
  }
  /**
   * Ask the user to enter an element for one element of the list of string recursively.
   * 
   * @param numberElement an Int corresponding to the number of the value currently being entered.
   * @param field a String corresponding to the name of the parameter given
   * @param listNameAuthor a List[String] containing the list of name of the author
   * @param maxDepth an Int corresponding to the maximum amount of recursion
   * @return a List[String] corresponding to the list of name of author entered by the user
   * */
  def askElement(field: String, listNameAuthor: List[String], numberElement: Int, maxDepth: Int): List[String] = {
    numberElement match {
      case currentHeight if currentHeight < maxDepth => {
        print(s"Enter $field number $numberElement (to quit enter 'quit', the number of author is limited to seven):")
        val nameAuthor = readLine()
        nameAuthor match {
          case emptyName if emptyName.isEmpty => listNameAuthor
          case quitEntered if quitEntered.equals("quit") => listNameAuthor
          case _ => askElement(field, nameAuthor :: listNameAuthor, numberElement + 1, maxDepth)
        }
      }
      case _ => listNameAuthor
    }

  }
  /**
   * Find the id of a user based on it email address.
   * @param emailResearch a String corresponding to the user's email address
   * @return the id of the user
   * */
  extension(libCat: LibCatalog) def findIdOfUser(emailResearch: String): Result[Id] = try {
    val resultId = libCat.users.values.filter((user) => user.email.equals(emailResearch)).head.id
    Right(resultId)
  } catch {
    case _ => Left("Id of User could not be found.")
  }
  end extension
  def askFor(fieldAndType: (String, String), convert: String => Any): UserInputResult[(String, Any)] = {
    val nameOfField = fieldAndType._1
    val typeOfField = fieldAndType._2
    println(s"Enter the $typeOfField for $nameOfField")
    try {
      val returnValue = convert(nameOfField)
      Right((nameOfField, returnValue))
    } catch {
      case _ => Left(UserError.convertTypeError(nameOfField, typeOfField))
    }
  }
  /**
   * This function is used to get the error message of a UserInputResultIfIt exist
   * 
   * @param res a Either UserInputResult[T] containing on the left a UserError and on the right a value T
   * @return A String corresponding to the error message if the UserInputResult or an empty String if the String is not empty
   * */
  extension [T](res: UserInputResult[T]) def getErrorMessageIfExist: String = res match {
  case Right(_) => ""
  case Left(error) => error.message
  }
  /**
   * This method is used to create to ask for all the value for all the parameter needed to run the value.
   *
   * @param ListFieldAndType a List[(String, String)] a list of tuple of two String, the first element correspond to the name of the parameter and the second element correspond to the type of the parameter.
   * @param mapInputMethod a Map[String, String => Any] a map used to get the wright input for each possible type of input
   * @param return a Result[List[(String, Any)] the Left contain String which is an error message and the Right contain a List[(String, Any)] where the first element of the tuple is the name of the parameter and the second element is the value of the parameter.
   * */
  def getInput(ListFieldAndType: List[(String, String)]): Result[List[(String, Any)]] = try {
    val defaultAskAndConvert: String => UserInputResult[String] = (nameField: String) => try {
      Right(readLine())
    } catch {
      case _ => Left(UserError.convertTypeError("String", "LocalDate"))
    }
    val processedElement = ListFieldAndType.map((fieldAndType: (String, String)) => InputFor(fieldAndType, mapToConvert.getOrElse(fieldAndType._2, defaultAskAndConvert)))
    val listErrorMessage = processedElement.map(_.getErrorMessageIfExist).filter(_.nonEmpty)
    listErrorMessage match {
      case listError if listError.length > 0 => Left(listError.reduce((a, b) => a + "\n" + b))
      case _ => Right(processedElement.map(_.getOrElse(("Error", 0))))
    }
  } catch {
    case _ => Left("Encountered an issue while taking input")
  }
  /**
   * This function is used to replace a tuple isbn and its value by the id of the corresponding book and the name indicating what the value correspond to
   *
   * @param fieldAndResearchISBN a (String, Any) a string corresponding to the name of the parameter and its value
   * @return A value Result[(String, Id)] corresponding to the name of the new parameter idBook and the value
   * */
  extension (libCat: LibCatalog) def getIdFromISBNTuple(fieldAndResearchISBN: (String, Any)): Result[(String, Id)] = try {
    val ResearchISBN = fieldAndResearchISBN._2
    val resultId = libCat.books.values.filter((book) => book.isbn.toString.equals(ResearchISBN.toString)).head.id
    Right(("idBook", resultId))
  } catch {
    case _ => Left("Id of Book could not be found from ISBN.")
  }

  /**
   * This function is used to replace a tuple email and its value by the id of the corresponding user and the name indicating what the value correspond to
   *
   * @param fieldAndEmailResearch a (String, Any) a string corresponding to the name of the parameter and its value
   * @return A value Result[(String, Id)] corresponding to the name of the new parameter idUser and the value
   * */
  extension (libCat: LibCatalog) def getIdFromEmailTuple(fieldAndEmailResearch: (String, Any)): Result[(String, Id)] = try {
    val emailResearch = fieldAndEmailResearch._2
    val resultId = libCat.users.values.filter((user) => user.email.equals(emailResearch)).head.id
    Right(("idUser", resultId))
  } catch {
    case _ => Left("Id of User could not be found from email.")
  }
  /**
   * The default function to replace a tuple, it used to not change the tuple
   *
   * @param fieldAndValue (String, T) corresponding to
   * */
  def defaulfReplaceSearch[T](fieldAndValue: (String, T)): Result[(String, T)] = try {
    Right(fieldAndValue)
  } catch {
    case _ => Left("Encountered unexpected issue.")
  }
  /**
   * This function is used to get the error message of Result[T]
   *
   * @param res a Result[T] we want to get the error message if it exist
   * @return a String corresponding to the error message
   * */
  extension [T](res: Result[T]) def getErrorMessageIfExistFromResult: String = res match {
    case Right(_) => ""
    case Left(error) => error
  }
  /**
   * This function is used to get the id of Book or User.
   *
   * @param listFieldValue a List[(String, Any)] a list of the field it corresponds to and the value associated to it
   * @param libCatalog the library's catalog we are looking to
   * */
  def getID(listFieldValue: List[(String, Any)], libCatalog: LibCatalog): Result[List[(String, Any)]] = {//TODO: a tester
    val mapToId = List(("isbn", (fieldAndISBN: (String, Any)) =>  libCatalog.getIdFromISBNTuple(fieldAndISBN)), ("email", (fieldAndEmail: (String, Any)) => libCatalog.getIdFromEmailTuple(fieldAndEmail))).toMap
    val processedList = listFieldValue.map((tupleFieldAndValue: (String, Any)) => mapToId.getOrElse(tupleFieldAndValue._1, defaulfReplaceSearch)(tupleFieldAndValue))
    val listErrorMessage = processedList.map(_.getErrorMessageIfExistFromResult).filter(_.nonEmpty)
    listErrorMessage match {
      case listError if listError.length > 0 => Left(listError.reduce((a, b) => a + "\n" + b))
      case _ => Right(processedList.map(_.getOrElse(("Error", 0))))
    }
  }
  /**
   * The function used to create a User based on the user input
   *
   * @param listParameter a List[
   * */
  def buildUser(listParameter: List[(String, Any)]): Result[User] = try {//TODO: a tester
    val typeUser = listParameter.toMap.getOrElse("userType", "error")
    typeUser match {
      case error if error == "error" => Left("No type was specified.")
      case userType if userType == UserType.External => createExternal(UUID.randomUUID().toString, listParameter.toMap.getOrElse("firstName", "absent").toString, listParameter.toMap.getOrElse("lastName", "absent").toString, listParameter.toMap.getOrElse("email",  "absent@absente.absente").toString)
      case userType if userType == UserType.Student => createStudent(UUID.randomUUID().toString, listParameter.toMap.getOrElse("firstName", "absent").toString, listParameter.toMap.getOrElse("lastName", "absent").toString, listParameter.toMap.getOrElse("email",  "absent@absente.absente").toString)
      case userType if userType == UserType.Faculty => createFaculty(UUID.randomUUID().toString, listParameter.toMap.getOrElse("firstName", "absent").toString, listParameter.toMap.getOrElse("lastName", "absent").toString, listParameter.toMap.getOrElse("email",  "absent@absente.absente").toString)
      case _ => Left("No type was specified")
    }
  } catch {
    case _ => Left("The wrong parameter were given to the User constructor.")
  }
  def wrapperResult[T](resultFunc: Result[T]): String | T = resultFunc match {
    case Left(error) => error
    case Right(success) => success
  }
  /**
   * Take list of value correponding
   * */
  def buildBook(listParameter: List[(String, Any)]): Result[Book] =  try {
    println(s"the result of listParameter : $listParameter")
    val startingCurriedBuilder: String => List[String] => String => LocalDate => String => Int => Description => String | Book = title => author => isbn => publishedDate => genre => totalCopies => description => wrapperResult(createBook(UUID.randomUUID().toString,
      title,
      author,
      isbn,
      publishedDate,
      genre,
      totalCopies,
      description
    )
    )
    val listOnlyValue = listParameter.map(_._2)
    val listParam = ListParam.fromList(listOnlyValue.reverse)
    println(s"The value of type ListParam $listParam")
    val result = listParam.foldFromHead(startingCurriedBuilder) match {
      case errorMessage: String => Left(errorMessage)
      case book:Book => Right(book)
      case _ => Left("The constructor returned an unexpected type.")
    }
    result
  }
  catch {
    case _ => Left("The constructor encountered a issue it may be due to a type mismatch.")
  }
  /**
   * Ask the user for a type that is not a String
   * 
   * @param nameField a String corresponding to the name of the field
   * @param precision a String corresponding to precision about the format of the type to enter
   * */
  def ask[T](nameField: String, precision: String, convert: String => UserInputResult[T]): UserInputResult[T] = try {
    println(precision)
    convert(readLine())
  } catch {
    case _ => Left(wrongLineReading(nameField))
  }
  /**
   * Transform a string to a LocalDate
   * 
   * @return a UserInputResult[LocalDate] which contain a LocalDate on the right and UserError on the left.
   * */
  extension (str: String) def convertToLocalDate: UserInputResult[LocalDate] = try {
    val result: LocalDate = LocalDate.parse(str)
    Right(result)
  } catch {
    case _ => Left(UserError.convertTypeError("String", "LocalDate"))
  }
  /**
   * transform a String to double
   * 
   * @return a UserInputResult[Double] which contain a Double on the right and UserError on the left.
   * */
  extension (str: String) def convertToDouble: UserInputResult[Double] = try {
    Right(str.toDouble)
  } catch {
    case _ => Left(UserError.convertTypeError("String", "Double"))
  }
  /**
   * Convert a string to integer.
   * 
   * @return a UserInputResult[Integer] which contain an Integer on the right and UserError on the left.
   * */
  extension (str: String) def convertToInteger: UserInputResult[Integer] = try {
    Right(str.toInt)
  } catch {
    case _ => Left(UserError.convertTypeError("String", "Integer"))
  }
  /**
   * Convert a string to Description
   * 
   * @return a UserInputResult[Description] which contain a Description on the right and UserError on the left.
   * */
  extension (str: String) def convertToDescription: UserInputResult[Description] = try {
    Right(Option[String](str))
  } catch {
    case _ => Left(UserError.convertTypeError("String", "Description"))
  }
  /**
   * Convert a String to a UserType
   * 
   * @return a UserInputResult[UserType] which contain an UserType on the right and UserError on the left.
   * */
  extension (str: String) def convertToUserType: UserInputResult[UserType] = try {
    str match {
      case external if 3 == external.toInt => {
        Right(External)
      }
      case student if 1 ==  student.toInt => {
        Right(UserType.Student)
      }
      case faculty if 2 == faculty.toInt => {
        Right(UserType.Faculty)
      }
      case _ => {
        println("no User Type Identified")
        Left(UserError.convertTypeError("String", "UserType"))
      }
    }
  } catch {
    case _ => {
      println("Cannot parse")
      Left(UserError.convertTypeError("String", "UserType"))
    }
  }
}

