package utils
import models.LibCatalog
import utils.FileError.{ConversionError, PathError, ReadingError}
import upickle.default.*

import java.time.{LocalDate, LocalDateTime}
import java.io.{File, FileNotFoundException, FileWriter}
import java.time.format.DateTimeFormatter
import scala.util.Try

// all the possible error
enum FileError():
  case PathError, AuthorisationError, ReadingError, ConversionError
  
  

//The type for the type of the output for each function
type fileOperationInnerResult[A] = Either[FileError, A]
type fileOperationOutput[A] = Either[String, A]

object JsonUtil {
  /** 
    * This function save a library in the json format
    * 
    * @param lib the LibCatalog that is going to be saved in a file
    * @param pathToFile a String corresponding the path to the file where we are saving the library
    * @return an Either[String, String] that contain on the left a message about the error encountered and on the right a message indicating success
    */
  def saveToFile(lib: LibCatalog, pathToFile: String): fileOperationOutput[String] = {
    val writeToFile: File => fileOperationInnerResult[String] = {
      (file: File) => safeLibConvertingToJson(lib).flatMap((jsonContent) => safeFileWriting(file, jsonContent))
    }
    safeFileOpening(pathToFile).flatMap(writeToFile) match {
      case Left(errorOperation) if  errorOperation == ConversionError => Left("the library could not be converted in Json.")
      case Left(errorOperation) if errorOperation == PathError => Left(s"$pathToFile does not exist")
      case Left(errorOperation) if errorOperation == FileError.AuthorisationError => Left(s"$pathToFile cannot not be edited.")
      case Right(result) => Right(result)
      case _ => Left("There is an issue when trying to save the library.")
    }
  }
  /**
    * This function load a library in the json format
    * 
    * @param pathToFile a String corresponding the path to the file from where we are loading the library
    * @return an Either[String, libCatalog] hat contain on the left a message about the error encountered and on the right the LibCatalog successfully loaded
    **/
  def LoadFromFile(pathToFile: String): fileOperationOutput[LibCatalog] = {
    safeFileOpening(pathToFile).flatMap(safeFileReading).flatMap(safeJsonConvertingToLib) match {
      case Left(errorOperation) if  errorOperation == ConversionError => Left("the library could not be converted in Json.")
      case Left(errorOperation) if errorOperation == PathError => Left(s"$pathToFile does not exist")
      case Left(errorOperation) if errorOperation == FileError.ReadingError => Left(s"$pathToFile cannot not be read.")
      case Right(result) => Right(result)
      case _ => Left("There is an issue when trying to load the library.")
    }
  }
  /**
    * This function open a file with safety measure.
    * 
    * @param pathToFile a String corresponding the path to the file we are opening
    * @return an Either[FileError, File] that contain on the left an error if opening the file failed and on the right the File successfully opened.
    */
  def safeFileOpening(pathToFile: String): fileOperationInnerResult[File] = {
    try {
      val jsonFile = new File(pathToFile)
      Right(jsonFile)
    } catch {
      case _ => Left(FileError.PathError)
    }
  }
  /**
    * This function write to a file with safety measure.
    * 
    * @param file a File object that correspond to the file the json is going to be written to.
    * @param JsonContent a String corresponding the json we are saving in the file.
    * @return an Either that contain on the left an error if writing in the file failed and a String containing a success message.
    */
  def safeFileWriting(file: File, jsonContent: String): fileOperationInnerResult[String] = {
    try {
      val fileWriter = new FileWriter(file)
      fileWriter.write(jsonContent)
      fileWriter.close()
      Right("Success: File written with success.")
    } catch {
      case _ => Left(FileError.AuthorisationError)
    }
  }

  /**
    * This function read a file with safety measure.
    *
    * @param fileName a File object that correspond to the file we are getting the content
    * @return an Either[FileError, String] that contain on the left an error if opening the file failed and on the right the content of the file.
    */
  def safeFileReading(fileName: File): fileOperationInnerResult[String] = {
    try {
      val FileReader = scala.io.Source.fromFile(fileName)
      val content = FileReader.getLines().mkString
      FileReader.close()
      Right(content)
    } catch {
      case _ => Left(FileError.ReadingError)
    }
  }
  /**
    * This function serialize a LibCatalog object
    *
    * @param lib a LibCatalog object that we are serializing
    * @return an Either[FileError, String] that contain on the left an error if opening serializing the object failed and on the right the String successfully serialized           
    */
  def safeLibConvertingToJson(lib: LibCatalog): fileOperationInnerResult[String] = {
    try {
      val jsonContent = upickle.default.write(lib)
      Right(jsonContent)
    } catch {
      case _ => Left(FileError.ConversionError)
    }
  }

  /**
    * This function deserialize to a LibCatalog object
    *
    * @param jsonContent a String we are deserializing
    * @return an Either[FileError, LibCatalog] that contain on the left an error if opening serializing the object failed and on the right the object successfully deserialized           
    */
  def safeJsonConvertingToLib(jsonContent: String): fileOperationInnerResult[LibCatalog] = {
    try {
      val lib = upickle.default.read[LibCatalog](jsonContent)
      Right(lib)
    } catch {
      case _ => Left(FileError.ConversionError)
    }
  }
  //based on https://github.com/com-lihaoyi/upickle/issues/260


  /**
   * This is a custom Picklers for the type LocalDate
   * */
  implicit val ReaderWriterLocalDate: ReadWriter[LocalDate] = upickle.default.readwriter[String].bimap(
    locDate => locDate.format(DateTimeFormatter.ISO_LOCAL_DATE),
    strLocalDate => LocalDate.parse(strLocalDate)
  )

  /**
    * This is the custom picklers for the type Local Date Time
    */
  implicit val ReaderWriterLocalDateTime: ReadWriter[LocalDateTime] = upickle.default.readwriter[String].bimap(
    locDateTime => locDateTime.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME),
    strLocalDate => LocalDateTime.parse(strLocalDate)
  )

}
