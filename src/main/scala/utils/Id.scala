package utils

/**
 * An Opaque type used for id
 * */
opaque type Id = String
object Id:
  /**
   * The constructor of the opaque type ID
   *
   * @constructor take a string and transform it to the opaque type Id
   * @param strId a String corresponding to the value of that Id will contain.
   *
   * */
  def apply(strId: String): Id = strId
  extension (id: Id) def strId: String = id
  /**
   * Transform an Id to a String
   * 
   * @return the Id converted into a String
   * */
  extension (id: Id) def toString: String = id
  /**
   * Convert a String into an Id
   * 
   * @return a the String converted into an Id
   * */
  extension (str: String) def toId: Id = Id(str)
  
  /**
   * Convert the second element of a list of tuple into an Id.
   * 
   * @return a Result which contain on the right side list of tuple of type (String, Id) and on the Left side a String indicating that the conversion did not work.
   * */
  def convertListAnyToId(listValue: List[(String, Any)]): Result[List[(String, Id)]] = try {
    Right(listValue.map((tupleFieldValue) => (tupleFieldValue._1, Id(s"${tupleFieldValue._2}"))))
  } catch {
    case _ => Left("A value could not be converted to Id.")
  }