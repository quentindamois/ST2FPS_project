package utils

import utils.CustomTypes._

/**
 * General object used to verify if an object satisfy a list of condition
 * */
object ValidationUtil {
  /**
   * verify if the function T => Boolean return true if we give the value T
   *
   * @return an Option[String] which contains a String if the function return false when we give the value T if function T => Boolean return false then we return None
   * */
   extension[T](conditionAndError: (T => Boolean, String)) private def check(value: T): Option[String] = if conditionAndError._1(value) then None else Option(conditionAndError._2)
  /**
   * wrapper function use to concatenate two sting and have a backslash between the two String
   *
   * @return a concatenated String
   * */
  def concatWithBackSlash(a: String, b: String): String = a + "\n" + b
  /**
   * apply a list of condition to a value, the condition correspond the first element of the tuple.
   *
   * @return If at least one condition is not fulfilled we return a string idicating every condition that is not fullfilled on the left of Result[T]. If every condition is fullfilled we return the value T on the right side of Result[T]
   * */
  extension[T](value: T) def validate(conditionsAndConsequences: List[(T => Boolean, String)]): Result[T] = conditionsAndConsequences
    .map(_.check(value))
    .filter(_.isDefined)
    .map(_.get) match {
    case fusedError if fusedError.nonEmpty => Left(fusedError.reduce(concatWithBackSlash))
    case _ =>       Right(value)
  }
  extension [T](value: T) def validateString(conditionsAndConsequences: List[(T => Boolean, String)]): T | String = conditionsAndConsequences
    .map(_.check(value))
    .filter(_.isDefined)
    .map(_.get)
    .reduce(concatWithBackSlash) match {
    case fusedError if fusedError.nonEmpty => fusedError
    case _ => value
  }
  extension [T](value: T) def getAllError(conditionsAndConsequences: List[(T => Boolean, String)]): Result[Boolean] = conditionsAndConsequences
    .map(_.check(value))
    .filter(_.isDefined)
    .map(_.get)
    .reduce(concatWithBackSlash) match {
    case fusedError if fusedError.nonEmpty => Left(fusedError)
    case _ => Right(true)
  }
}