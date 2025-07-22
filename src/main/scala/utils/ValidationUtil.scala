package utils

import utils.CustomTypes._

object ValidationUtil {
  extension[T](conditionAndError: (T => Boolean, String)) def check(value: T): Option[String] = if conditionAndError._1(value) then None else Option(conditionAndError._2)
  def concatWithBackSlash(a: String, b: String): String = a + "\n" + b
  extension[T](value: T) def validate(conditionsAndConsequences: List[(T => Boolean, String)]): Result[T] = conditionsAndConsequences
    .map(_.check(value))
    .filter(_.isDefined)
    .map(_.get)
    .reduce(concatWithBackSlash) match {
    case fusedError if fusedError.nonEmpty => Left(fusedError)
    case _ => Right(value)
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